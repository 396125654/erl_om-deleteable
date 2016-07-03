-module(erl_om_handler_esp).
-export([handle/4]).
-define(MAX_RECV_BODY, (1024*1024)).
-record(state, {pid, ref, response, mode=binary}).

handle(Method, Req, DocRoot, Options) ->
    Path = Req:get(path),
    Components = filename:split(Path),
    Drop = proplists:get_value(drop, Options, 0),
    Root = proplists:get_value(root, Options, DocRoot),
    MaxRecvBody = proplists:get_value(max_recv_body, Options, ?MAX_RECV_BODY),
    [Filename|RestArgs] = lists:nthtail(Drop, Components),
    ScriptFilename = filename:join([Root, Filename]),
    case file:read_file(ScriptFilename) of
        {ok, ScriptString} ->
            try
                Data = Req:recv_body(MaxRecvBody),
                QueryString = Req:parse_qs(),
                Bindings =
                    lists:foldl(
                      fun({Name, Value}, Binding) ->
                              erl_eval:add_binding(Name, Value, Binding)
                      end,
                      erl_eval:new_bindings(),
                      [{'Args', RestArgs},
                       {'Method', Method},
                       {'Data', Data},
                       {'QueryString', QueryString}
                      ]),
                Exprs = parse(binary_to_list(ScriptString)),
                Self = self(),
                {Pid, Ref} = spawn_monitor(fun() ->
                                                   erlang:group_leader(Self, self()),
                                                   try erl_eval:exprs(Exprs, Bindings)
                                                   catch
                                                       C1:E1 ->
                                                           io:format("internal error ~p:~p~n",[C1, E1])
                                                   end
                                           end),
                Headers = [{"Content-Type", "text/plain"}],
                Response = Req:respond({200, Headers, chunked}),
                %% ignore any exception after responding to the
                %% client, there is no way to take back what has been
                %% sent.
                (catch forward_output(#state{pid = Pid, ref = Ref, response = Response})),
                %% close the chunk
                (catch Response:write_chunk([]))
            catch
                Class:Error ->
                    ResponseString = io_lib:format("parse error ~p:~p ~p~n", [Class, Error, erlang:get_stacktrace()]),
                    Req:respond({500, [{"Content-Type", "text/plain"}], ResponseString})
            end;
        _FileError ->
            ResponseString = io_lib:format("file not found: ~s~n",[ScriptFilename]),
            Req:respond({404, [{"Content-Type", "text/plain"}], ResponseString})
    end.


parse(Str) ->
    prelude() ++ parse_rel (Str) ++ post_action().

parse_rel(Str) ->
    {ok, Tokens, _} = erl_scan:string(Str),
    {ok, Exprs} = erl_parse:parse_exprs(Tokens),
    expect_transform(Exprs).

expect_transform(Exprs) ->
    lists:flatmap(fun(X) -> expect_transform_expr(X) end,  Exprs).

expect_transform_expr({call,_LINE1,{atom,_LINE2,echo},[{atom,_LINE3,on}]}) ->
    erlang:put(echo, true),
    [];
expect_transform_expr({call,_LINE1,{atom,_LINE2,echo},[{atom,_LINE3,off}]}) ->
    erlang:put(echo, false),
    [];
expect_transform_expr(Expr) ->
    Line = element(2, Expr),
    ExprStr = binary_to_list(
                iolist_to_binary(
                  [ integer_to_list(Line), "> ", erl_pp:expr(Expr)])),
    Var = list_to_atom(
            binary_to_list(
              iolist_to_binary(
                [ "Out", integer_to_list(Line) ]))),
    %% for debugging: io:format("~p: ~s -> ~p~n",[Line, ExprStr, Expr]),
    case erlang:get(echo) of
        true ->
            [{call,Line,                                % io:format("~s~n",[<EXPR_STR>])
              {remote,Line,{atom,Line,io},{atom,Line,format}},
              [{string, Line, "~s~n"},
               {cons,Line,{string,Line,ExprStr},{nil,Line}}]},
             {match, Line, {var, 1, Var},               % Out<LINE> = <Result>
              Expr},
             {call,Line,                                % io:format("~p~n", [Out<LINE>])
              {remote,Line,{atom,Line,io},{atom,Line,format}},
              [{string, Line, "~p~n"},
               {cons,Line,{var, 1, Var},{nil,Line}}]}
            ];
        _ ->
            [Expr]
    end.



set_default_cookie() ->
    case os:getenv("ERLANG_COOKIE") of
        Cookie when is_list(Cookie) ->
            erlang:set_cookie(node(), list_to_atom(Cookie));
        _ ->
            ok
    end.


prelude() ->
    [].

post_action() ->
    Line = 0,
    [{call,Line,                                % io:format("~s~n",[<EXPR_STR>])
      {remote,Line,{atom,Line,erlang},{atom,Line,send}},
      [{tuple, Line, [{atom,Line,return_port},
                      {atom,Line,node()}]},
       {atom, Line, ok}]}].


forward_output(State) ->
    loop(State).

loop(#state{ref = Ref} = State) ->
    receive
        {io_request, From, ReplyAs, Request} ->
            case request(Request,State) of
                {Tag, Reply, NewState} when Tag =:= ok; Tag =:= error ->
                    reply(From, ReplyAs, Reply),
                    loop(NewState);
                {stop, Reply, _NewState} ->
                    reply(From, ReplyAs, Reply),
                    State
            end;
        {'DOWN', Ref, _, _, _} ->
            State;
        Unknwon ->
            io:format("receive unknown message ~p~n",[Unknwon]),
            State
    end.
reply(From, ReplyAs, Reply) ->
    From ! {io_reply, ReplyAs, Reply}.

request({put_chars, Encoding, Chars}, State) ->
    put_chars(unicode:characters_to_list(Chars,Encoding),State);
request({put_chars, Encoding, Module, Function, Args}, State) ->
    try
        request({put_chars, Encoding, apply(Module, Function, Args)}, State)
    catch
        _:_ ->
            {error, {error,Function}, State}
    end;
request({get_until, _Encoding, _Prompt, _M, _F, _As}, State) ->
    {error, {error, writeonly}, State};
request({get_chars, _Encoding, _Prompt, _N}, State) ->
    {error, {error, writeonly}, State};
request({get_line, _Encoding, _Prompt}, State) ->
    {error, {error, writeonly}, State};
request({get_geometry,_}, State) ->
    {error, {error, writeonly}, State};
request({setopts, Opts}, State) ->
    setopts(Opts, State);
request(getopts, State) ->
    getopts(State);
request({requests, Reqs}, State) ->
    multi_request(Reqs, {ok, ok, State});
request({put_chars,Chars}, State) ->
    request({put_chars,latin1,Chars}, State);
request({put_chars,M,F,As}, State) ->
    request({put_chars,latin1,M,F,As}, State);
request({get_chars,Prompt,N}, State) ->
    request({get_chars,latin1,Prompt,N}, State);
request({get_line,Prompt}, State) ->
    request({get_line,latin1,Prompt}, State);
request({get_until, Prompt,M,F,As}, State) ->
    request({get_until,latin1,Prompt,M,F,As}, State);
request(_Other, State) ->
    {error, {error, request}, State}.

multi_request([R|Rs], {ok, _Res, State}) ->
    multi_request(Rs, request(R, State));
multi_request([_|_], Error) ->
    Error;
multi_request([], Result) ->
    Result.
setopts(Opts0,State) ->
    Opts = proplists:unfold(
             proplists:substitute_negations(
               [{list,binary}],
               Opts0)),
    case check_valid_opts(Opts) of
        true ->
	        case proplists:get_value(binary, Opts) of
                true ->
                    {ok,ok,State#state{mode=binary}};
                false ->
                    {ok,ok,State#state{mode=binary}};
                _ ->
                    {ok,ok,State}
            end;
        false ->
            {error,{error,enotsup},State}
    end.
check_valid_opts([]) ->
    true;
check_valid_opts([{binary,Bool}|T]) when is_boolean(Bool) ->
    check_valid_opts(T);
check_valid_opts(_) ->
    false.

getopts(#state{mode=M} = S) ->
    {ok,[{binary, case M of
                      binary ->
                          true;
                      _ ->
                          false
                  end}],S}.

put_chars(Chars, #state{response = Response} = State) ->
    Reply = Response:write_chunk(Chars),
    {ok, Reply, State}.

check(unicode, List) ->
    List;
check(latin1, List) ->
    try
	[ throw(not_unicode) || X <- List,
				X > 255 ],
	List
    catch
	throw:_ ->
	    {error,{cannot_convert, unicode, latin1}}
    end.

until_newline([],eof,_MyStopCharacter) ->
    {done,eof,[]};
until_newline(ThisFar,eof,_MyStopCharacter) ->
    {done,ThisFar,[]};
until_newline(ThisFar,CharList,MyStopCharacter) ->
    case
        lists:splitwith(fun(X) -> X =/= MyStopCharacter end,  CharList)
    of
	{L,[]} ->
            {more,ThisFar++L};
	{L2,[MyStopCharacter|Rest]} ->
	    {done,ThisFar++L2++[MyStopCharacter],Rest}
    end.
