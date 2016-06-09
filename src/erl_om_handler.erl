%%%-------------------------------------------------------------------
%%% @author WangChunye <wcy123@gmail.com>
%%% @copyright (C) 2016, WangChunye
%%% @doc
%%%   handler manager
%%% @end
%%% Created :  8 Jun 2016 by WangChunye <>
%%%-------------------------------------------------------------------
-module(erl_om_handler).
-behaviour(gen_server).

%% API
-export([start_link/0, add_handler/2, get_handlers/0, get_handler/1, remove_handler/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% constants
table() -> ?MODULE.

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add_handler(Regexp, Handler) ->
    gen_server:call(?SERVER, {add_handler, Regexp, Handler,[]}).
remove_handler(Regexp) ->
    gen_server:call(?SERVER, {remove_handler, Regexp}).

get_handlers() ->
    ets:tab2list(table()).

get_handler(Path) ->
    ets:foldl(
      fun({_Regexp, Handler, MP}, Acc) ->
              case Acc of
                  undefined ->
                      %% this line is copied from http://alexmarandon.com/articles/mochiweb_tutorial/#a-simple-url-dispatcher
                      Match = re:run(Path, MP, [global, {capture, all_but_first, list}]),
                      case Match of
                          {match, [_]} ->
                              Handler;
                          _ ->
                              Acc
                      end;
                  _ ->
                      Acc
              end
      end, undefined, table()).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    Table = table(),
    Handlers = application:get_env(erl_om, handlers, []),
    Table = ets:new(Table, [set, protected, named_table, {read_concurrency,true}]),
    lists:foreach(fun({Regexp, Handler})->
                          ok = do_add_handler(Regexp, Handler)
                  end, Handlers),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({add_handler, Regexp, Handler}, _From, State) ->
    {reply, do_add_handler(Regexp,Handler), State};
handle_call({remove_handler, Regexp}, _From, State) ->
    {reply, do_remove_handler(Regexp), State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


do_add_handler(Regexp, Handler) ->
    {ok, MP}  =  re:compile(Regexp),
    try ets:insert(table(),{Regexp, Handler, MP}) of
        true ->
            ok;
        Else ->
            {error, Else}
    catch
        C:E -> {error, {C,E}}
    end.
do_remove_handler(Regexp) ->
    try ets:delete(table(), Regexp) of
        true ->
            ok;
        Error ->
            {error, Error}
    catch
        C:E ->
            {error, {C, E}}
    end.
