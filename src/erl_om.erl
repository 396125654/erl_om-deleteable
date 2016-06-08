%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc erl_om.

-module(erl_om).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the erl_om server.
start() ->
    erl_om_deps:ensure(),
    ensure_started(crypto),
    application:start(erl_om).


%% @spec stop() -> ok
%% @doc Stop the erl_om server.
stop() ->
    application:stop(erl_om).
