%% @author Mochi Media <dev@mochimedia.com>
%% @copyright erl_om Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the erl_om application.

-module(erl_om_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for erl_om.
start(_Type, _StartArgs) ->
    erl_om_deps:ensure(),
    erl_om_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for erl_om.
stop(_State) ->
    ok.
