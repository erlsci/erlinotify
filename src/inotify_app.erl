-module(inotify_app).

-behaviour(application).

%% Application callbacks
-export([start/2,
         stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

-spec start(normal | {takeover,node()} | {failover,node()}, term())
  -> {error, _} | {ok, pid()}.
start(_StartType, _StartArgs) ->
    inotify_sup:start_link().

-spec stop(term()) -> ok.
stop(_State) ->
    ok.
