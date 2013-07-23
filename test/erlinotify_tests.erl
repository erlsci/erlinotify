-module(erlinotify_tests).
-include_lib("eunit/include/eunit.hrl").
-spec test () -> term().

setup() ->
    %% Suppress so we can have some peace.
    error_logger:tty(false),
    Path = "/tmp/test/",
    ok = filelib:ensure_dir(Path),
    application:start(ets_manager),
    application:start(erlinotify),
    Path.

cleanup(_Arg) ->
    application:stop(erlinotify),
    application:stop(ets_manager),
    error_logger:tty(true).

-spec basic_test_ () -> none().
basic_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Path) ->
              Ref = self(),
              erlinotify:watch(Path,
                               fun(Var) -> Ref ! ?_assertMatch({Path,file,_Event,0,"monkey"}, Var) end ),
              ok = file:write_file(Path++"monkey", "testing123", [write]),
              receive
                  Assert -> Assert
              after 1000 -> ?_assertMatch(timeout, error)
              end
        end
    }.
