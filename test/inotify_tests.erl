-module(inotify_tests).
-include_lib("eunit/include/eunit.hrl").

-spec test () -> term().

setup() ->
    %% Suppress so we can have some peace.
    error_logger:tty(false),
    Path = "/tmp/test/",
    ok = filelib:ensure_dir(Path),
    application:ensure_all_started(inotify),
    Path.

cleanup(_Arg) ->
    application:stop(inotify),
    application:stop(ets_manager),
    error_logger:tty(true).

-spec basic_test_ () -> none().
basic_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Path) ->
              Ref = self(),
              inotify:watch(Path,
                               fun(Var) ->
                                Ref ! ?_assertMatch({Path,file,_Event,0,"monkey"}, Var) end),
              ok = file:write_file(Path++"monkey", "testing123", [write]),
              receive
                  Assert -> Assert
              after 1000 -> ?_assertMatch(timeout, error)
              end
        end
    }.
