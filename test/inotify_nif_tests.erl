-module(inotify_nif_tests).

-include_lib("eunit/include/eunit.hrl").

-spec basic_test () -> none().
basic_test() ->
    {ok, Ref} = inotify_nif:start(),
    ?assertEqual({ok,1}, inotify_nif:add_watch(Ref, "/tmp/")),
    ?assertEqual(ok, inotify_nif:remove_watch(Ref, 1)),
    ?assertEqual(ok, inotify_nif:stop(Ref)).

-spec thread_test () -> none().
thread_test() ->
    Path = "/tmp/test/",
    Filename = "monkey",
    ok = filelib:ensure_dir(Path),
    _P = spawn(spawn_watcher(Path, Filename)),
    ok = file:write_file(Path++Filename, "testing123", [write]).

spawn_watcher(Path, Filename) ->
    fun() ->
        {ok, Ref} = inotify_nif:start(),
        ?assertEqual({ok,1}, inotify_nif:add_watch(Ref, Path)),
        receive
            {inotify_event, 1, file, _Event, 0, File} ->
                ?assertEqual(Filename, File)
        end,
        ?assertEqual(ok, inotify_nif:remove_watch(Ref, 1)),
        ?assertEqual(ok, inotify_nif:stop(Ref))
    end.

-spec test () -> term().
