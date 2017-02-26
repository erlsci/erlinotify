-module(inotify_nif).

-export([start/0,
         stop/1,
         add_watch/2,
         remove_watch/2]).

-on_load(init/0).

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------
-define(nif_stub, nif_stub_error(?LINE)).


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% @doc Start inotify by creating a file descriptor and spawing a
%% thread that will poll the file descriptor for any events.
-spec start () -> {ok, term()} | {error, term()}.
start() ->
    ?nif_stub.

%% @doc Stop inotify by closing the file descriptor to it and
%% implicitly killing the spawned thread?
-spec stop (term()) -> {ok, term()} | {error, term()}.
stop(_Ref) ->
    ?nif_stub.

%% @doc Add a directoy to watch for changes returning an integer
%% reference to it.
-spec add_watch (term(), file:name()) -> {ok, integer()} | {error, term()}.
add_watch(_Ref, _Dirname) ->
    ?nif_stub.

%% @doc Remove a watching directory by supplying the integer reference.
-spec remove_watch (term(), integer()) -> ok | {error, term()}.
remove_watch(_Ref, _Wd) ->
    ?nif_stub.

%% ------------------------------------------------------------------
%% Utility Function Definitions
%% ------------------------------------------------------------------

%% @spec nif_stub_error(Linenumber) -> ok
%%       Linenumber = int()
%%
%% @doc Log failed linked method call.
nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

%% @spec init() -> ok | {error, {atom(), string()}}
%%
%% @doc Load the c library
init() ->
    PrivDir = case code:priv_dir(?MODULE) of
                  {error, bad_name} ->
                      EbinDir = filename:dirname(code:which(?MODULE)),
                      AppPath = filename:dirname(EbinDir),
                      filename:join(AppPath, "priv");
                  Path ->
                      Path
              end,
    erlang:load_nif(filename:join(PrivDir, ?MODULE), 0).
