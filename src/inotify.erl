-module(inotify).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

-define(log_error(T),
        error_logger:error_report(
          [process_info(self(),current_function),{line,?LINE},T])).

-define(log_info(T),
        error_logger:info_report(
          [process_info(self(),current_function),{line,?LINE},T])).

%% TODO
%% Docs
%% Ets table to store Dir

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0,
         watch/2,
         unwatch/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% ------------------------------------------------------------------
%% Record and Type Definitions
%% ------------------------------------------------------------------

-include_lib("inotify/include/state.hrl").

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec start_link() ->
    {ok, Pid :: pid()} | ignore | {error, {already_started, Pid :: pid()} | term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

% TODO: CB type?
-spec watch(Dirname :: file:name(), any()) -> ok.
watch(Name, CB) ->
    gen_server:cast(?MODULE, {watch, Name, CB}).

-spec unwatch(Dirname :: file:name()) -> ok.
unwatch(Name) ->
    gen_server:cast(?MODULE, {unwatch, Name}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State} |
%%          {ok, State, Timeout} |
%%          ignore |
%%          {stop, Reason}
%%----------------------------------------------------------------------

-spec init(term()) -> {'ok', #state{
    fd::'undefined' | non_neg_integer(), dirnames::'undefined' | ets:tid(),
                watchdescriptors::'undefined' | ets:tid()}}.
init(_Args) ->
    {ok, Fd} = inotify_nif:start(),
    {ok, Ds} = ets_manager:create_or_return(dirnames),
    {ok, Wds} = ets_manager:create_or_return(watchdescriptors),
    {ok, CBs} = ets_manager:create_or_return(callbacks, [bag]),
    {ok, #state{fd=Fd, callbacks = CBs, dirnames = Ds, watchdescriptors = Wds}}.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State} |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State} |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, Reply, State} | (terminate/2 is called)
%%          {stop, Reason, State} (terminate/2 is called)
%%----------------------------------------------------------------------
-spec handle_call(term(), {pid(), term()}, State::state()) -> {reply, ok, any()}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State} |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State} (terminate/2 is called)
%%----------------------------------------------------------------------
-spec handle_cast(term(), State::state())
    -> {stop, normal, State::state()} | {noreply, State::state()}.
handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast({watch, Watch, CB}, State) ->
  {noreply, do_watch(Watch, CB, State)};
handle_cast({unwatch, Unwatch}, State) ->
  {noreply, do_unwatch(Unwatch, State)};
handle_cast(Msg, State) ->
  ?log_info({unknown_message, Msg}),
  {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State} |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State} (terminate/2 is called)
%%----------------------------------------------------------------------
% TODO: be more specific about Info in spec
-spec handle_info(Info::term(), State::state()) -> {noreply, State::state()}.
handle_info({inotify_event, _WD, file, ignored, _Cookie, _File} = _Info, State) ->
    %% ignore unwatched messages.
    {noreply, State};
handle_info({inotify_event, Wd, Type, Event, Cookie, Name} = _Info, State) ->
  case ets:lookup(State#state.watchdescriptors, Wd) of
      [] -> %%?log_info({unknown_file_watch, _Info}),
            {noreply, State};
      [{Wd, File}] ->
            %%?log_info({known_file_watch, Info}),
            [_Look] = [CB({File, Type, Event, Cookie, Name}) ||
                {_File, CB} <- ets:lookup(State#state.callbacks, File)],
            {noreply, State}
  end;
handle_info({'ETS-TRANSFER', _Tid, _Pid, new_table}, State) ->
    %% log at some point?
    {noreply, State};
handle_info({'ETS-TRANSFER', Tid, _Pid, reissued} = _Info, State) ->
    %%?log_info({rewatch_this, _Info}),
    case ets:info(Tid, name) of
        dirnames -> case rewatch(State) of
                        {ok, State} -> ok,
                                       {noreply, State};
                        Error -> ?log_error({error,Error}),
                                 {noreply, State}
                    end;
        _DontCare -> %%?log_info({ignored, _Info}),
                     {noreply, State}
    end;
handle_info(Info, State) ->
  ?log_info({unknown_message, Info}),
  {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
-spec terminate(_, state()) -> {close, fd()}.
terminate(_Reason, #state{fd = Fd}) ->
    case inotify_nif:stop(Fd) of
        ok ->
            ok;
        {ok, _State} ->
            ok;
        Result ->
            ?log_info({unexpected_result, Result})
    end,
    {close, Fd}.

-spec code_change(_, State::state(), _) -> {ok, State::state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% @doc Makes a call to the nif to add a resource to
%% watch. Logs on error
-spec do_watch(Dirname :: file:name(), State::state())
    -> State::state().
do_watch(Dirname, State) ->
    case inotify_nif:add_watch(State#state.fd, Dirname) of
        {ok, Wd} -> ets:insert(State#state.watchdescriptors, {Wd, Dirname}),
                    State;
        Error -> ?log_error([Error, Dirname]),
                 State
    end.

% TODO: CB type?
-spec do_watch(Dirname :: file:name(), any(), State::state())
    -> State::state().
do_watch(Dirname, CB, State) ->
    case inotify_nif:add_watch(State#state.fd, Dirname) of
        {ok, Wd} -> ets:insert(State#state.dirnames, {Dirname, Wd}),
                    ets:insert(State#state.callbacks, {Dirname, CB}),
                    ets:insert(State#state.watchdescriptors, {Wd, Dirname}),
                    State;
        Error -> ?log_error([Error, Dirname]),
                 State
    end.

%% @doc Makes a call to the nif to remove a resource to
%% watch. Logs on error
-spec do_unwatch(Dirname :: file:name(), State::state())
    -> State::state().
do_unwatch(Dirname, State) ->
    case ets:lookup(State#state.dirnames, Dirname) of
        [] -> State;
        [{Dirname,Wd}] ->
            ets:delete(State#state.dirnames, Dirname),
            ets:delete(State#state.callbacks, Dirname),
            ets:delete(State#state.watchdescriptors, Wd),
            ok = inotify_nif:remove_watch(State#state.fd, Wd),
            State
    end.

%% @doc Rewatch everything in the ets table. Assigning a new
%% Wd as we move through.
-spec rewatch (State::state()) -> {ok, State::state()}.
rewatch(State) ->
    ets:delete_all_objects(State#state.watchdescriptors),
    Key = ets:first(State#state.dirnames),
    rewatch(State, Key).

-spec rewatch (State::state(), Key::atom() | [atom() | [any()] | char()]) -> {ok, State::state()}.
rewatch(State, '$end_of_table') ->
    {ok, State};
rewatch(State, Key) ->
    #state{fd = _F, callbacks = _C, dirnames = _D, watchdescriptors = _W} = do_watch(Key, State),
    NextKey = ets:next(State#state.dirnames, Key),
    rewatch(State, NextKey).

%% EOF inotify.erl
