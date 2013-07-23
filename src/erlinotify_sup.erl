
-module(erlinotify_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

-spec start_link () -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

-spec init (term())
  ->
	  {ok,{
		    {supervisor:strategy(),non_neg_integer(),pos_integer()},
				[supervisor:child_spec()]
		}}.
init([]) ->
    {ok, { {one_for_one, 5, 10}, [?CHILD(erlinotify, worker)] } }.
