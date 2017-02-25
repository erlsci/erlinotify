-type fd() :: non_neg_integer().
-type callback() :: term().
-record(state, {fd :: fd(),
                callbacks :: callback(),
                dirnames :: ets:tid(),
                watchdescriptors :: ets:tid()}).
-type state() :: #state{}.
