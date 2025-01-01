-module(task_supervisor).
-behaviour(supervisor).

%% API exports
-export([
    start_link/0,
    start_worker/1,
    stop_worker/1,
    list_workers/0
]).

%% Supervisor callbacks
-export([init/1]).

%% Optional callbacks
-export([format_status/2]).

-define(SERVER, ?MODULE).
-define(MAX_RESTART, 5).
-define(MAX_TIME, 60).
-define(WORKER_SHUTDOWN_TIMEOUT, 5000).

%%% API Functions

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% Start a new worker with specified args
start_worker(Args) ->
    ChildSpec = create_worker_spec(Args),
    supervisor:start_child(?SERVER, ChildSpec).

%% Stop a specific worker
stop_worker(WorkerId) ->
    case find_worker(WorkerId) of
        {ok, ChildId} ->
            supervisor:terminate_child(?SERVER, ChildId),
            supervisor:delete_child(?SERVER, ChildId),
            ok;
        error ->
            {error, worker_not_found}
    end.

%% List all active workers
list_workers() ->
    [{Id, Pid, Type, [Module]} || 
        {Id, Pid, Type, [Module]} <- supervisor:which_children(?SERVER),
        Pid /= undefined].

%%% Supervisor Callbacks

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => ?MAX_RESTART,
        period => ?MAX_TIME
    },
    
    % Initial worker pool can be defined here if needed
    InitialWorkers = [],
    
    {ok, {SupFlags, InitialWorkers}}.

%%% Optional Callbacks

format_status(_Opt, [_PDict, State]) ->
    [{data, [{"State", State}]}].

%%% Internal Functions

create_worker_spec(Args) ->
    WorkerId = generate_worker_id(),
    #{
        id => WorkerId,
        start => {worker, start_link, [Args]},
        restart => transient,  % Don't restart if terminates normally
        shutdown => ?WORKER_SHUTDOWN_TIMEOUT,
        type => worker,
        modules => [worker]
    }.

generate_worker_id() ->
    list_to_atom("worker_" ++ integer_to_list(erlang:system_time())).

find_worker(WorkerId) ->
    Children = supervisor:which_children(?SERVER),
    find_worker_in_children(WorkerId, Children).

find_worker_in_children(WorkerId, [{ChildId, Pid, _Type, _Modules} | Rest]) ->
    case Pid of
        undefined ->
            find_worker_in_children(WorkerId, Rest);
        _ ->
            case worker:get_id(Pid) of
                {ok, WorkerId} -> {ok, ChildId};
                _ -> find_worker_in_children(WorkerId, Rest)
            end
    end;
find_worker_in_children(_WorkerId, []) ->
    error.

%%% Example recovery strategy module
-module(recovery_strategy).
-export([handle_worker_failure/2]).

handle_worker_failure(WorkerId, Reason) ->
    error_logger:error_msg(
        "Worker ~p failed with reason: ~p. Initiating recovery...",
        [WorkerId, Reason]
    ),
    
    % Implement custom recovery logic here
    case Reason of
        {shutdown, _} ->
            % Normal shutdown, no recovery needed
            ok;
        {error, timeout} ->
            % Restart with increased timeout
            restart_with_timeout(WorkerId);
        _ ->
            % Default recovery: simple restart
            restart_worker(WorkerId)
    end.

restart_with_timeout(WorkerId) ->
    NewArgs = [WorkerId, [{timeout, 30000}]],
    task_supervisor:start_worker(NewArgs).

restart_worker(WorkerId) ->
    task_supervisor:start_worker([WorkerId]).
-module(my_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    task_supervisor:start_link().

stop(_State) ->
    ok.