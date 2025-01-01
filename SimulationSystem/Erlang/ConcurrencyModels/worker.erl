-module(worker).
-behaviour(gen_server).

%% API exports
-export([
    start_link/1,
    perform_task/2,
    get_state/1,
    get_task_result/2,
    stop/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(TIMEOUT, 5000).

%% State record
-record(state, {
    worker_id,
    tasks = #{},          % Map of task_id -> {status, result}
    task_counter = 0,
    stats = #{
        completed = 0,
        failed = 0,
        processing = 0
    }
}).

%%% API Functions

start_link(WorkerId) ->
    gen_server:start_link(?MODULE, [WorkerId], []).

perform_task(Pid, Task) ->
    gen_server:call(Pid, {perform_task, Task}).

get_state(Pid) ->
    gen_server:call(Pid, get_state).

get_task_result(Pid, TaskId) ->
    gen_server:call(Pid, {get_task_result, TaskId}).

stop(Pid) ->
    gen_server:cast(Pid, stop).

%%% Callback Functions

init([WorkerId]) ->
    process_flag(trap_exit, true),
    {ok, #state{worker_id = WorkerId}}.

handle_call({perform_task, Task}, _From, State) ->
    TaskId = State#state.task_counter + 1,
    NewState = State#state{
        task_counter = TaskId,
        tasks = maps:put(TaskId, {processing, undefined}, State#state.tasks),
        stats = update_stats(State#state.stats, processing, 1)
    },
    
    % Spawn task execution
    Self = self(),
    spawn_link(fun() ->
        try
            Result = execute_task(Task),
            gen_server:cast(Self, {task_completed, TaskId, Result})
        catch
            Error:Reason:Stacktrace ->
                gen_server:cast(Self, {task_failed, TaskId, {Error, Reason, Stacktrace}})
        end
    end),
    
    {reply, {ok, TaskId}, NewState};

handle_call({get_task_result, TaskId}, _From, State) ->
    case maps:find(TaskId, State#state.tasks) of
        {ok, {Status, Result}} ->
            {reply, {ok, {Status, Result}}, State};
        error ->
            {reply, {error, task_not_found}, State}
    end;

handle_call(get_state, _From, State) ->
    {reply, {ok, State}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({task_completed, TaskId, Result}, State) ->
    NewTasks = maps:put(TaskId, {completed, Result}, State#state.tasks),
    NewStats = State#state.stats
        |> update_stats(processing, -1)
        |> update_stats(completed, 1),
    {noreply, State#state{tasks = NewTasks, stats = NewStats}};

handle_cast({task_failed, TaskId, Error}, State) ->
    NewTasks = maps:put(TaskId, {failed, Error}, State#state.tasks),
    NewStats = State#state.stats
        |> update_stats(processing, -1)
        |> update_stats(failed, 1),
    {noreply, State#state{tasks = NewTasks, stats = NewStats}};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', _Pid, Reason}, State) ->
    error_logger:error_msg("Worker ~p received EXIT: ~p~n", [State#state.worker_id, Reason]),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    error_logger:info_msg("Worker ~p terminating. Stats: ~p~n", [State#state.worker_id, State#state.stats]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal Functions

execute_task(Task) when is_function(Task, 0) ->
    Task();
execute_task({Module, Function, Args}) ->
    apply(Module, Function, Args);
execute_task(Task) when is_number(Task) ->
    % Example operation for backward compatibility
    Task * 2.

update_stats(Stats, Key, Delta) ->
    CurrentValue = maps:get(Key, Stats, 0),
    maps:put(Key, CurrentValue + Delta, Stats).
% Start a worker
{ok, Pid} = worker:start_link("worker1").

% Submit different types of tasks
% Simple numeric task (backward compatible)
{ok, TaskId1} = worker:perform_task(Pid, 42).

% Function task
{ok, TaskId2} = worker:perform_task(Pid, fun() -> 
    timer:sleep(1000), 
    "Complex calculation" 
end).

% MFA task
{ok, TaskId3} = worker:perform_task(Pid, {math, pow, [2, 8]}).

% Get task results
{ok, Result1} = worker:get_task_result(Pid, TaskId1).

% Get worker state
{ok, State} = worker:get_state(Pid).

% Stop the worker
worker:stop(Pid).