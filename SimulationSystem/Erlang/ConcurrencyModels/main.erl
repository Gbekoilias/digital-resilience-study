%%% main.erl - Application entry point
-module(main).
-behaviour(application).

-export([start/2, stop/1, start/0]).

%% Convenience function for manual starting
start() ->
    application:start(my_app).

%% Application callbacks
start(_StartType, _StartArgs) ->
    my_app_sup:start_link().

stop(_State) ->
    ok.

%%% my_app_sup.erl - Main supervisor
-module(my_app_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },
    
    WorkerSpecs = [
        #{
            id => worker_sup,
            start => {worker_sup, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => supervisor,
            modules => [worker_sup]
        }
    ],
    
    {ok, {SupFlags, WorkerSpecs}}.

%%% worker_sup.erl - Worker supervisor
-module(worker_sup).
-behaviour(supervisor).

-export([start_link/0, init/1, start_worker/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 5,
        period => 60
    },
    
    WorkerSpec = #{
        id => worker,
        start => {worker, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [worker]
    },
    
    {ok, {SupFlags, [WorkerSpec]}}.

start_worker(Args) ->
    supervisor:start_child(?MODULE, [Args]).

%%% worker.erl - Worker implementation
-module(worker).
-behaviour(gen_server).

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([do_work/2]).

-record(state, {
    worker_id,
    data = []
}).

%% API
start_link(WorkerId) ->
    gen_server:start_link(?MODULE, [WorkerId], []).

do_work(Pid, Work) ->
    gen_server:cast(Pid, {do_work, Work}).

%% Callbacks
init([WorkerId]) ->
    process_flag(trap_exit, true),
    {ok, #state{worker_id = WorkerId}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({do_work, Work}, State) ->
    % Perform work here
    NewState = State#state{data = [Work | State#state.data]},
    {noreply, NewState};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
% Start the application
application:start(my_app).

% Start a new worker
{ok, WorkerPid} = worker_sup:start_worker("worker1").

% Send work to the worker
worker:do_work(WorkerPid, "some work").