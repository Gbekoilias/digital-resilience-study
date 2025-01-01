-module(data_handler).
-behaviour(gen_server).

%% API exports
-export([
    start_link/0,
    load_data/1,
    load_data/2,
    preprocess_data/1,
    get_processed_data/0,
    get_stats/0,
    clear_data/0
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

-define(SERVER, ?MODULE).
-define(DEFAULT_CHUNK_SIZE, 1024 * 1024). % 1MB chunks

-record(state, {
    raw_data = #{},        % Map of filename to raw data
    processed_data = #{},  % Map of filename to processed data
    stats = #{            % Statistics about processed data
        total_files = 0,
        total_size = 0,
        processing_errors = 0
    }
}).

%%% API Functions

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

load_data(Filename) ->
    load_data(Filename, []).

load_data(Filename, Options) ->
    gen_server:call(?SERVER, {load_data, Filename, Options}, infinity).

preprocess_data(Data) when is_binary(Data) ->
    gen_server:call(?SERVER, {preprocess_data, Data}, infinity).

get_processed_data() ->
    gen_server:call(?SERVER, get_processed_data).

get_stats() ->
    gen_server:call(?SERVER, get_stats).

clear_data() ->
    gen_server:cast(?SERVER, clear_data).

%%% Callback Functions

init([]) ->
    {ok, #state{}}.

handle_call({load_data, Filename, Options}, _From, State) ->
    case load_file(Filename, Options) of
        {ok, Data} ->
            NewState = update_state_with_data(State, Filename, Data),
            {reply, {ok, Data}, NewState};
        Error ->
            {reply, Error, State}
    end;

handle_call({preprocess_data, Data}, _From, State) ->
    try
        ProcessedData = do_preprocess_data(Data),
        NewState = State#state{
            processed_data = maps:put(make_ref(), ProcessedData, State#state.processed_data)
        },
        {reply, {ok, ProcessedData}, NewState}
    catch
        Error:Reason:Stacktrace ->
            error_logger:error_msg(
                "Data preprocessing failed:~n Error: ~p~n Reason: ~p~n Stacktrace: ~p~n",
                [Error, Reason, Stacktrace]
            ),
            NewStats = maps:update_with(
                processing_errors,
                fun(Count) -> Count + 1 end,
                1,
                State#state.stats
            ),
            {reply, {error, {Error, Reason}}, State#state{stats = NewStats}}
    end;

handle_call(get_processed_data, _From, State) ->
    {reply, {ok, State#state.processed_data}, State};

handle_call(get_stats, _From, State) ->
    {reply, {ok, State#state.stats}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(clear_data, _State) ->
    {noreply, #state{}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal Functions

load_file(Filename, Options) ->
    ChunkSize = proplists:get_value(chunk_size, Options, ?DEFAULT_CHUNK_SIZE),
    case file:open(Filename, [read, binary]) of
        {ok, File} ->
            try
                read_chunks(File, ChunkSize, [])
            after
                file:close(File)
            end;
        Error ->
            Error
    end.

read_chunks(File, ChunkSize, Acc) ->
    case file:read(File, ChunkSize) of
        {ok, Data} ->
            read_chunks(File, ChunkSize, [Data | Acc]);
        eof ->
            {ok, list_to_binary(lists:reverse(Acc))};
        Error ->
            Error
    end.

do_preprocess_data(Data) when is_binary(Data) ->
    % Example preprocessing steps
    Decoded = decode_data(Data),
    Normalized = normalize_data(Decoded),
    Validated = validate_data(Normalized),
    Transform = transform_data(Validated),
    {ok, Transform}.

decode_data(Data) ->
    try
        jsx:decode(Data)
    catch
        _:_ -> Data  % Return raw data if not JSON
    end.

normalize_data(Data) when is_list(Data) ->
    lists:map(fun normalize_entry/1, Data);
normalize_data(Data) ->
    Data.

normalize_entry(Entry) when is_map(Entry) ->
    maps:map(fun(_Key, Value) ->
        case Value of
            V when is_binary(V) -> string:trim(V);
            V when is_list(V) -> string:trim(V);
            V -> V
        end
    end, Entry);
normalize_entry(Entry) ->
    Entry.

validate_data(Data) ->
    % Add your validation logic here
    Data.

transform_data(Data) ->
    % Add your transformation logic here
    Data.

update_state_with_data(State, Filename, Data) ->
    NewRawData = maps:put(Filename, Data, State#state.raw_data),
    NewStats = State#state.stats#{
        total_files => maps:size(NewRawData),
        total_size => maps:fold(fun(_K, V, Acc) -> Acc + byte_size(V) end, 0, NewRawData)
    },
    State#state{raw_data = NewRawData, stats = NewStats}.
% Start the data handler
{ok, Pid} = data_handler:start_link().

% Load data from a file with custom options
{ok, Data} = data_handler:load_data("input.json", [{chunk_size, 2048}]).

% Preprocess some data
{ok, ProcessedData} = data_handler:preprocess_data(Data).

% Get statistics about processed data
{ok, Stats} = data_handler:get_stats().

% Clear all data
data_handler:clear_data().