%%%-------------------------------------------------------------------
%%% @doc
%%% map path finder(async process pool support)
%%% @end
%%%-------------------------------------------------------------------
-module(path_finder).
-behaviour(gen_server).
%% API
-export([find/4]).
-export([start/0, start_link/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc find
-spec find(Id :: non_neg_integer(), MapId :: non_neg_integer(), Start :: a_star:point(), End :: a_star:point()) -> ok | {error, term()}.
find(Id, MapId, Start, End) ->
    case volley:get(?MODULE) of
        {ok, Worker} ->
            gen_server:cast(Worker, {find, self(), Id, MapId, Start, End});
        Error ->
            Error
    end.

%% @doc start role client
-spec start() -> {ok, pid()} | {error, term()}.
start() ->
    %% read pool args from application config
    PoolArgs = config:path_finder_pool(),
    volley:start_pool(?MODULE, [{worker, {?MODULE, start_link, []}} | PoolArgs]).

%% @doc server start
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%% @doc init
-spec init(Args :: term()) -> {ok, State :: []}.
init([]) ->
    erlang:process_flag(trap_exit, true),
    {ok, []}.

%% @doc handle_call
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: []) -> {reply, Reply :: term(), NewState :: []}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% @doc handle_cast
-spec handle_cast(Request :: term(), State :: []) -> {noreply, NewState :: []}.
handle_cast({find, From, Id, MapId, Start, End}, State) ->
    case a_star:find(MapId, Start, End) of
        [_ | T = [_ | _]] ->
            gen_server:cast(From, {path, Id, lists:droplast(lists:sublist(T, 4))});
        [_] ->
            gen_server:cast(From, {path, Id, []});
        _ ->
            gen_server:cast(From, {path, Id, []})
    end,
    {noreply, State};
handle_cast(_Info, State) ->
    {noreply, State}.

%% @doc handle_info
-spec handle_info(Request :: term(), State :: []) -> {noreply, NewState :: []}.
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc terminate
-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()), State :: []) -> {ok, NewState :: []}.
terminate(_Reason, State) ->
    {ok, State}.

%% @doc code_change
-spec code_change(OldVsn :: (term() | {down, term()}), State :: [], Extra :: term()) -> {ok, NewState :: []}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
