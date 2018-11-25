%%%-------------------------------------------------------------------
%%% @doc
%%% module rand rate
%%% @end
%%%-------------------------------------------------------------------
-module(rand_server).
-compile(nowarn_deprecated_function).
-behaviour(gen_server).
%% API
-export([one/1, one/2]).
-export([hit/1, hit/3, hit_ge/1, hit_ge/3, hit_le/1, hit_le/3]).
-export([rand/0, rand/2, fetch/0]).
-export([start/0, start_link/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% state record
-record(state, {seed}).
%%%===================================================================
%%% API
%%%===================================================================
%% @doc 从列表随机一个
-spec one(List :: list()) -> term().
one(List) ->
    one(List, []).
-spec one(List :: list(), Default :: term()) -> term().
one([], Default) ->
    Default;
one([I], _) ->
    I;
one(List, _) ->
    lists:nth(rand(1, length(List)), List).

%% @doc 命中判断 (大于等于)
-spec hit(Rate :: non_neg_integer()) -> boolean().
hit(Rate) ->
    hit(1, 1000, Rate).
-spec hit(Min :: non_neg_integer(), Max :: non_neg_integer(), Rate :: non_neg_integer()) -> boolean().
hit(Min, Max, Rate) ->
    Rate =< rand(Min, Max).

%% @doc 命中判断(大于等于)
-spec hit_ge(Rate :: non_neg_integer()) -> boolean().
hit_ge(Rate) ->
    hit_ge(1, 1000, Rate).
-spec hit_ge(Min :: non_neg_integer(), Max :: non_neg_integer(), Rate :: non_neg_integer()) -> boolean().
hit_ge(Min, Max, Rate) ->
    Rate =< rand(Min, Max).

%% @doc 命中判断大(小于等于)
-spec hit_le(Rate :: non_neg_integer()) -> boolean().
hit_le(Rate) ->
    hit_le(1, 1000, Rate).
-spec hit_le(Min :: non_neg_integer(), Max :: non_neg_integer(), Rate :: non_neg_integer()) -> boolean().
hit_le(Min, Max, Rate) ->
    rand(Min, Max) =< Rate.

%% @doc 产生一个介于Min到Max之间的随机整数
-spec rand() -> pos_integer().
rand() ->
    rand(1, 1000).
-spec rand(Min :: pos_integer(), Max :: pos_integer()) -> pos_integer().
rand(Same, Same) ->
    Same;
rand(Min, Max) ->
    %% 如果没有种子，将从核心服务器中去获取一个种子，以保证不同进程都可取得不同的种子
    case get('RANDOM_SEED') of
        undefined ->
            RandSeed = fetch(),
            random:seed(RandSeed),
            put('RANDOM_SEED', RandSeed);
        _ ->
            skip
    end,
    M = Min - 1,
    random:uniform(Max - M) + M.

%% @doc 取得一个随机数种子
-spec fetch() -> {pos_integer(), pos_integer(), pos_integer()}.
fetch() ->
    gen_server:call(?MODULE, 'GET').

%% @doc server start
start() ->
    server_supervisor:start_child(?MODULE).

%% @doc server start
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([]) ->
    random:seed(erlang:now()),
    {ok, #state{seed = get(random_seed)}}.

handle_call('GET', _From, State = #state{seed = S}) ->
    random:seed(S),
    Seed = {random:uniform(897456), random:uniform(742038), random:uniform(678523)},
    {reply, Seed, State#state{seed = Seed}};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
