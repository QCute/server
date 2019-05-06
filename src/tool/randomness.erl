%%%-------------------------------------------------------------------
%%% @doc
%%% module rand
%%% OTP_20 or later, random module will deprecated, rand module replace it
%%% this module can change name to random/randomness
%%% @end
%%%-------------------------------------------------------------------
-module(randomness).
-compile(nowarn_deprecated_function).
-behaviour(gen_server).
-include("common.hrl").
%% API
-export([one/1, one/2]).
-export([ratio/2, ratio_total/2]).
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
    hit(1, 10000, Rate).
-spec hit(Min :: non_neg_integer(), Max :: non_neg_integer(), Rate :: non_neg_integer()) -> boolean().
hit(Min, Max, Rate) ->
    Rate =< rand(Min, Max).

%% @doc 命中判断(大于等于)
-spec hit_ge(Rate :: non_neg_integer()) -> boolean().
hit_ge(Rate) ->
    hit_ge(1, 10000, Rate).
-spec hit_ge(Min :: non_neg_integer(), Max :: non_neg_integer(), Rate :: non_neg_integer()) -> boolean().
hit_ge(Min, Max, Rate) ->
    Rate =< rand(Min, Max).

%% @doc 命中判断大(小于等于)
-spec hit_le(Rate :: non_neg_integer()) -> boolean().
hit_le(Rate) ->
    hit_le(1, 10000, Rate).
-spec hit_le(Min :: non_neg_integer(), Max :: non_neg_integer(), Rate :: non_neg_integer()) -> boolean().
hit_le(Min, Max, Rate) ->
    rand(Min, Max) =< Rate.

%% @doc rand one in fix range (10000 by default)
-spec ratio(List :: [tuple()], N :: pos_integer()) -> Element :: tuple().
ratio(List, N) ->
    Rand = rand(1, 10000),
    find_ratio(List, N, Rand).

%% it will find if given argument valid, let it crash when data error
find_ratio([H | T], N, Rand) ->
    case Rand =< element(N, H) of
        true ->
            H;
        false when T == [] ->
            H;
        false ->
            find_ratio(T, N, Rand)
    end.

%% @doc rand one in total range
-spec ratio_total(List :: [tuple()], N :: pos_integer()) -> Element :: tuple().
ratio_total(List, N) ->
    Total = tool:key_sum(List, N),
    Rand = rand(1, Total),
    find_ratio_total(List, N, Rand, 0).

%% it will find if given argument valid, let it crash when data error
find_ratio_total([H | T], N, Rand, StartRatio) ->
    EndRatio = StartRatio + element(N, H),
    case StartRatio < Rand andalso Rand =< EndRatio of
        true ->
            H;
        false ->
            find_ratio_total(T, N, Rand, EndRatio)
    end.

%% @doc 产生一个介于Min到Max之间的随机整数
-spec rand() -> pos_integer().
rand() ->
    rand(1, 10000).
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
    gen_server:call(process:pid(?MODULE), 'GET').

%% @doc server start
start() ->
    process:start(?MODULE).

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
