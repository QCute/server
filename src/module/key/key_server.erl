%%%-------------------------------------------------------------------
%%% @doc
%%% module online manager
%%% @end
%%%-------------------------------------------------------------------
-module(key_server).
-behaviour(gen_server).
%% export API function
-export([start/0, start_link/0]).
-export([award/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("player.hrl").
-include("key.hrl").
%%%===================================================================
%%% API
%%%===================================================================
%% @doc start
start() ->
    process:start(?MODULE).

%% @doc gen_server entry
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc award
award(User = #user{id = PlayerId}, Key) ->
    case data_key:award(data_key:get(Key)) of
        #data_key_award{only = Only, award = Award} ->
            case gen_server:call(process:pid(?MODULE), {'get', PlayerId, Key, Only}) of
                {ok, _} ->
                    item:add(User, Award);
                Error ->
                    Error
            end;
        _ ->
            {error, 2}
    end.
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init(_) ->
    ets:new(key, [{keypos, #key.key}, named_table, protected, duplicate_bag]),
    Save = fun(X) -> ets:insert(key, X) end,
    data_tool:load(key_sql:select(), key, Save),
    {ok, key}.

handle_call({'get', PlayerId, Key, Only}, _From, State) ->
    %% update online player info cache
    Reply = award(State, PlayerId, Key, Only),
    {reply, Reply, State};
handle_call(_Info, _From, State)->
    {reply, ok, State}.

handle_cast(_Info, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra)->
    {ok, State}.
%% ====================================================================
%% Internal functions
%% ====================================================================
%% receive award
award(Tab, PlayerId, Key, Only) ->
    case ets:lookup(Tab, Key) of
        [] ->
            KeyData = #key{player_id = PlayerId, key = Key},
            ets:insert(Tab, KeyData),
            key_sql:insert(KeyData),
            {ok, ok};
        List when Only == 0 ->
            case lists:keyfind(PlayerId, #key.player_id, List) of
                false ->
                    KeyData = #key{player_id = PlayerId, key = Key},
                    ets:insert(Tab, KeyData),
                    key_sql:insert(KeyData),
                    {ok, ok};
                _ ->
                    {error, received}
            end;
        _ ->
            {error, received}
    end.
