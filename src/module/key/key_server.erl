%%%-------------------------------------------------------------------
%%% @doc
%%% module key server
%%% @end
%%%-------------------------------------------------------------------
-module(key_server).
-behaviour(gen_server).
%% API
-export([start/0, start_link/0]).
-export([award/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% Includes
-include("common.hrl").
-include("user.hrl").
-include("role.hrl").
-include("key.hrl").
%%%===================================================================
%%% API
%%%===================================================================
%% @doc start
-spec start() -> {ok, Pid :: pid()} | {error, term()}.
start() ->
    process:start(?MODULE).

%% @doc server start
-spec start_link() -> {ok, Pid :: pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc award
-spec award(User :: #user{}, Key :: binary()) -> ok() | error().
award(User = #user{role_id = RoleId}, Key) ->
    case key_data:award(key_data:get(Key)) of
        #key_award_data{only = Only, award = Award} ->
            case process:call(?MODULE, {'get', RoleId, Key, Only}) of
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
    parser:convert(key_sql:select(), key, Save),
    {ok, key}.

handle_call({'get', RoleId, Key, Only}, _From, State) ->
    %% update online role info cache
    Reply = award(State, RoleId, Key, Only),
    {reply, Reply, State};
handle_call(_Info, _From, State) ->
    {reply, ok, State}.

handle_cast(_Info, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%% ====================================================================
%% Internal functions
%% ====================================================================
%% receive award
award(Tab, RoleId, Key, Only) ->
    case ets:lookup(Tab, Key) of
        [] ->
            KeyData = #key{role_id = RoleId, key = Key},
            ets:insert(Tab, KeyData),
            key_sql:insert(KeyData),
            {ok, ok};
        List when Only == 0 ->
            case lists:keyfind(RoleId, #key.role_id, List) of
                false ->
                    KeyData = #key{role_id = RoleId, key = Key},
                    ets:insert(Tab, KeyData),
                    key_sql:insert(KeyData),
                    {ok, ok};
                _ ->
                    {error, received}
            end;
        _ ->
            {error, received}
    end.
