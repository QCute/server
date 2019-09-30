%%%-------------------------------------------------------------------
%%% @doc
%%% module buff
%%% @end
%%%-------------------------------------------------------------------
-module(buff).
%% API
-export([load/1, save/1, clean/1]).
-export([query/1]).
-export([add/2]).
%% Includes
-include("common.hrl").
-include("user.hrl").
-include("buff.hrl").
-include("protocol.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% @doc load
-spec load(User :: #user{}) -> NewUser :: #user{}.
load(User = #user{role_id = RoleId}) ->
    Buff =  parser:convert(buff_sql:select(RoleId), ?MODULE),
    User#user{buff = Buff}.

%% @doc save
-spec save(User :: #user{}) -> NewUser :: #user{}.
save(User = #user{buff = Buff}) ->
    NewSkill = buff_sql:insert_update(Buff),
    User#user{buff = NewSkill}.

%% @doc clean
-spec clean(User :: #user{}) -> NewUser :: #user{}.
clean(User) ->
    User.

%% @doc query
-spec query(User :: #user{}) -> ok().
query(#user{buff = Buff}) ->
    {ok, [Buff]}.

%% @doc add
-spec add(User :: #user{}, BuffId :: non_neg_integer()) -> ok() | error().
add(User = #user{role_id = RoleId, buff = BuffList}, BuffId) ->
    Now = time:ts(),
    Buff = #buff{end_time = EndTime, overlap = Overlap} = listing:key_find(BuffId, #buff.buff_id, BuffList, #buff{role_id = RoleId, buff_id = BuffId, start_time = Now}),
    case buff_data:get(BuffId) of
        #buff_data{overlap_type = 1, time = Time} ->
            NewBuff = Buff#buff{end_time = Time + EndTime, flag = 1},
            NewBuffList = lists:keystore(BuffId, #buff.buff_id, BuffList, NewBuff),
            user_sender:send(User, ?PROTOCOL_BUFF, [Buff]),
            {ok, User#user{buff = NewBuffList}};
        #buff_data{overlap_type = 2, effect = Effect} ->
            NewBuff = Buff#buff{overlap = Overlap + 1, flag = 1},
            NewBuffList = lists:keystore(BuffId, #buff.buff_id, BuffList, NewBuff),
            {ok, NewUser} = effect:add(User, Effect),
            user_sender:send(NewUser, ?PROTOCOL_BUFF, [Buff]),
            {ok, NewUser#user{buff = NewBuffList}};
        #buff_data{overlap_type = 3, time = Time, effect = Effect} ->
            NewBuff = Buff#buff{end_time = Time + EndTime, overlap = Overlap + 1, flag = 1},
            NewBuffList = lists:keystore(BuffId, #buff.buff_id, BuffList, NewBuff),
            {ok, NewUser} = effect:add(User, Effect),
            user_sender:send(NewUser, ?PROTOCOL_BUFF, [Buff]),
            {ok, NewUser#user{buff = NewBuffList}};
        _ ->
            {error, 0}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================