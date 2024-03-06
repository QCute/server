%%%-------------------------------------------------------------------
%%% @doc
%%% device 
%%% @end
%%%-------------------------------------------------------------------
-module(device).
%% API
-export([create/1]).
-export([load/1, save/1]).
%% Includes
-include("common.hrl").
-include("user.hrl").
-include("device.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc create
-spec create(User :: #user{}) -> NewUser :: #user{}.
create(User = #user{role_id = RoleId, device = Device}) ->
    NewDevice = Device#device{role_id = RoleId},
    device_sql:insert(NewDevice),
    User#user{device = NewDevice}.

%% @doc load
-spec load(User :: #user{}) -> NewUser :: #user{}.
load(User = #user{role_id = RoleId}) ->
    case device_sql:select(RoleId) of
        [Device] ->
            Device;
        [] ->
            Device = #device{}
    end,
    User#user{device = Device}.

%% @doc save
-spec save(User :: #user{}) -> NewUser :: #user{}.
save(User = #user{role_id = RoleId, device = Device = #device{role_id = 0}}) ->
    NewDevice = Device#device{role_id = RoleId},
    %% insert new
    device_sql:insert(NewDevice),
    User#user{device = NewDevice};
save(User = #user{device = Device}) ->
    device_sql:update(Device),
    User.

%%%===================================================================
%%% Internal functions
%%%===================================================================
