%%%-------------------------------------------------------------------
%%% @doc
%%% device 
%%% @end
%%%-------------------------------------------------------------------
-module(device).
%% API
-export([on_create/1]).
-export([on_load/1, on_save/1]).
%% Includes
-include("user.hrl").
-include("device.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc on create
-spec on_create(User :: #user{}) -> NewUser :: #user{}.
on_create(User = #user{role_id = RoleId, device = Device}) ->
    NewDevice = Device#device{role_id = RoleId},
    device_sql:insert(NewDevice),
    User#user{device = NewDevice}.

%% @doc on load
-spec on_load(User :: #user{}) -> NewUser :: #user{}.
on_load(User = #user{role_id = RoleId}) ->
    case device_sql:select(RoleId) of
        [Device] ->
            Device;
        [] ->
            Device = #device{}
    end,
    User#user{device = Device}.

%% @doc on save
-spec on_save(User :: #user{}) -> NewUser :: #user{}.
on_save(User = #user{role_id = RoleId, device = Device = #device{role_id = 0}}) ->
    NewDevice = Device#device{role_id = RoleId},
    %% insert new
    device_sql:insert(NewDevice),
    User#user{device = NewDevice};
on_save(User = #user{device = Device}) ->
    device_sql:update(Device),
    User.

%%%===================================================================
%%% Internal functions
%%%===================================================================
