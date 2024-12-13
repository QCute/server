%%%-------------------------------------------------------------------
%%% @doc
%%% package 
%%% @end
%%%-------------------------------------------------------------------
-module(package).
%% API
-export([on_load/1, on_save/1]).
%% Includes
-include("user.hrl").
-include("package.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc on load
-spec on_load(User :: #user{}) -> NewUser :: #user{}.
on_load(User = #user{role_id = RoleId}) ->
    case package_sql:select(RoleId) of
        [Package] ->
            Package;
        [] ->
            Package = #package{
                role_id = RoleId,
                item_size = parameter_data:get(item_size),
                bag_size = parameter_data:get(bag_size),
                body_size = parameter_data:get(body_size),
                store_size = parameter_data:get(store_size)
            }
    end,
    User#user{package = Package}.

%% @doc on save
-spec on_save(User :: #user{}) -> NewUser :: #user{}.
on_save(User = #user{role_id = RoleId, package = Package = #package{role_id = 0}}) ->
    NewPackage = Package#package{role_id = RoleId},
    %% insert new
    package_sql:insert(NewPackage),
    User#user{package = NewPackage};
on_save(User = #user{package = Package}) ->
    package_sql:update(Package),
    User.

%%%===================================================================
%%% Internal functions
%%%===================================================================
