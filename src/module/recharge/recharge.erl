%%%------------------------------------------------------------------
%%% @doc
%%% module recharge
%%% @end
%%%------------------------------------------------------------------
-module(recharge).
%% API
-export([charge/2]).
%% Includes
-include("common.hrl").
-include("user.hrl").
-include("recharge.hrl").
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc recharge
-spec charge(User :: #user{}, UniqueId :: non_neg_integer()) -> ok() | error().
charge(User, UniqueId) ->
    case parser:convert(sql:select(io_lib:format("SELECT * FROM `recharge` WHERE `unique_id` = ~p", [UniqueId])), ?MODULE) of
        #recharge{gold = Gold, status = 0} ->
            sql:update(io_lib:format("UPDATE * FROM `recharge` WHERE `unique_id` = ~p AND `status` = ~p", [UniqueId, 1])),
            asset:add(User, [{gold, Gold}], ?MODULE);
        #recharge{status = 1} ->
            {error, gold_already_receive};
        _ ->
            {error, no_such_id}
    end.
%%%==================================================================
%%% Internal functions
%%%==================================================================
