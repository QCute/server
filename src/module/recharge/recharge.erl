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
-spec charge(User :: #user{}, RechargeNo :: non_neg_integer()) -> ok() | error().
charge(User, RechargeNo) ->
    case parser:convert(sql:select(parser:format(<<"SELECT * FROM `recharge` WHERE `recharge_no` = ~w">>, [RechargeNo])), ?MODULE) of
        [#recharge{gold = Gold, status = 0}] ->
            sql:update(parser:format(<<"UPDATE * FROM `recharge` WHERE `recharge_no` = ~w AND `status` = ~w">>, [RechargeNo, 1])),
            asset:add(User, [{gold, Gold}], ?MODULE);
        [#recharge{status = 1}] ->
            {error, gold_already_receive};
        _ ->
            {error, no_such_id}
    end.
%%%==================================================================
%%% Internal functions
%%%==================================================================
