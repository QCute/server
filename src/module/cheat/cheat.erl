%%%------------------------------------------------------------------
%%% @doc
%%% module cheat
%%% @end
%%%------------------------------------------------------------------
-module(cheat).
%% API
-export([cheat/2]).
%% Includes
-include("../../../include/common.hrl").
-include("../../../include/user.hrl").
%% Macros
-ifdef(DEBUG).
-define(CHEAT, 1).
-else.
-define(CHEAT, 0).
-endif.
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc cheat
-spec cheat(User :: #user{}, Command :: string()) -> ok() | error().
cheat(User, Command) ->
    case execute_command(User, Command, ?CHEAT) of
        {ok, NewUser = #user{}} ->
            {ok, [ok, Command], NewUser};
        Error ->
            Error
    end.

%%%==================================================================
%%% Internal functions
%%%==================================================================
execute_command(_User, _Command, 0) ->
    ok;
execute_command(User, Command, _) ->
    case string:tokens(Command, "_") of
        ["add", "gold", Value] ->
            asset:add(User, [{gold, type:to_integer(Value)}], ?MODULE);
        ["add", "sliver", Value] ->
            asset:add(User, [{sliver, type:to_integer(Value)}], ?MODULE);
        _ ->
            {error, no_such_command}
    end.
