%%%-------------------------------------------------------------------
%%% @doc
%%% game master
%%% @end
%%%-------------------------------------------------------------------
-module(master).
%% API
-export([treat/2]).
%% Includes
-include("net.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc treat game master command
-spec treat(State :: #client{}, Http :: #http{}) -> {stop, Reason :: term(), NewState :: #client{}}.
treat(State, Http = #http{version = Version, fields = Fields, body = Body}) ->
    case allow(State) of
        true ->
            Command = proplists:get_value("command", Fields, ""),
            Result = execute_command(State, Http#http{body = json:decode(Body)}, Command),
            Response = [
                Version, <<" 200 OK\r\n">>,
                <<"Connection: close\r\n">>,
                <<"Date: ">>, httpd_util:rfc1123_date(), <<"\r\n">>,
                <<"Content-Length: ">>, integer_to_binary(byte_size(Result)), <<"\r\n">>,
                <<"\r\n">>, <<"{\"result\":\"">>, Result, <<"\"\}">>
            ],
            sender:send(State, list_to_binary(Response)),
            {stop, normal, State};
        false ->
            {stop, normal, State}
    end.

allow(#client{ip = {127, 0, 0, 1}}) ->
    true;
allow(#client{ip = {0, 0, 0, 0, 0, 0, 16#7f00, 16#01}}) ->
    true;
allow(_) ->
    false.

%%%===================================================================
%%% Internal functions
%%%===================================================================
execute_command(State, #http{body = Body}, "notice") ->
    notice:broadcast(State, [notice, json:get(<<"title">>, Body, <<>>), json:get(<<"content">>, Body, <<>>)]),
    <<"ok">>;
execute_command(_State, _Http, "mail") ->
    <<"ok">>;
execute_command(_State, _Http, "ban_chat") ->
    <<"ok">>;
execute_command(_State, _Http, "allow_chat") ->
    <<"ok">>;
execute_command(_State, _Http, "set_role_refuse") ->
    <<"ok">>;
execute_command(_State, _Http, "set_role_normal") ->
    <<"ok">>;
execute_command(_State, _Http, "set_role_insider") ->
    <<"ok">>;
execute_command(_State, _Http, "set_role_master") ->
    <<"ok">>;
execute_command(_State, #http{body = Body}, "recharge") ->
    RoleId = json:get(<<"role_id">>, Body, <<>>),
    RechargeNo = json:get(<<"recharge_no">>, Body, <<>>),
    user_server:apply_cast(type:to_integer(RoleId), recharge, recharge, [type:to_integer(RechargeNo)]),
    <<"ok">>;
execute_command(_State, _Http, Command) ->
    <<"Unknown Command: ", Command/binary>>.
