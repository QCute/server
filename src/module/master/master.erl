%%%-------------------------------------------------------------------
%%% @doc
%%% module game master
%%% @end
%%%-------------------------------------------------------------------
-module(master).
%% API
-export([treat/2]).
%% Includes
-include("socket.hrl").
-include("online.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc treat game master command
-spec treat(State :: #client{}, Http :: #http{}) -> {stop, Reason :: term(), NewState :: #client{}}.
treat(State, Http) ->
    case allow(State) of
        true ->
            Command = http:get_header_field(<<"Command">>, Http),
            Result = execute_command(State, Http, Command),
            Response = [
                <<"HTTP/">>, http:get_version(Http), <<" 200 OK\r\n">>,
                <<"Connection: Close\r\n">>,
                <<"Date: ">>, list_to_binary(httpd_util:rfc1123_date()), <<"\r\n">>,
                <<"Content-Length: ">>, integer_to_binary(byte_size(Result)), <<"\r\n">>,
                <<"\r\n">>, Result
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
execute_command(State, #http{body = Body}, <<"notice">>) ->
    Json = json:decode(Body),
    Title = json:get(<<"title">>, Json, <<>>),
    Content = json:get(<<"content">>, Json, <<>>),
    notice:broadcast(State, [notice, Title, Content]),
    <<"ok">>;
execute_command(_State, _Http, <<"mail">>) ->
    <<"ok">>;
execute_command(_State, _Http, <<"ban_chat">>) ->
    <<"ok">>;
execute_command(_State, _Http, <<"allow_chat">>) ->
    <<"ok">>;
execute_command(_State, _Http, <<"set_role_refuse">>) ->
    <<"ok">>;
execute_command(_State, _Http, <<"set_role_normal">>) ->
    <<"ok">>;
execute_command(_State, _Http, <<"set_role_insider">>) ->
    <<"ok">>;
execute_command(_State, _Http, <<"set_role_master">>) ->
    <<"ok">>;
execute_command(_State, #http{body = Body}, <<"recharge">>) ->
    Json = json:decode(Body),
    RoleId = json:get(<<"role_id">>, Json, <<>>),
    RechargeNo = json:get(<<"recharge_no">>, Json, <<>>),
    user_server:apply_cast(type:to_integer(RoleId), recharge, recharge, [type:to_integer(RechargeNo)]),
    <<"ok">>;
execute_command(_State, _Http, Command) ->
    <<"Unknown Command: ", Command/binary>>.
