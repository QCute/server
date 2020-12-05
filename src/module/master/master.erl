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
-include("online.hrl").
-include("notice.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc treat game master command
-spec treat(State :: #client{}, Http :: #http{}) -> {stop, Reason :: term(), NewState :: #client{}}.
treat(State, Http = #http{version = Version, fields = Fields, body = Body}) ->
    case allow(State) of
        true ->
            Command = proplists:get_value(<<"command">>, Fields, ""),
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
    true.

%%%===================================================================
%%% Internal functions
%%%===================================================================
execute_command(_State, #http{body = Body}, <<"notice">>) ->
    notice:broadcast(?NOTICE_SCOPE_WORLD, ?NOTICE_TYPE_DIALOG, json:get(<<"title">>, Body, <<>>), json:get(<<"content">>, Body, <<>>)),
    <<"ok">>;

execute_command(_State, #http{body = Body}, <<"recharge">>) ->
    RoleId = json:get(<<"role_id">>, Body, <<>>),
    RechargeNo = json:get(<<"recharge_no">>, Body, <<>>),
    user_server:apply_cast(RoleId, recharge, recharge, [RechargeNo]),
    <<"ok">>;

execute_command(_State, #http{body = Body}, <<"mail">>) ->
    RoleId = json:get(<<"role_id">>, Body, <<>>),
    Title = json:get(<<"title">>, Body, <<>>),
    Content = json:get(<<"content">>, Body, <<>>),
    Items = parser:to_term(json:get(<<"Items">>, Body, <<>>)),
    mail:send(RoleId, Title, Content, ?MODULE, Items),
    <<"ok">>;

execute_command(_State, _Http, <<"set_server_refuse">>) ->
    user_manager:set_server_state(?SERVER_STATE_REFUSE),
    <<"ok">>;
execute_command(_State, _Http, <<"set_server_master">>) ->
    user_manager:set_server_state(?SERVER_STATE_MASTER),
    <<"ok">>;
execute_command(_State, _Http, <<"set_server_insider">>) ->
    user_manager:set_server_state(?SERVER_STATE_INSIDER),
    <<"ok">>;
execute_command(_State, _Http, <<"set_server_normal">>) ->
    user_manager:set_server_state(?SERVER_STATE_NORMAL),
    <<"ok">>;
execute_command(_State, _Http, <<"set_server_silent">>) ->
    user_manager:set_server_state(?SERVER_STATE_SILENT),
    <<"ok">>;
execute_command(_State, _Http, <<"set_server_silent_world">>) ->
    user_manager:set_server_state(?SERVER_STATE_SILENT_WORLD),
    <<"ok">>;
execute_command(_State, _Http, <<"set_server_silent_guild">>) ->
    user_manager:set_server_state(?SERVER_STATE_SILENT_GUILD),
    <<"ok">>;
execute_command(_State, _Http, <<"set_server_silent_private">>) ->
    user_manager:set_server_state(?SERVER_STATE_SILENT_PRIVATE),
    <<"ok">>;

execute_command(_State, #http{body = Body}, <<"set_role_refuse">>) ->
    RoleId = json:get(<<"role_id">>, Body, <<>>),
    sql:query(parser:format(<<"UPDATE `role` SET `status` = ((`status` >> 4) << 4) | ~w WHERE `role_id` IN (~s)">>, [?SERVER_STATE_REFUSE bor ?SERVER_STATE_MASTER bor ?SERVER_STATE_INSIDER bor ?SERVER_STATE_NORMAL, RoleId])),
    <<"ok">>;
execute_command(_State, #http{body = Body}, <<"set_role_master">>) ->
    RoleId = json:get(<<"role_id">>, Body, <<>>),
    sql:query(parser:format(<<"UPDATE `role` SET `status` = ((`status` >> 4) << 4) | ~w WHERE `role_id` IN (~s)">>, [?SERVER_STATE_MASTER bor ?SERVER_STATE_INSIDER bor ?SERVER_STATE_NORMAL, RoleId])),
    <<"ok">>;
execute_command(_State, #http{body = Body}, <<"set_role_insider">>) ->
    RoleId = json:get(<<"role_id">>, Body, <<>>),
    sql:query(parser:format(<<"UPDATE `role` SET `status` = ((`status` >> 4) << 4) | ~w WHERE `role_id` IN (~s)">>, [?SERVER_STATE_INSIDER bor ?SERVER_STATE_NORMAL, RoleId])),
    <<"ok">>;
execute_command(_State, #http{body = Body}, <<"set_role_normal">>) ->
    RoleId = json:get(<<"role_id">>, Body, <<>>),
    sql:query(parser:format(<<"UPDATE `role` SET `status` = ((`status` >> 4) << 4) | ~w WHERE `role_id` IN (~s)">>, [?SERVER_STATE_NORMAL, RoleId])),
    <<"ok">>;
execute_command(_State, #http{body = Body}, <<"set_role_slient">>) ->
    RoleId = json:get(<<"role_id">>, Body, <<>>),
    sql:query(parser:format(<<"UPDATE `role` SET `status` = `status` | ~w WHERE `role_id` IN (~s)">>, [?SERVER_STATE_SILENT, RoleId])),
    <<"ok">>;
execute_command(_State, #http{body = Body}, <<"set_role_slient_world">>) ->
    RoleId = json:get(<<"role_id">>, Body, <<>>),
    sql:query(parser:format(<<"UPDATE `role` SET `status` = `status` | ~w WHERE `role_id` IN (~s)">>, [?SERVER_STATE_SILENT_WORLD, RoleId])),
    <<"ok">>;
execute_command(_State, #http{body = Body}, <<"set_role_slient_guild">>) ->
    RoleId = json:get(<<"role_id">>, Body, <<>>),
    sql:query(parser:format(<<"UPDATE `role` SET `status` = `status` | ~w WHERE `role_id` IN (~s)">>, [?SERVER_STATE_SILENT_GUILD, RoleId])),
    <<"ok">>;
execute_command(_State, #http{body = Body}, <<"set_role_slient_private">>) ->
    RoleId = json:get(<<"role_id">>, Body, <<>>),
    sql:query(parser:format(<<"UPDATE `role` SET `status` = `status` | ~w WHERE `role_id` IN (~s)">>, [?SERVER_STATE_SILENT_PRIVATE, RoleId])),
    <<"ok">>;

execute_command(_State, _Http, Command) ->
    <<"Unknown Command: ", Command/binary>>.
