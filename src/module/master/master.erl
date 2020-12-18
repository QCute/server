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
            Command = proplists:get_value(<<"command">>, Fields, <<"">>),
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
execute_command(_State, _Http, <<"set_server_normal">>) ->
    user_manager:set_server_state(?SERVER_STATE_NORMAL),
    <<"ok">>;
execute_command(_State, _Http, <<"set_server_insider">>) ->
    user_manager:set_server_state(?SERVER_STATE_INSIDER),
    <<"ok">>;
execute_command(_State, _Http, <<"set_server_master">>) ->
    user_manager:set_server_state(?SERVER_STATE_MASTER),
    <<"ok">>;
execute_command(_State, _Http, <<"set_server_chat_unlimited">>) ->
    user_manager:set_chat_state(?CHAT_STATE_UNLIMITED),
    <<"ok">>;
execute_command(_State, _Http, <<"set_server_chat_silent">>) ->
    ChatState = user_manager:get_chat_state(),
    user_manager:set_chat_state(ChatState bor ?CHAT_STATE_SILENT),
    <<"ok">>;
execute_command(_State, _Http, <<"set_server_chat_silent_world">>) ->
    ChatState = user_manager:get_chat_state(),
    user_manager:set_chat_state(ChatState bor ?CHAT_STATE_SILENT_WORLD),
    <<"ok">>;
execute_command(_State, _Http, <<"set_server_chat_silent_guild">>) ->
    ChatState = user_manager:get_chat_state(),
    user_manager:set_chat_state(ChatState bor ?CHAT_STATE_SILENT_GUILD),
    <<"ok">>;
execute_command(_State, _Http, <<"set_server_chat_silent_private">>) ->
    ChatState = user_manager:get_chat_state(),
    user_manager:set_chat_state(ChatState bor ?CHAT_STATE_SILENT_PRIVATE),
    <<"ok">>;

execute_command(_State, #http{body = Body}, <<"set_role_refuse">>) ->
    RoleIdList = json:get(<<"role_id">>, Body, <<>>),
    db:query(parser:format(<<"UPDATE `role` SET `type` = ~w WHERE `role_id` IN (~s)">>, [?SERVER_STATE_REFUSE, parser:join(RoleIdList, <<"~w">>)])),
    [user_server:apply_cast(Id, role, set_type, [?SERVER_STATE_REFUSE]) || Id <- RoleIdList],
    <<"ok">>;
execute_command(_State, #http{body = Body}, <<"set_role_normal">>) ->
    RoleIdList = json:get(<<"role_id">>, Body, <<>>),
    db:query(parser:format(<<"UPDATE `role` SET `type` = ~w WHERE `role_id` IN (~s)">>, [?SERVER_STATE_NORMAL, parser:join(RoleIdList, <<"~w">>)])),
    [user_server:apply_cast(Id, role, set_type, [?SERVER_STATE_NORMAL]) || Id <- RoleIdList],
    <<"ok">>;
execute_command(_State, #http{body = Body}, <<"set_role_insider">>) ->
    RoleIdList = json:get(<<"role_id">>, Body, <<>>),
    db:query(parser:format(<<"UPDATE `role` SET `type` = ~w WHERE `role_id` IN (~s)">>, [?SERVER_STATE_INSIDER, parser:join(RoleIdList, <<"~w">>)])),
    [user_server:apply_cast(Id, role, set_type, [?SERVER_STATE_INSIDER]) || Id <- RoleIdList],
    <<"ok">>;
execute_command(_State, #http{body = Body}, <<"set_role_master">>) ->
    RoleIdList = json:get(<<"role_id">>, Body, <<>>),
    db:query(parser:format(<<"UPDATE `role` SET `type` = ~w WHERE `role_id` IN (~s)">>, [?SERVER_STATE_MASTER, parser:join(RoleIdList, <<"~w">>)])),
    [user_server:apply_cast(Id, role, set_type, [?SERVER_STATE_MASTER]) || Id <- RoleIdList],
    <<"ok">>;
execute_command(_State, #http{body = Body}, <<"set_role_chat_unlimited">>) ->
    RoleIdList = json:get(<<"role_id">>, Body, <<>>),
    db:query(parser:format(<<"UPDATE `role` SET `status` = `status` WHERE `role_id` IN (~s)">>, [?CHAT_STATE_UNLIMITED, parser:join(RoleIdList, <<"~w">>)])),
    [user_server:apply_cast(Id, role, set_status, [?CHAT_STATE_UNLIMITED]) || Id <- RoleIdList],
    <<"ok">>;
execute_command(_State, #http{body = Body}, <<"set_role_chat_slient">>) ->
    RoleIdList = json:get(<<"role_id">>, Body, <<>>),
    db:query(parser:format(<<"UPDATE `role` SET `status` = `status` | ~w WHERE `role_id` IN (~s)">>, [?CHAT_STATE_SILENT, parser:join(RoleIdList, <<"~w">>)])),
    [user_server:apply_cast(Id, role, set_status, [?CHAT_STATE_SILENT]) || Id <- RoleIdList],
    <<"ok">>;
execute_command(_State, #http{body = Body}, <<"set_role_chat_slient_world">>) ->
    RoleIdList = json:get(<<"role_id">>, Body, <<>>),
    db:query(parser:format(<<"UPDATE `role` SET `status` = `status` | ~w WHERE `role_id` IN (~s)">>, [?CHAT_STATE_SILENT_WORLD, parser:join(RoleIdList, <<"~w">>)])),
    [user_server:apply_cast(Id, role, set_status, [?CHAT_STATE_SILENT_WORLD]) || Id <- RoleIdList],
    <<"ok">>;
execute_command(_State, #http{body = Body}, <<"set_role_chat_slient_guild">>) ->
    RoleIdList = json:get(<<"role_id">>, Body, <<>>),
    db:query(parser:format(<<"UPDATE `role` SET `status` = `status` | ~w WHERE `role_id` IN (~s)">>, [?CHAT_STATE_SILENT_GUILD, parser:join(RoleIdList, <<"~w">>)])),
    [user_server:apply_cast(Id, role, set_status, [?CHAT_STATE_SILENT_GUILD]) || Id <- RoleIdList],
    <<"ok">>;
execute_command(_State, #http{body = Body}, <<"set_role_chat_slient_private">>) ->
    RoleIdList = json:get(<<"role_id">>, Body, <<>>),
    db:query(parser:format(<<"UPDATE `role` SET `status` = `status` | ~w WHERE `role_id` IN (~s)">>, [?CHAT_STATE_SILENT_PRIVATE, parser:join(RoleIdList, <<"~w">>)])),
    [user_server:apply_cast(Id, role, set_status, [?CHAT_STATE_SILENT_PRIVATE]) || Id <- RoleIdList],
    <<"ok">>;

execute_command(_State, _Http, Command) ->
    <<"Unknown Command: ", Command/binary>>.
