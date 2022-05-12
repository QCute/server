%%%-------------------------------------------------------------------
%%% @doc
%%% game master
%%% @end
%%%-------------------------------------------------------------------
-module(master).
%% API
-export([treat/3]).
%% Includes
-include("common.hrl").
-include("net.hrl").
-include("online.hrl").
-include("notice.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc treat game master command
-spec treat(State :: #client{}, Http :: #http{}, Body :: binary()) -> {ok, NewState :: #client{}} | {stop, Reason :: term(), NewState :: #client{}}.
treat(State, Http = #http{version = Version}, Body) ->
    case allow(State, Http) of
        true ->
            Json = json:decode(Body, maps:new()),
            Command = json:get(<<"command">>, Json, <<"">>),
            Data = json:get(<<"data">>, Json, maps:new()),
            Message = execute_command(State, Data, Command),
            Result = json:encode(maps:put(result, Message, maps:new())),
            Response = [
                Version, <<" 200 OK\r\n">>,
                <<"Connection: keep-alive\r\n">>,
                <<"Keep-Alive: timeout=60, max=1000\r\n">>,
                <<"Date: ">>, httpd_util:rfc1123_date(), <<"\r\n">>,
                <<"Content-Type: application/json">>, <<"\r\n">>,
                <<"Content-Length: ">>, integer_to_binary(byte_size(Result)), <<"\r\n">>,
                <<"\r\n">>, Result
            ],
            sender:send(State, list_to_binary(Response)),
            {ok, State};
        false ->
            {stop, normal, State}
    end.

allow(#client{ip = {127, 0, 0, 1}}, _) ->
    true;
allow(#client{ip = {0, 0, 0, 0, 0, 0, 16#7f00, 16#01}}, _) ->
    true;
allow(#client{}, #http{fields = Fields}) ->
    listing:key_find(<<"Cookie">>, 1, Fields, <<>>) == atom_to_binary(config:cookie()).

%%%===================================================================
%%% Internal functions
%%%===================================================================
execute_command(_State, Data, <<"recharge">>) ->
    RoleId = json:get(<<"role_id">>, Data, 0),
    RechargeNo = json:get(<<"recharge_no">>, Data, 0),
    user_server:apply_cast(RoleId, recharge, recharge, [RechargeNo]),
    <<"ok">>;

execute_command(_State, Data, <<"notice">>) ->
    RoleId = db:select_column(<<"SELECT `role_id` FROM `role`">>),
    Title = json:get(<<"title">>, Data, <<>>),
    Content = json:get(<<"content">>, Data, <<>>),
    Items = parser:to_term(json:get(<<"items">>, Data, [])),
    mail:send(RoleId, Title, Content, ?MODULE, Items),
    <<"ok">>;

execute_command(_State, Data, <<"mail">>) ->
    RoleId = json:get(<<"role_id">>, Data, []),
    Title = json:get(<<"title">>, Data, <<>>),
    Content = json:get(<<"content">>, Data, <<>>),
    Items = parser:to_term(json:get(<<"items">>, Data, [])),
    mail:send(RoleId, Title, Content, ?MODULE, Items),
    <<"ok">>;

%% server create role control
execute_command(_State, _Data, <<"set_server_allow_create">>) ->
    user_manager:set_create_state(?TRUE),
    <<"ok">>;
execute_command(_State, _Data, <<"set_server_refuse_create">>) ->
    user_manager:set_create_state(?FALSE),
    <<"ok">>;

%% server login control
execute_command(_State, _Data, <<"set_server_refuse">>) ->
    user_manager:set_server_state(?SERVER_STATE_FORBIDDEN),
    <<"ok">>;
execute_command(_State, _Data, <<"set_server_normal">>) ->
    user_manager:set_server_state(?SERVER_STATE_NORMAL),
    <<"ok">>;
execute_command(_State, _Data, <<"set_server_insider">>) ->
    user_manager:set_server_state(?SERVER_STATE_INSIDER),
    <<"ok">>;
execute_command(_State, _Data, <<"set_server_master">>) ->
    user_manager:set_server_state(?SERVER_STATE_MASTER),
    <<"ok">>;

%% server chat control
execute_command(_State, _Data, <<"set_server_chat_unlimited">>) ->
    user_manager:set_chat_state(?CHAT_STATE_UNLIMITED),
    <<"ok">>;
execute_command(_State, _Data, <<"set_server_chat_silent">>) ->
    user_manager:set_chat_state(?CHAT_STATE_SILENT),
    <<"ok">>;
execute_command(_State, _Data, <<"set_server_chat_silent_world">>) ->
    ChatState = user_manager:get_chat_state(),
    user_manager:set_chat_state(ChatState bor ?CHAT_STATE_SILENT_WORLD),
    <<"ok">>;
execute_command(_State, _Data, <<"set_server_chat_silent_guild">>) ->
    ChatState = user_manager:get_chat_state(),
    user_manager:set_chat_state(ChatState bor ?CHAT_STATE_SILENT_GUILD),
    <<"ok">>;
execute_command(_State, _Data, <<"set_server_chat_silent_private">>) ->
    ChatState = user_manager:get_chat_state(),
    user_manager:set_chat_state(ChatState bor ?CHAT_STATE_SILENT_PRIVATE),
    <<"ok">>;

%% role login control
execute_command(_State, Data, <<"set_role_refuse">>) ->
    RoleIdList = json:get(<<"role_id">>, Data, []),
    db:query(parser:format(<<"UPDATE `role` SET `type` = ~w WHERE `role_id` IN (~s)">>, [?SERVER_STATE_FORBIDDEN, parser:join(RoleIdList, <<"~w">>)])),
    [user_server:apply_cast(Id, role, set_type, [?SERVER_STATE_FORBIDDEN]) || Id <- RoleIdList],
    <<"ok">>;
execute_command(_State, Data, <<"set_role_normal">>) ->
    RoleIdList = json:get(<<"role_id">>, Data, []),
    db:query(parser:format(<<"UPDATE `role` SET `type` = ~w WHERE `role_id` IN (~s)">>, [?SERVER_STATE_NORMAL, parser:join(RoleIdList, <<"~w">>)])),
    [user_server:apply_cast(Id, role, set_type, [?SERVER_STATE_NORMAL]) || Id <- RoleIdList],
    <<"ok">>;
execute_command(_State, Data, <<"set_role_insider">>) ->
    RoleIdList = json:get(<<"role_id">>, Data, []),
    db:query(parser:format(<<"UPDATE `role` SET `type` = ~w WHERE `role_id` IN (~s)">>, [?SERVER_STATE_INSIDER, parser:join(RoleIdList, <<"~w">>)])),
    [user_server:apply_cast(Id, role, set_type, [?SERVER_STATE_INSIDER]) || Id <- RoleIdList],
    <<"ok">>;
execute_command(_State, Data, <<"set_role_master">>) ->
    RoleIdList = json:get(<<"role_id">>, Data, []),
    db:query(parser:format(<<"UPDATE `role` SET `type` = ~w WHERE `role_id` IN (~s)">>, [?SERVER_STATE_MASTER, parser:join(RoleIdList, <<"~w">>)])),
    [user_server:apply_cast(Id, role, set_type, [?SERVER_STATE_MASTER]) || Id <- RoleIdList],
    <<"ok">>;

%% role chat control
execute_command(_State, Data, <<"set_role_chat_unlimited">>) ->
    RoleIdList = json:get(<<"role_id">>, Data, []),
    db:query(parser:format(<<"UPDATE `role` SET `status` = `status` WHERE `role_id` IN (~s)">>, [?CHAT_STATE_UNLIMITED, parser:join(RoleIdList, <<"~w">>)])),
    [user_server:apply_cast(Id, role, set_status, [?CHAT_STATE_UNLIMITED]) || Id <- RoleIdList],
    <<"ok">>;
execute_command(_State, Data, <<"set_role_chat_silent">>) ->
    RoleIdList = json:get(<<"role_id">>, Data, []),
    db:query(parser:format(<<"UPDATE `role` SET `status` = `status` | ~w WHERE `role_id` IN (~s)">>, [?CHAT_STATE_SILENT, parser:join(RoleIdList, <<"~w">>)])),
    [user_server:apply_cast(Id, role, set_status, [?CHAT_STATE_SILENT]) || Id <- RoleIdList],
    <<"ok">>;
execute_command(_State, Data, <<"set_role_chat_silent_world">>) ->
    RoleIdList = json:get(<<"role_id">>, Data, []),
    db:query(parser:format(<<"UPDATE `role` SET `status` = `status` | ~w WHERE `role_id` IN (~s)">>, [?CHAT_STATE_SILENT_WORLD, parser:join(RoleIdList, <<"~w">>)])),
    [user_server:apply_cast(Id, role, set_status, [?CHAT_STATE_SILENT_WORLD]) || Id <- RoleIdList],
    <<"ok">>;
execute_command(_State, Data, <<"set_role_chat_silent_guild">>) ->
    RoleIdList = json:get(<<"role_id">>, Data, []),
    db:query(parser:format(<<"UPDATE `role` SET `status` = `status` | ~w WHERE `role_id` IN (~s)">>, [?CHAT_STATE_SILENT_GUILD, parser:join(RoleIdList, <<"~w">>)])),
    [user_server:apply_cast(Id, role, set_status, [?CHAT_STATE_SILENT_GUILD]) || Id <- RoleIdList],
    <<"ok">>;
execute_command(_State, Data, <<"set_role_chat_silent_private">>) ->
    RoleIdList = json:get(<<"role_id">>, Data, []),
    db:query(parser:format(<<"UPDATE `role` SET `status` = `status` | ~w WHERE `role_id` IN (~s)">>, [?CHAT_STATE_SILENT_PRIVATE, parser:join(RoleIdList, <<"~w">>)])),
    [user_server:apply_cast(Id, role, set_status, [?CHAT_STATE_SILENT_PRIVATE]) || Id <- RoleIdList],
    <<"ok">>;

%% test
execute_command(_State, _Data, <<"test">>) ->
    <<"test">>;
%% error report
execute_command(_State, _Data, Command) ->
    <<"Unknown Command: ", Command/binary>>.
