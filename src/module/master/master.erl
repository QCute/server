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
treat(State, Http = #http{fields = Fields}, Body) ->
    case listing:key_get(<<"cookie">>, 1, Fields, <<>>) == atom_to_binary(config:cookie()) of
        true ->
            case listing:key_get(<<"content-type">>, 1, Fields, <<>>) of
                <<"application/json", _/binary>> ->
                    Json = json:decode(Body, maps:new()),
                    dispatch(State, Http, Json),
                    {ok, State};
                _ ->
                    response(State, Http, #{}, 406),
                    {stop, normal, State}
            end;
        false ->
            response(State, Http, #{}, 403),
            {stop, normal, State}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

dispatch(State, Http = #http{uri = <<"/charge">>}, Data) ->
    RoleId = json:get(<<"role_id">>, Data, 0),
    ChargeNo = json:get(<<"charge_no">>, Data, 0),
    user_server:apply_cast(RoleId, charge, charge, [ChargeNo]),
    response(State, Http, #{result => <<"ok">>}, 200);


dispatch(State, Http = #http{uri = <<"/notice">>}, Data) ->
    Type = json:get(<<"type">>, Data, 1),
    Title = json:get(<<"title">>, Data, <<>>),
    Content = json:get(<<"content">>, Data, <<>>),
    Items = parser:to_term(json:get(<<"items">>, Data, [])),
    notice_server:add(Type, Title, Content, Items),
    response(State, Http, #{result => <<"ok">>}, 200);


dispatch(State, Http = #http{uri = <<"/mail">>}, Data) ->
    RoleId = json:get(<<"role_id">>, Data, []),
    Title = json:get(<<"title">>, Http = #http{uri = <<>>}, Data),
    Content = json:get(<<"content">>, Http = #http{uri = <<>>}, Data),
    Items = parser:to_term(json:get(<<"items">>, Data, [])),
    mail:send(RoleId, Title, Content, ?MODULE, Items),
    response(State, Http, #{result => <<"ok">>}, 200);


%% server create role control
dispatch(State, Http = #http{uri = <<"/set_server_allow_create">>}, _Data) ->
    user_manager:set_create_state(?TRUE),
    response(State, Http, #{result => <<"ok">>}, 200);

dispatch(State, Http = #http{uri = <<"/set_server_refuse_create">>}, _Data) ->
    user_manager:set_create_state(?FALSE),
    response(State, Http, #{result => <<"ok">>}, 200);


%% server login control
dispatch(State, Http = #http{uri = <<"/set_server_refuse">>}, _Data) ->
    user_manager:set_server_state(?SERVER_STATE_FORBIDDEN),
    response(State, Http, #{result => <<"ok">>}, 200);

dispatch(State, Http = #http{uri = <<"/set_server_normal">>}, _Data) ->
    user_manager:set_server_state(?SERVER_STATE_NORMAL),
    response(State, Http, #{result => <<"ok">>}, 200);

dispatch(State, Http = #http{uri = <<"/set_server_insider">>}, _Data) ->
    user_manager:set_server_state(?SERVER_STATE_INSIDER),
    response(State, Http, #{result => <<"ok">>}, 200);

dispatch(State, Http = #http{uri = <<"/set_server_master">>}, _Data) ->
    user_manager:set_server_state(?SERVER_STATE_MASTER),
    response(State, Http, #{result => <<"ok">>}, 200);


%% server chat control
dispatch(State, Http = #http{uri = <<"/set_server_chat_unlimited">>}, _Data) ->
    user_manager:set_chat_state(?CHAT_STATE_UNLIMITED),
    response(State, Http, #{result => <<"ok">>}, 200);

dispatch(State, Http = #http{uri = <<"/set_server_chat_silent">>}, _Data) ->
    user_manager:set_chat_state(?CHAT_STATE_SILENT),
    response(State, Http, #{result => <<"ok">>}, 200);

dispatch(State, Http = #http{uri = <<"/set_server_chat_silent_world">>}, _Data) ->
    ChatState = user_manager:get_chat_state(),
    user_manager:set_chat_state(ChatState bor ?CHAT_STATE_SILENT_WORLD),
    response(State, Http, #{result => <<"ok">>}, 200);

dispatch(State, Http = #http{uri = <<"/set_server_chat_silent_guild">>}, _Data) ->
    ChatState = user_manager:get_chat_state(),
    user_manager:set_chat_state(ChatState bor ?CHAT_STATE_SILENT_GUILD),
    response(State, Http, #{result => <<"ok">>}, 200);

dispatch(State, Http = #http{uri = <<"/set_server_chat_silent_private">>}, _Data) ->
    ChatState = user_manager:get_chat_state(),
    user_manager:set_chat_state(ChatState bor ?CHAT_STATE_SILENT_PRIVATE),
    response(State, Http, #{result => <<"ok">>}, 200);


%% role login control
dispatch(State, Http = #http{uri = <<"/set_role_refuse">>}, Data) ->
    RoleIdList = json:get(<<"role_id">>, Data, []),
    db:query(parser:format(<<"UPDATE `role` SET `type` = ~w WHERE `role_id` IN (~s)">>, [?SERVER_STATE_FORBIDDEN, parser:join(RoleIdList, <<"~w">>)])),
    [user_server:apply_cast(Id, role, set_type, [?SERVER_STATE_FORBIDDEN]) || Id <- RoleIdList],
    response(State, Http, #{result => <<"ok">>}, 200);

dispatch(State, Http = #http{uri = <<"/set_role_normal">>}, Data) ->
    RoleIdList = json:get(<<"role_id">>, Data, []),
    db:query(parser:format(<<"UPDATE `role` SET `type` = ~w WHERE `role_id` IN (~s)">>, [?SERVER_STATE_NORMAL, parser:join(RoleIdList, <<"~w">>)])),
    [user_server:apply_cast(Id, role, set_type, [?SERVER_STATE_NORMAL]) || Id <- RoleIdList],
    response(State, Http, #{result => <<"ok">>}, 200);

dispatch(State, Http = #http{uri = <<"/set_role_insider">>}, Data) ->
    RoleIdList = json:get(<<"role_id">>, Data, []),
    db:query(parser:format(<<"UPDATE `role` SET `type` = ~w WHERE `role_id` IN (~s)">>, [?SERVER_STATE_INSIDER, parser:join(RoleIdList, <<"~w">>)])),
    [user_server:apply_cast(Id, role, set_type, [?SERVER_STATE_INSIDER]) || Id <- RoleIdList],
    response(State, Http, #{result => <<"ok">>}, 200);

dispatch(State, Http = #http{uri = <<"/set_role_master">>}, Data) ->
    RoleIdList = json:get(<<"role_id">>, Data, []),
    db:query(parser:format(<<"UPDATE `role` SET `type` = ~w WHERE `role_id` IN (~s)">>, [?SERVER_STATE_MASTER, parser:join(RoleIdList, <<"~w">>)])),
    [user_server:apply_cast(Id, role, set_type, [?SERVER_STATE_MASTER]) || Id <- RoleIdList],
    response(State, Http, #{result => <<"ok">>}, 200);


%% role chat control
dispatch(State, Http = #http{uri = <<"/set_role_chat_unlimited">>}, Data) ->
    RoleIdList = json:get(<<"role_id">>, Data, []),
    db:query(parser:format(<<"UPDATE `role` SET `status` = `status` WHERE `role_id` IN (~s)">>, [?CHAT_STATE_UNLIMITED, parser:join(RoleIdList, <<"~w">>)])),
    [user_server:apply_cast(Id, role, set_status, [?CHAT_STATE_UNLIMITED]) || Id <- RoleIdList],
    response(State, Http, #{result => <<"ok">>}, 200);

dispatch(State, Http = #http{uri = <<"/set_role_chat_silent">>}, Data) ->
    RoleIdList = json:get(<<"role_id">>, Data, []),
    db:query(parser:format(<<"UPDATE `role` SET `status` = `status` | ~w WHERE `role_id` IN (~s)">>, [?CHAT_STATE_SILENT, parser:join(RoleIdList, <<"~w">>)])),
    [user_server:apply_cast(Id, role, set_status, [?CHAT_STATE_SILENT]) || Id <- RoleIdList],
    response(State, Http, #{result => <<"ok">>}, 200);

dispatch(State, Http = #http{uri = <<"/set_role_chat_silent_world">>}, Data) ->
    RoleIdList = json:get(<<"role_id">>, Data, []),
    db:query(parser:format(<<"UPDATE `role` SET `status` = `status` | ~w WHERE `role_id` IN (~s)">>, [?CHAT_STATE_SILENT_WORLD, parser:join(RoleIdList, <<"~w">>)])),
    [user_server:apply_cast(Id, role, set_status, [?CHAT_STATE_SILENT_WORLD]) || Id <- RoleIdList],
    response(State, Http, #{result => <<"ok">>}, 200);

dispatch(State, Http = #http{uri = <<"/set_role_chat_silent_guild">>}, Data) ->
    RoleIdList = json:get(<<"role_id">>, Data, []),
    db:query(parser:format(<<"UPDATE `role` SET `status` = `status` | ~w WHERE `role_id` IN (~s)">>, [?CHAT_STATE_SILENT_GUILD, parser:join(RoleIdList, <<"~w">>)])),
    [user_server:apply_cast(Id, role, set_status, [?CHAT_STATE_SILENT_GUILD]) || Id <- RoleIdList],
    response(State, Http, #{result => <<"ok">>}, 200);

dispatch(State, Http = #http{uri = <<"/set_role_chat_silent_private">>}, Data) ->
    RoleIdList = json:get(<<"role_id">>, Data, []),
    db:query(parser:format(<<"UPDATE `role` SET `status` = `status` | ~w WHERE `role_id` IN (~s)">>, [?CHAT_STATE_SILENT_PRIVATE, parser:join(RoleIdList, <<"~w">>)])),
    [user_server:apply_cast(Id, role, set_status, [?CHAT_STATE_SILENT_PRIVATE]) || Id <- RoleIdList],
    response(State, Http, #{result => <<"ok">>}, 200);


%% test
dispatch(State, Http = #http{uri = <<"/test">>}, _Data) ->
    response(State, Http, #{result => <<"test">>}, 200);


%% not found
dispatch(State, Http, _Data) ->
    response(State, Http, #{}, 404).


%% send response package
response(State, #http{ version = Version }, Content, Code) ->
    Body = json:encode(Content),
    Response = <<
        Version/binary, " ", (integer_to_binary(Code))/binary, " ", (list_to_binary(httpd_util:reason_phrase(Code)))/binary, "\r\n",
        "Connection", ":", "keep-alive", "\r\n",
        "Keep-Alive", ":", "timeout=60, max=1000", "\r\n",
        "Date", ":", (list_to_binary(httpd_util:rfc1123_date()))/binary, "\r\n",
        "Server", ":", "erlang/", (list_to_binary(erlang:system_info(version)))/binary, "\r\n",
        "Content-Length", ":", (integer_to_binary(byte_size(Body)))/binary, "\r\n",
        "\r\n",
        Body/binary
    >>,
    sender:send(State, Response).
