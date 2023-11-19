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
-include("permission.hrl").
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
                    Json = json:decode(Body),
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

%% server create role control
dispatch(State, Http = #http{uri = <<"/server/create/allow">>}, _) ->
    user_manager:set_create_state(?TRUE),
    response(State, Http, #{result => <<"ok">>}, 200);

dispatch(State, Http = #http{uri = <<"/server/create/block">>}, _) ->
    user_manager:set_create_state(?FALSE),
    response(State, Http, #{result => <<"ok">>}, 200);


%%%===================================================================
%%% server login control
%%%===================================================================

dispatch(State, Http = #http{uri = <<"/server/login/ban">>}, _) ->
    user_manager:set_server_state(?SERVER_STATE_BAN),
    response(State, Http, #{result => <<"ok">>}, 200);

dispatch(State, Http = #http{uri = <<"/server/login/normal">>}, _) ->
    user_manager:set_server_state(?SERVER_STATE_NORMAL),
    response(State, Http, #{result => <<"ok">>}, 200);

dispatch(State, Http = #http{uri = <<"/server/login/insider">>}, _) ->
    user_manager:set_server_state(?SERVER_STATE_INSIDER),
    response(State, Http, #{result => <<"ok">>}, 200);

dispatch(State, Http = #http{uri = <<"/server/login/master">>}, _) ->
    user_manager:set_server_state(?SERVER_STATE_MASTER),
    response(State, Http, #{result => <<"ok">>}, 200);


%%%===================================================================
%%% server chat control
%%%===================================================================

dispatch(State, Http = #http{uri = <<"/server/chat">>}, Data) ->
    ChatState = #{
        <<"normal">> => ?CHAT_STATE_NORMAL,
        <<"ban">> => ?CHAT_STATE_BAN,
        <<"ban-world">> => ?CHAT_STATE_BAN_WORLD,
        <<"ban-guild">> => ?CHAT_STATE_BAN_GUILD,
        <<"ban-scene">> => ?CHAT_STATE_BAN_SCENE,
        <<"ban-private">> => ?CHAT_STATE_BAN_PRIVATE
    },
    State = lists:foldl(fun(Operation, Acc) -> Acc bor maps:get(Operation, ChatState, 0) end, 0, maps:get(<<"operations">>, Data, [])),
    user_manager:set_chat_state(State),
    response(State, Http, #{result => <<"ok">>}, 200);

dispatch(State, Http = #http{uri = <<"/server/chat/normal">>}, _) ->
    user_manager:set_chat_state(?CHAT_STATE_NORMAL),
    response(State, Http, #{result => <<"ok">>}, 200);

dispatch(State, Http = #http{uri = <<"/server/chat/ban">>}, _) ->
    user_manager:set_chat_state(?CHAT_STATE_BAN),
    response(State, Http, #{result => <<"ok">>}, 200);

dispatch(State, Http = #http{uri = <<"/server/chat/ban/world">>}, _) ->
    ChatState = user_manager:get_chat_state(),
    user_manager:set_chat_state(ChatState bor ?CHAT_STATE_BAN_WORLD),
    response(State, Http, #{result => <<"ok">>}, 200);

dispatch(State, Http = #http{uri = <<"/server/chat/ban/guild">>}, _) ->
    ChatState = user_manager:get_chat_state(),
    user_manager:set_chat_state(ChatState bor ?CHAT_STATE_BAN_GUILD),
    response(State, Http, #{result => <<"ok">>}, 200);

dispatch(State, Http = #http{uri = <<"/server/chat/ban/private">>}, _) ->
    ChatState = user_manager:get_chat_state(),
    user_manager:set_chat_state(ChatState bor ?CHAT_STATE_BAN_PRIVATE),
    response(State, Http, #{result => <<"ok">>}, 200);


%%%===================================================================
%%% role login control
%%%===================================================================

dispatch(State, Http = #http{uri = <<"/role/login/ban">>}, Data) ->
    RoleIdList = maps:get(<<"roles">>, Data, []),
    db:query(db:format(<<"UPDATE `role` SET `type` = ? WHERE `role_id` IN (?)">>, [?SERVER_STATE_BAN, db:in(RoleIdList)])),
    [user_server:apply_cast(Id, role, set_type, [?SERVER_STATE_BAN]) || Id <- RoleIdList],
    response(State, Http, #{result => <<"ok">>}, 200);

dispatch(State, Http = #http{uri = <<"/role/login/normal">>}, Data) ->
    RoleIdList = maps:get(<<"roles">>, Data, []),
    db:query(db:format(<<"UPDATE `role` SET `type` = ? WHERE `role_id` IN (?)">>, [?SERVER_STATE_NORMAL, db:in(RoleIdList)])),
    [user_server:apply_cast(Id, role, set_type, [?SERVER_STATE_NORMAL]) || Id <- RoleIdList],
    response(State, Http, #{result => <<"ok">>}, 200);

dispatch(State, Http = #http{uri = <<"/role/login/insider">>}, Data) ->
    RoleIdList = maps:get(<<"roles">>, Data, []),
    db:query(db:format(<<"UPDATE `role` SET `type` = ? WHERE `role_id` IN (?)">>, [?SERVER_STATE_INSIDER, db:in(RoleIdList)])),
    [user_server:apply_cast(Id, role, set_type, [?SERVER_STATE_INSIDER]) || Id <- RoleIdList],
    response(State, Http, #{result => <<"ok">>}, 200);

dispatch(State, Http = #http{uri = <<"/role/login/master">>}, Data) ->
    RoleIdList = maps:get(<<"roles">>, Data, []),
    db:query(db:format(<<"UPDATE `role` SET `type` = ? WHERE `role_id` IN (?)">>, [?SERVER_STATE_MASTER, db:in(RoleIdList)])),
    [user_server:apply_cast(Id, role, set_type, [?SERVER_STATE_MASTER]) || Id <- RoleIdList],
    response(State, Http, #{result => <<"ok">>}, 200);


%%%===================================================================
%%% role chat control
%%%===================================================================

dispatch(State, Http = #http{uri = <<"/role/chat">>}, Data) ->
    ChatState = #{
        <<"normal">> => ?CHAT_STATE_NORMAL,
        <<"ban">> => ?CHAT_STATE_BAN,
        <<"ban-world">> => ?CHAT_STATE_BAN_WORLD,
        <<"ban-guild">> => ?CHAT_STATE_BAN_GUILD,
        <<"ban-scene">> => ?CHAT_STATE_BAN_SCENE,
        <<"ban-private">> => ?CHAT_STATE_BAN_PRIVATE
    },
    State = lists:foldl(fun(Operation, Acc) -> Acc bor maps:get(Operation, ChatState, 0) end, 0, maps:get(<<"operations">>, Data, [])),
    RoleIdList = maps:get(<<"roles">>, Data, []),
    db:query(db:format(<<"UPDATE `role` SET `status` = `status` WHERE `role_id` IN (?)">>, [State, db:in(RoleIdList)])),
    [user_server:apply_cast(Id, role, set_status, [State]) || Id <- RoleIdList],
    response(State, Http, #{result => <<"ok">>}, 200);

dispatch(State, Http = #http{uri = <<"/role/chat/normal">>}, Data) ->
    RoleIdList = maps:get(<<"roles">>, Data, []),
    db:query(db:format(<<"UPDATE `role` SET `status` = `status` WHERE `role_id` IN (?)">>, [?CHAT_STATE_NORMAL, db:in(RoleIdList)])),
    [user_server:apply_cast(Id, role, set_status, [?CHAT_STATE_NORMAL]) || Id <- RoleIdList],
    response(State, Http, #{result => <<"ok">>}, 200);

dispatch(State, Http = #http{uri = <<"/role/chat/ban">>}, Data) ->
    RoleIdList = maps:get(<<"roles">>, Data, []),
    db:query(db:format(<<"UPDATE `role` SET `status` = `status` | ? WHERE `role_id` IN (?)">>, [?CHAT_STATE_BAN, db:in(RoleIdList)])),
    [user_server:apply_cast(Id, role, set_status, [?CHAT_STATE_BAN]) || Id <- RoleIdList],
    response(State, Http, #{result => <<"ok">>}, 200);

dispatch(State, Http = #http{uri = <<"/role/chat/ban/world">>}, Data) ->
    RoleIdList = maps:get(<<"roles">>, Data, []),
    db:query(db:format(<<"UPDATE `role` SET `status` = `status` | ? WHERE `role_id` IN (?)">>, [?CHAT_STATE_BAN_WORLD, db:in(RoleIdList)])),
    [user_server:apply_cast(Id, role, set_status, [?CHAT_STATE_BAN_WORLD]) || Id <- RoleIdList],
    response(State, Http, #{result => <<"ok">>}, 200);

dispatch(State, Http = #http{uri = <<"/role/chat/ban/guild">>}, Data) ->
    RoleIdList = maps:get(<<"roles">>, Data, []),
    db:query(db:format(<<"UPDATE `role` SET `status` = `status` | ? WHERE `role_id` IN (?)">>, [?CHAT_STATE_BAN_GUILD, db:in(RoleIdList)])),
    [user_server:apply_cast(Id, role, set_status, [?CHAT_STATE_BAN_GUILD]) || Id <- RoleIdList],
    response(State, Http, #{result => <<"ok">>}, 200);

dispatch(State, Http = #http{uri = <<"/role/chat/ban/private">>}, Data) ->
    RoleIdList = maps:get(<<"roles">>, Data, []),
    db:query(db:format(<<"UPDATE `role` SET `status` = `status` | ? WHERE `role_id` IN (?)">>, [?CHAT_STATE_BAN_PRIVATE, db:in(RoleIdList)])),
    [user_server:apply_cast(Id, role, set_status, [?CHAT_STATE_BAN_PRIVATE]) || Id <- RoleIdList],
    response(State, Http, #{result => <<"ok">>}, 200);


%%%===================================================================
%%% admin operation control
%%%===================================================================

%% charge event notify
dispatch(State, Http = #http{uri = <<"/charge">>}, Data) ->
    RoleId = maps:get(<<"role_id">>, Data, 0),
    ChargeNo = maps:get(<<"charge_no">>, Data, 0),
    user_server:apply_cast(RoleId, charge, callback, [ChargeNo]),
    response(State, Http, #{result => <<"ok">>}, 200);

%% admin notice notify
dispatch(State, Http = #http{uri = <<"/notice">>}, Data) ->
    Type = maps:get(<<"type">>, Data, 1),
    Title = maps:get(<<"title">>, Data, <<>>),
    Content = maps:get(<<"content">>, Data, <<>>),
    Items = [{Id, Number} || #{id := Id, number := Number} <- maps:get(<<"items">>, Data, [])],
    notice_server:add(Type, Title, Content, Items),
    response(State, Http, #{result => <<"ok">>}, 200);

%% admin mail notify
dispatch(State, Http = #http{uri = <<"/mail">>}, Data) ->
    RoleId = maps:get(<<"roles">>, Data, []),
    Title = maps:get(<<"title">>, Http = #http{uri = <<>>}, Data),
    Content = maps:get(<<"content">>, Http = #http{uri = <<>>}, Data),
    Items = [{Id, Number} || #{id := Id, number := Number} <- maps:get(<<"items">>, Data, [])],
    mail:send(RoleId, Title, Content, ?MODULE, Items),
    response(State, Http, #{result => <<"ok">>}, 200);


%%%===================================================================
%%% json rpc test
%%%===================================================================

dispatch(State, Http = #http{uri = <<"/test">>}, Data) ->
    response(State, Http, Data, 200);


%% not found
dispatch(State, Http, _) ->
    response(State, Http, #{}, 404).

%%%===================================================================
%%% HTTP Response
%%%===================================================================

%% send response package
response(State, #http{version = Version}, Content, Code) ->
    Body = unicode:characters_to_binary(json:encode(Content)),
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
