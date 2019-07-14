%%%-------------------------------------------------------------------
%%% @doc
%%% module notice
%%% @end
%%%-------------------------------------------------------------------
-module(notice).
%% API
-export([make/2, broadcast/2]).
%% Includes
-include("common.hrl").
-include("user.hrl").
-include("role.hrl").
-include("notice.hrl").
-include("protocol.hrl").
%%%===================================================================
%%% API
%%%===================================================================
%% @doc construct notice msg
-spec make(User :: #user{}, Content :: term()) -> {ok, binary()}.
make(User, Content) ->
    format(User, Content).

%% @doc broadcast
-spec broadcast(User :: #user{}, Content :: term()) -> ok.
broadcast(User, Content) ->
    {ok, Data} = make(User, Content),
    user_manager:broadcast(Data),
    ok.
%%%===================================================================
%%% Internal functions
%%%===================================================================
format(#user{}, [game_update]) ->
    Msg = text_data:get(game_update),
    user_router:write(?PROTOCOL_NOTICE, [?NOTICE_WORLD, ?NOTICE_DIALOG, Msg]);
format(#user{role = #role{level = Level}}, [level_upgrade]) ->
    Msg = io_lib:format(text_data:get(level_upgrade), [Level]),
    user_router:write(?PROTOCOL_NOTICE, [?NOTICE_WORLD, ?NOTICE_SCROLL, Msg]);
format(#user{role_name = RoleName}, [guild_create, ClubId, GuildName]) ->
    Msg = io_lib:format(text_data:get(guild_create), [RoleName, ClubId, GuildName]),
    user_router:write(?PROTOCOL_NOTICE, [?NOTICE_WORLD, ?NOTICE_CHAT, Msg]);
format(_, _) ->
    {ok, <<>>}.
