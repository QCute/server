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
-include("player.hrl").
-include("notice.hrl").
-include("protocol.hrl").
%% notice float scroll
%% world scene team guild
%%%===================================================================
%%% API
%%%===================================================================
%% @doc broadcast
make(User, Content) ->
    format(User, Content).

%% @doc broadcast
broadcast(User, Content) ->
    {ok, Data} = make(User, Content),
    player_manager:broadcast(Data),
    ok.
%%%===================================================================
%%% Internal functions
%%%===================================================================
format(#user{}, [game_update]) ->
    Msg = data_text:get(game_update),
    player_route:write(?CMD_NOTICE, [?NOTICE_WORLD, ?NOTICE_DIALOG, Msg]);
format(#user{player = #player{level = Level}}, [level_upgrade]) ->
    Msg = io_lib:format(data_text:get(level_upgrade), [Level]),
    player_route:write(?CMD_NOTICE, [?NOTICE_WORLD, ?NOTICE_SCROLL, Msg]);
format(#user{name = Name}, [guild_create, ClubId, GuildName]) ->
    Msg = io_lib:format(data_text:get(guild_create), [Name, ClubId, GuildName]),
    player_route:write(?CMD_NOTICE, [?NOTICE_WORLD, ?NOTICE_CHAT, Msg]);
format(_, _) ->
    {ok, <<>>}.