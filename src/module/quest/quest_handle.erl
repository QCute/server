%%%-------------------------------------------------------------------
%%% @doc
%%% module quest handle
%%% @end
%%%-------------------------------------------------------------------
-module(quest_handle).
%% export API functions
-export([handle/3]).
-include("player.hrl").
-include("protocol.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% @doc 任务列表
handle(?CMD_QUEST, #user{quest = Quest}, []) ->
    {reply, Quest};

%% @doc 接受任务
handle(?CMD_QUEST_ACCEPT, User, [QuestId]) ->
    quest:accept(User, QuestId);

%% @doc 提交任务
handle(?CMD_QUEST_SUBMIT, User, [QuestId]) ->
    quest:submit(User, QuestId);

%% @doc 容错
handle(Protocol, _User, Data) ->
    {error, Protocol, Data}.


