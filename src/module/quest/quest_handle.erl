%%%-------------------------------------------------------------------
%%% @doc
%%% module quest handle
%%% @end
%%%-------------------------------------------------------------------
-module(quest_handle).
%% API
-export([handle/3]).
%% Includes
-include("user.hrl").
-include("player.hrl").
-include("quest.hrl").
-include("protocol.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% @doc 任务列表
handle(?CMD_QUEST, #user{quest = Quest}, []) ->
    {reply, Quest};

%% @doc 接受任务
handle(?CMD_QUEST_ACCEPT, User, [QuestId]) ->
    case quest:accept(User, QuestId) of
        {ok, Quest, NewUser} ->
            {reply, [1, Quest], NewUser};
        {error, Code} ->
            {reply, [Code, #quest{}]};
        _ ->
            skip
    end;

%% @doc 提交任务
handle(?CMD_QUEST_SUBMIT, User, [QuestId]) ->
    quest:submit(User, QuestId);

%% @doc 容错
handle(Protocol, _User, Data) ->
    {error, Protocol, Data}.


