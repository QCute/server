%%%-------------------------------------------------------------------
%%% @doc
%%% module quest handle
%%% @end
%%%-------------------------------------------------------------------
-module(quest_handle).
%% export API functions
-export([handle/3]).
-include("protocol.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% @doc 接受任务
handle(34567, User, [QuestId]) ->
    quest:accept(User, QuestId);

%% @doc 容错
handle(Code, _, Data) ->
    {error, Code, Data}.


