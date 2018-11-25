%%%-------------------------------------------------------------------
%%% @doc
%%% module item handle
%%% @end
%%%-------------------------------------------------------------------
-module(item_handle).
%% export API functions
-export([handle/3]).
-include("player.hrl").
-include("protocol.hrl").

%%%===================================================================
%%% API
%%%===================================================================
handle(?PP_ITEM_INFO, #user{item = Item}, []) ->
    {reply, Item};

%% @doc 容错
handle(_Protocol, _User, _Data) ->
    %% ?WARNING_MSG("handle_account no match_/~p/~p/", [Cmd, Data]),
    {error, "handle item no match"}.

