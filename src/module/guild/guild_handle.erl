%%%-------------------------------------------------------------------
%%% @doc
%%% module guild handle
%%% @end
%%%-------------------------------------------------------------------
-module(guild_handle).
%% API
-export([handle/3]).
%% Includes
-include("protocol.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% @doc 创建帮派
handle(?CMD_GUILD_CREATE, User, [Type, Name]) ->
    case guild_server:create(User, Type, Name) of
        {update, NewUser} ->
            {update, NewUser};
        Error ->
            {reply, Error}
    end;

%% @doc 容错
handle(Protocol, _User, Data) ->
    {error, Protocol, Data}.


