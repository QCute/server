%%%-------------------------------------------------------------------
%%% @doc
%%% module protocol read
%%% @end
%%%-------------------------------------------------------------------
-module(proto_10).
%% export API function
-export([read/2, write/2]).
-include("protocol.hrl").
-define(GM_ID, 1000).

%%%===================================================================
%%% API
%%%===================================================================
%% @doc unpack data
read(?PP_ACCOUNT_CREATE, <<ServerId:16, Sex:8, Career:8, AgentId:16, Binary/binary>>) ->
    [UserName, NickName, Device, Mac, DeviceType] = protocol:read_string(5, Binary),
    {ok, ?PP_ACCOUNT_CREATE, [UserName, ServerId, NickName, Sex, Career, AgentId, Device, Mac, DeviceType]};

read(Protocol, _) ->
    {error, Protocol}.

%% @doc pack data
write(?PP_ACCOUNT_LOGIN, <<ServerId:16, UserId:64, Binary/binary>>) ->
    {[UserName], _} = protocol:read_string(Binary),
    {ok, [ServerId, UserId, UserName]};
write(Protocol, _) ->
    {error, Protocol}.
