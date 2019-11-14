%%%------------------------------------------------------------------
%%% @doc
%%% module robot
%%% @end
%%%------------------------------------------------------------------
-module(robot).
-behavior(gen_server).
-compile(nowarn_export_all).
-compile(export_all).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("../../include/asset.hrl").
-include("../../include/common.hrl").
-include("../../include/event.hrl").
-include("../../include/guild.hrl").
-include("../../include/item.hrl").
-include("../../include/key.hrl").
-include("../../include/mail.hrl").
-include("../../include/notice.hrl").
-include("../../include/role.hrl").
-include("../../include/protocol.hrl").
-include("../../include/quest.hrl").
-include("../../include/rank.hrl").
-include("../../include/serialize.hrl").
-include("../../include/socket.hrl").
-include("../../include/sorter.hrl").
-include("../../include/user.hrl").
-include("../../include/vip.hrl").

%%%==================================================================
%%% API functions
%%%==================================================================

send(Id, Code, Data) ->
    user_server:cast(Id, {'socket_event', Code, Data}).

start() ->
    start([]).

start(Args) ->
    ChildSpec = {?MODULE, {?MODULE, start_link, [Args]}, permanent, 60000, worker, [?MODULE]},
    service_supervisor:start_child(ChildSpec).

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

%%%==================================================================
%%% general server
%%%==================================================================
init(_) ->
    process_flag(trap_exit, true),
    erlang:send_after(1000, self(), login),
    erlang:send_after(1000, self(), loop),
    gen_tcp:connect("127.0.0.1", config:net_gen_tcp_port() + config:server_id(), []).
handle_call(_Request, _From, State) ->
    {reply, ok, State}.
handle_cast(_Request, State) ->
    {noreply, State}.
handle_info(login, State) ->
    Id = config:server_id(),
    Data = protocol:pack(?PROTOCOL_ACCOUNT_LOGIN, <<Id:16, 1:16, "1">>),
    gen_tcp:send(State, Data),
    {noreply, State};
handle_info(_Request, State) ->
    io:format("~p~n", [_Request]),
    {noreply, State}.
terminate(_Reason, State) ->
    gen_tcp:close(State),
    {ok, State}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%%%==================================================================
%%% Internal functions
%%%==================================================================
