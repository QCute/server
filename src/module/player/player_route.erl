%%%-------------------------------------------------------------------
%%% @doc
%%% module player protocol routing
%%% @end
%%%-------------------------------------------------------------------
-module(player_route).
-export([read/2, write/2, handle_routing/3]).

%%%===================================================================
%%% API
%%%===================================================================
%% @doc handle packet data
%% 区分数据包
read(Protocol, Binary) ->
    %% 取前面二位区分功能类型
    read(Protocol div 10, Protocol, Binary).

read(10, Protocol, Binary) ->
    proto_10:read(Protocol, Binary);
read(11, Protocol, Binary) ->
    proto_11:read(Protocol, Binary);
read(12, Protocol, Binary) ->
    proto_12:read(Protocol, Binary);
read(13, Protocol, Binary) ->
    proto_13:read(Protocol, Binary);
read(14, Protocol, Binary) ->
    proto_14:read(Protocol, Binary);
read(15, Protocol, Binary) ->
    proto_15:read(Protocol, Binary);
read(16, Protocol, Binary) ->
    proto_16:read(Protocol, Binary);
read(17, Protocol, Binary) ->
    proto_17:read(Protocol, Binary);
read(18, Protocol, Binary) ->
    proto_18:read(Protocol, Binary);
read(19, Protocol, Binary) ->
    proto_19:read(Protocol, Binary);
read(20, Protocol, Binary) ->
    proto_20:read(Protocol, Binary);
read(21, Protocol, Binary) ->
    proto_21:read(Protocol, Binary);
read(22, Protocol, Binary) ->
    proto_22:read(Protocol, Binary);
read(23, Protocol, Binary) ->
    proto_23:read(Protocol, Binary);
read(24, Protocol, Binary) ->
    proto_24:read(Protocol, Binary);
read(25, Protocol, Binary) ->
    proto_25:read(Protocol, Binary);
read(26, Protocol, Binary) ->
    proto_26:read(Protocol, Binary);
read(27, Protocol, Binary) ->
    proto_27:read(Protocol, Binary);
read(28, Protocol, Binary) ->
    proto_28:read(Protocol, Binary);
read(29, Protocol, Binary) ->
    proto_29:read(Protocol, Binary);
read(30, Protocol, Binary) ->
    proto_30:read(Protocol, Binary);
read(31, Protocol, Binary) ->
    proto_31:read(Protocol, Binary);
read(32, Protocol, Binary) ->
    proto_32:read(Protocol, Binary);
read(33, Protocol, Binary) ->
    proto_33:read(Protocol, Binary);
read(34, Protocol, Binary) ->
    proto_34:read(Protocol, Binary);
read(35, Protocol, Binary) ->
    proto_35:read(Protocol, Binary);
read(36, Protocol, Binary) ->
    proto_36:read(Protocol, Binary);
read(37, Protocol, Binary) ->
    proto_37:read(Protocol, Binary);
read(38, Protocol, Binary) ->
    proto_38:read(Protocol, Binary);
read(39, Protocol, Binary) ->
    proto_39:read(Protocol, Binary);
read(40, Protocol, Binary) ->
    proto_40:read(Protocol, Binary);
read(41, Protocol, Binary) ->
    proto_41:read(Protocol, Binary);
read(42, Protocol, Binary) ->
    proto_42:read(Protocol, Binary);
read(43, Protocol, Binary) ->
    proto_43:read(Protocol, Binary);
read(44, Protocol, Binary) ->
    proto_44:read(Protocol, Binary);
read(45, Protocol, Binary) ->
    proto_45:read(Protocol, Binary);
read(46, Protocol, Binary) ->
    proto_46:read(Protocol, Binary);
read(47, Protocol, Binary) ->
    proto_47:read(Protocol, Binary);
read(48, Protocol, Binary) ->
    proto_48:read(Protocol, Binary);
read(49, Protocol, Binary) ->
    proto_49:read(Protocol, Binary);
read(50, Protocol, Binary) ->
    proto_50:read(Protocol, Binary);
read(_, Protocol, _) ->
    Error = io_lib:format("Routing failure: ~p~n", [Protocol]),
    {error, Error}.

%% @doc handle packet data
%% 区分数据包
write(Protocol, Binary) ->
    %% 取前面二位区分功能类型
    write(Protocol div 10, Protocol, Binary).

write(10, Protocol, Binary) ->       
    proto_10:write(Protocol, Binary);
write(11, Protocol, Binary) ->       
    proto_11:write(Protocol, Binary);
write(12, Protocol, Binary) ->       
    proto_12:write(Protocol, Binary);
write(13, Protocol, Binary) ->       
    proto_13:write(Protocol, Binary);
write(14, Protocol, Binary) ->       
    proto_14:write(Protocol, Binary);
write(15, Protocol, Binary) ->       
    proto_15:write(Protocol, Binary);
write(16, Protocol, Binary) ->       
    proto_16:write(Protocol, Binary);
write(17, Protocol, Binary) ->       
    proto_17:write(Protocol, Binary);
write(18, Protocol, Binary) ->       
    proto_18:write(Protocol, Binary);
write(19, Protocol, Binary) ->       
    proto_19:write(Protocol, Binary);
write(20, Protocol, Binary) ->       
    proto_20:write(Protocol, Binary);
write(21, Protocol, Binary) ->       
    proto_21:write(Protocol, Binary);
write(22, Protocol, Binary) ->       
    proto_22:write(Protocol, Binary);
write(23, Protocol, Binary) ->       
    proto_23:write(Protocol, Binary);
write(24, Protocol, Binary) ->       
    proto_24:write(Protocol, Binary);
write(25, Protocol, Binary) ->       
    proto_25:write(Protocol, Binary);
write(26, Protocol, Binary) ->       
    proto_26:write(Protocol, Binary);
write(27, Protocol, Binary) ->       
    proto_27:write(Protocol, Binary);
write(28, Protocol, Binary) ->       
    proto_28:write(Protocol, Binary);
write(29, Protocol, Binary) ->       
    proto_29:write(Protocol, Binary);
write(30, Protocol, Binary) ->       
    proto_30:write(Protocol, Binary);
write(31, Protocol, Binary) ->       
    proto_31:write(Protocol, Binary);
write(32, Protocol, Binary) ->       
    proto_32:write(Protocol, Binary);
write(33, Protocol, Binary) ->       
    proto_33:write(Protocol, Binary);
write(34, Protocol, Binary) ->       
    proto_34:write(Protocol, Binary);
write(35, Protocol, Binary) ->       
    proto_35:write(Protocol, Binary);
write(36, Protocol, Binary) ->       
    proto_36:write(Protocol, Binary);
write(37, Protocol, Binary) ->       
    proto_37:write(Protocol, Binary);
write(38, Protocol, Binary) ->       
    proto_38:write(Protocol, Binary);
write(39, Protocol, Binary) ->       
    proto_39:write(Protocol, Binary);
write(40, Protocol, Binary) ->       
    proto_40:write(Protocol, Binary);
write(41, Protocol, Binary) ->       
    proto_41:write(Protocol, Binary);
write(42, Protocol, Binary) ->       
    proto_42:write(Protocol, Binary);
write(43, Protocol, Binary) ->       
    proto_43:write(Protocol, Binary);
write(44, Protocol, Binary) ->       
    proto_44:write(Protocol, Binary);
write(45, Protocol, Binary) ->       
    proto_45:write(Protocol, Binary);
write(46, Protocol, Binary) ->       
    proto_46:write(Protocol, Binary);
write(47, Protocol, Binary) ->       
    proto_47:write(Protocol, Binary);
write(48, Protocol, Binary) ->       
    proto_48:write(Protocol, Binary);
write(49, Protocol, Binary) ->       
    proto_49:write(Protocol, Binary);
write(50, Protocol, Binary) ->       
    proto_50:write(Protocol, Binary);
write(_, Protocol, _) ->
    Error = io_lib:format("Routing failure: ~p~n", [Protocol]),
    {error, Error}.

%% @doc protocol routing
%% 路由
handle_routing(Player, Protocol, Data) ->
    %% 取前面二位区分功能类型
    case Protocol div 1000 of
        11 ->
            player_handle:handle(Player, Protocol, Data);
        12 ->
            item_handle:handle(Player, Protocol, Data);
        _ ->
            {error, "Routing failure"}
    end.
%%%===================================================================
%%% Internal functions
%%%===================================================================


