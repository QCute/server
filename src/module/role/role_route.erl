%%%-------------------------------------------------------------------
%%% @doc
%%% module role protocol routing
%%% @end
%%%-------------------------------------------------------------------
-module(role_route).
%% API
-export([read/2, write/2]).
-export([handle_routing/3]).
%% Includes
-include("user.hrl").
%%%===================================================================
%%% API
%%%===================================================================
%% @doc handle packet data
-spec read(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, list()} | {error, non_neg_integer(), binary()}.
read(Protocol, Binary) ->
    %% 取前面二位区分功能类型
    read(Protocol div 1000, Protocol, Binary).

read(10, Protocol, Binary) ->
    protocol_10:read(Protocol, Binary);
read(11, Protocol, Binary) ->
    protocol_11:read(Protocol, Binary);
read(12, Protocol, Binary) ->
    protocol_12:read(Protocol, Binary);
read(13, Protocol, Binary) ->
    protocol_13:read(Protocol, Binary);
read(14, Protocol, Binary) ->
    protocol_14:read(Protocol, Binary);
read(15, Protocol, Binary) ->
    protocol_15:read(Protocol, Binary);
read(16, Protocol, Binary) ->
    protocol_16:read(Protocol, Binary);
read(17, Protocol, Binary) ->
    protocol_17:read(Protocol, Binary);
read(18, Protocol, Binary) ->
    protocol_18:read(Protocol, Binary);
read(19, Protocol, Binary) ->
    protocol_19:read(Protocol, Binary);
read(20, Protocol, Binary) ->
    protocol_20:read(Protocol, Binary);
read(21, Protocol, Binary) ->
    protocol_21:read(Protocol, Binary);
read(22, Protocol, Binary) ->
    protocol_22:read(Protocol, Binary);
read(23, Protocol, Binary) ->
    protocol_23:read(Protocol, Binary);
read(24, Protocol, Binary) ->
    protocol_24:read(Protocol, Binary);
read(25, Protocol, Binary) ->
    protocol_25:read(Protocol, Binary);
read(26, Protocol, Binary) ->
    protocol_26:read(Protocol, Binary);
read(27, Protocol, Binary) ->
    protocol_27:read(Protocol, Binary);
read(28, Protocol, Binary) ->
    protocol_28:read(Protocol, Binary);
read(29, Protocol, Binary) ->
    protocol_29:read(Protocol, Binary);
read(30, Protocol, Binary) ->
    protocol_30:read(Protocol, Binary);
read(31, Protocol, Binary) ->
    protocol_31:read(Protocol, Binary);
read(32, Protocol, Binary) ->
    protocol_32:read(Protocol, Binary);
read(33, Protocol, Binary) ->
    protocol_33:read(Protocol, Binary);
read(34, Protocol, Binary) ->
    protocol_34:read(Protocol, Binary);
read(35, Protocol, Binary) ->
    protocol_35:read(Protocol, Binary);
read(36, Protocol, Binary) ->
    protocol_36:read(Protocol, Binary);
read(37, Protocol, Binary) ->
    protocol_37:read(Protocol, Binary);
read(38, Protocol, Binary) ->
    protocol_38:read(Protocol, Binary);
read(39, Protocol, Binary) ->
    protocol_39:read(Protocol, Binary);
read(40, Protocol, Binary) ->
    protocol_40:read(Protocol, Binary);
read(41, Protocol, Binary) ->
    protocol_41:read(Protocol, Binary);
read(42, Protocol, Binary) ->
    protocol_42:read(Protocol, Binary);
read(43, Protocol, Binary) ->
    protocol_43:read(Protocol, Binary);
read(44, Protocol, Binary) ->
    protocol_44:read(Protocol, Binary);
read(45, Protocol, Binary) ->
    protocol_45:read(Protocol, Binary);
read(46, Protocol, Binary) ->
    protocol_46:read(Protocol, Binary);
read(47, Protocol, Binary) ->
    protocol_47:read(Protocol, Binary);
read(48, Protocol, Binary) ->
    protocol_48:read(Protocol, Binary);
read(49, Protocol, Binary) ->
    protocol_49:read(Protocol, Binary);
read(50, Protocol, Binary) ->
    protocol_50:read(Protocol, Binary);
read(60, Protocol, Data) ->
    protocol_60:read(Protocol, Data);
read(_, Protocol, _) ->
    {error, Protocol}.

%% @doc handle packet data
-spec write(Protocol :: non_neg_integer(), Binary :: binary()) -> {ok, list()} | {error, non_neg_integer(), binary()}.
write(Protocol, Data) ->
    %% 取前面二位区分功能类型
    write(Protocol div 1000, Protocol, Data).

write(10, Protocol, Data) ->       
    protocol_10:write(Protocol, Data);
write(11, Protocol, Data) ->       
    protocol_11:write(Protocol, Data);
write(12, Protocol, Data) ->       
    protocol_12:write(Protocol, Data);
write(13, Protocol, Data) ->       
    protocol_13:write(Protocol, Data);
write(14, Protocol, Data) ->       
    protocol_14:write(Protocol, Data);
write(15, Protocol, Data) ->       
    protocol_15:write(Protocol, Data);
write(16, Protocol, Data) ->       
    protocol_16:write(Protocol, Data);
write(17, Protocol, Data) ->       
    protocol_17:write(Protocol, Data);
write(18, Protocol, Data) ->       
    protocol_18:write(Protocol, Data);
write(19, Protocol, Data) ->       
    protocol_19:write(Protocol, Data);
write(20, Protocol, Data) ->       
    protocol_20:write(Protocol, Data);
write(21, Protocol, Data) ->       
    protocol_21:write(Protocol, Data);
write(22, Protocol, Data) ->       
    protocol_22:write(Protocol, Data);
write(23, Protocol, Data) ->       
    protocol_23:write(Protocol, Data);
write(24, Protocol, Data) ->       
    protocol_24:write(Protocol, Data);
write(25, Protocol, Data) ->       
    protocol_25:write(Protocol, Data);
write(26, Protocol, Data) ->       
    protocol_26:write(Protocol, Data);
write(27, Protocol, Data) ->       
    protocol_27:write(Protocol, Data);
write(28, Protocol, Data) ->       
    protocol_28:write(Protocol, Data);
write(29, Protocol, Data) ->       
    protocol_29:write(Protocol, Data);
write(30, Protocol, Data) ->       
    protocol_30:write(Protocol, Data);
write(31, Protocol, Data) ->       
    protocol_31:write(Protocol, Data);
write(32, Protocol, Data) ->       
    protocol_32:write(Protocol, Data);
write(33, Protocol, Data) ->       
    protocol_33:write(Protocol, Data);
write(34, Protocol, Data) ->       
    protocol_34:write(Protocol, Data);
write(35, Protocol, Data) ->       
    protocol_35:write(Protocol, Data);
write(36, Protocol, Data) ->       
    protocol_36:write(Protocol, Data);
write(37, Protocol, Data) ->       
    protocol_37:write(Protocol, Data);
write(38, Protocol, Data) ->       
    protocol_38:write(Protocol, Data);
write(39, Protocol, Data) ->       
    protocol_39:write(Protocol, Data);
write(40, Protocol, Data) ->       
    protocol_40:write(Protocol, Data);
write(41, Protocol, Data) ->       
    protocol_41:write(Protocol, Data);
write(42, Protocol, Data) ->       
    protocol_42:write(Protocol, Data);
write(43, Protocol, Data) ->       
    protocol_43:write(Protocol, Data);
write(44, Protocol, Data) ->       
    protocol_44:write(Protocol, Data);
write(45, Protocol, Data) ->       
    protocol_45:write(Protocol, Data);
write(46, Protocol, Data) ->       
    protocol_46:write(Protocol, Data);
write(47, Protocol, Data) ->       
    protocol_47:write(Protocol, Data);
write(48, Protocol, Data) ->       
    protocol_48:write(Protocol, Data);
write(49, Protocol, Data) ->       
    protocol_49:write(Protocol, Data);
write(50, Protocol, Data) ->       
    protocol_50:write(Protocol, Data);
write(60, Protocol, Data) ->
    protocol_60:write(Protocol, Data);
write(_, Protocol, _) ->
    {error, Protocol}.

%% @doc protocol routing dispatch
-spec handle_routing(User :: #user{}, Protocol :: non_neg_integer(), Data :: list()) -> Result :: ok | {ok, #user{}} | {reply, term(), #user{}} | {reply, term()} | {error, protocol, non_neg_integer()} | term().
handle_routing(User, Protocol, Data) ->
    %% 取前面二位区分功能类型
    case Protocol div 1000 of
        10 ->
            %% account handle in receiver process
            ok;
        11 ->
            role_handle:handle(Protocol, User, Data);
        12 ->
            item_handle:handle(Protocol, User, Data);
        _ ->
            {error, protocol, Protocol}
    end.
%%%===================================================================
%%% Internal functions
%%%===================================================================


