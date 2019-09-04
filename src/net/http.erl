%%%-------------------------------------------------------------------
%%% @doc
%%% module http reader
%%% @end
%%%-------------------------------------------------------------------
-module(http).
%% API
-export([
    handle_request/2,
    get_method/1,
    get_uri/1,
    get_version/1,
    get_header_field/2
]).
%% Includes
-include("socket.hrl").
%% Types
-type header() :: {Method :: binary(), Uri :: binary(), Version :: binary(), Fields :: [{Key :: binary(), Value :: binary()}]}.

%% ====================================================================
%% API functions
%% ====================================================================
%% @doc 处理http请求
-spec handle_request(Data :: binary(), State :: #client{}) -> {read, non_neg_integer(), non_neg_integer(), #client{}} | {stop, term(), #client{}}.
handle_request(Data, State) ->
    %% GET / HTTP/1.1\r\n
    %% POST / HTTP/1.1\r\n
    %% HEAD / HTTP/1.1\r\n
    case parse_header(Data) of
        {<<"HEAD">>, _, Version, _} ->
            Response = [
                <<"HTTP/">>, Version, <<" 200 OK\r\n">>,
                <<"Connection: close\r\n">>,
                <<"Date: ">>, list_to_binary(httpd_util:rfc1123_date()), <<"\r\n">>,
                <<"Server: erlang/">>, list_to_binary(erlang:system_info(version)), <<"\r\n">>,
                <<"\r\n">>
            ],
            sender:response(State, Response),
            {read, ?PACKET_HEAD_LENGTH, ?TCP_TIMEOUT, State};
        HttpHeader ->
            case get_header_field(<<"Upgrade">>, HttpHeader) of
                <<"websocket">> ->
                    %% websocket upgrade
                    web_socket:handle_upgrade(HttpHeader, State);
                _ ->
                    %% other http request
                    {stop, {not_websocket, HttpHeader}, State}
            end
    end.

%% @doc 获取请求方法
-spec get_method(header()) -> binary().
get_method({Method, _Uri, _Version, _Fields}) ->
    Method.

%% @doc 获取路径
-spec get_uri(header()) -> binary().
get_uri({_Method, Uri, _Version, _Fields}) ->
    Uri.

%% @doc 获取版本
-spec get_version(header()) -> binary().
get_version({_Method, _Uri, Version, _Fields}) ->
    Version.

%% @doc 获取协议头内容
-spec get_header_field(Key :: string(), header()) -> binary().
get_header_field(Key, {_Method, _Uri, _Version, Fields}) ->
    element(2, listing:key_find(Key, 1, Fields, {<<>>, <<>>})).

%% ====================================================================
%% Internal functions
%% ====================================================================
%% 解析http头
parse_header(Packet) ->
    {[Method, Uri, Version | _], Rest} = parse_header(Packet, <<>>, []),
    Fields = parse_header_field(Rest, <<>>, <<>>, []),
    {Method, Uri, Version, Fields}.

%% 解析http方法, 资源路径, 版本信息
parse_header(<<"\r\n", Rest/binary>>, Segment, Result) ->
    {lists:reverse([Segment | Result]), Rest};
parse_header(<<"HTTP/", Rest/binary>>, _, Result) ->
    parse_header(Rest, <<>>, Result);
parse_header(<<32:8, Rest/binary>>, Segment, Result) ->
    parse_header(Rest, <<>>, [Segment | Result]);
parse_header(<<Byte:8, Rest/binary>>, Segment, Result) ->
    parse_header(Rest, <<Segment/binary, Byte:8>>, Result).

%% 解析http字段
parse_header_field(<<>>, _, _, Result) ->
    Result;
parse_header_field(<<"\r\n\r\n", Body/binary>>, _, _, Result) ->
    [{<<"Body">>, Body} | Result];
parse_header_field(<<"\r\n", Rest/binary>>, Segment, Key, Result) ->
    parse_header_field(Rest, <<>>, <<>>, [{Key, Segment} | Result]);
parse_header_field(<<":", 32:8, Rest/binary>>, Segment, _, Result) ->
    parse_header_field(Rest, <<>>, Segment, Result);
parse_header_field(<<Byte:8, Rest/binary>>, Segment, Key, Result) ->
    parse_header_field(Rest, <<Segment/binary, Byte:8>>, Key, Result).
