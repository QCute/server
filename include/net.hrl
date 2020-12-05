%%%-------------------------------------------------------------------
%%% @doc
%%% socket define
%%% @end
%%%-------------------------------------------------------------------
-define(PACKET_HEADER_LENGTH,                         4).         %% packet header length
-define(TCP_TIMEOUT,                                  60 * 1000). %% receive timeout

%% receiver state
-record(client, {
    socket_type,                                      %% gen_tcp or ssl
    socket,                                           %% socket/port
    reference = 0,                                    %% socket message reference
    ip,                                               %% IP address
    handler,                                          %% receiver handler
    packet = <<>>,                                    %% packet
    protocol_type = tcp,                              %% protocol type, tcp(default) web_socket(Draft-HyBi-00)(not-supported) web_socket(Draft-HiXie-76)
    protocol = 0,                                     %% protocol number
    login_state,                                      %% login state
    role_id = 0,                                      %% role id
    role_pid,                                         %% role Pid
    heartbeat_time = 0,                               %% heartbeat time
    last_time = 0,                                    %% last packet time
    total_packet = 0                                  %% total packet number
}).

%% http content
-record(http, {
    method = <<>>,                                    %% request method
    uri = <<>>,                                       %% request uri
    version = <<>>,                                   %% version
    fields = [],                                      %% header fields [{erlang:decode_packet :: HttpField, binary()}, ...]
    body = <<>>                                       %% body content
}).
