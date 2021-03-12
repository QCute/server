%%%-------------------------------------------------------------------
%%% @doc
%%% socket define
%%% @end
%%%-------------------------------------------------------------------

%% receiver state
-record(client, {
    socket_type,                                      %% gen_tcp or ssl
    socket,                                           %% socket/port
    reference = 0,                                    %% socket message reference
    ip,                                               %% IP address
    handler,                                          %% receiver handler
    data = <<>>,                                      %% data
    protocol_type = tcp,                              %% protocol type, tcp(default) web socket(Draft-HyBi-10-17) web socket(Draft-HiXie-76)(not supported)
    role_pid,                                         %% role Pid
    heartbeat_time = 0,                               %% heartbeat time
    protocol_interval                                 %% protocol interval
}).

%% http content
-record(http, {
    method = <<>>,                                    %% request method
    uri = <<>>,                                       %% request uri
    version = <<>>,                                   %% version
    fields = [],                                      %% header fields [{erlang:decode_packet :: HttpField, binary()}, ...]
    body = <<>>                                       %% body content
}).
