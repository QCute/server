%%%-------------------------------------------------------------------
%%% @doc
%%% socket define
%%% @end
%%%-------------------------------------------------------------------

%% receiver state
-record(client, {
    socket_type,                                      %% gen_tcp/ssl
    socket,                                           %% socket/port
    ip,                                               %% IP address
    protocol_type = tcp,                              %% protocol type, tcp(default) web socket(Draft-HyBi-10-17) web socket(Draft-HiXie-76)(not supported)
    role_pid,                                         %% role Pid
    protocol_interval                                 %% protocol interval
}).

%% http content
-record(http, {
    method = <<>>,                                    %% request method
    uri = <<>>,                                       %% request uri
    version = <<>>,                                   %% version
    fields = []                                       %% header fields [{binary(), binary()}, ...]
}).
