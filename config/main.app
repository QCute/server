{
    application, main,
    [
        {description, "This is main application server."},
        {vsn, "1.0.0"},
        {modules, [main]},
        {registered, [main]},
        {applications, [kernel, stdlib, sasl]},
        {mod, {main, []}},
        {start_phases, []},
        {env, [
            {database, [                                    %% 数据库配置(游戏数据)
                {host,          "127.0.0.1"},               %% 地址
                {port,          3306},                      %% 端口
                {user,          "root"},                    %% 用户名
                {password,      "root"},                    %% 密码
                {database,      "game"},                    %% 数据库
                {encode,        utf8mb4}                    %% 数据库字符集
            ]},
            {ssl_file,          "fake"},                    %% 安全验证文件, 固定后缀 .crt  .key  自动添加
            {server_no,         0},                         %% 服务器编号
            {gen_tcp_port,      "8974"},                    %% tcp起始端口
            {ssl_port,          "8998"},                    %% ssl起始端口
            {socket_type,       gen_tcp},                   %% gen_tcp/ssl
            {strict_md5,        0},                         %% 是否严格验证 1:验证，0不验证
            {infant_control,    1},                         %% 防沉迷后台控 1不弹窗 2弹窗 3聊天
            {node_prefix,       "erlang"},                  %% node 前缀
            {game_version,      1}                          %% 游戏版本
        ]}
    ]
}.
