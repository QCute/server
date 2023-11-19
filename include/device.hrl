-ifndef(DEVICE_HRL).
-define(DEVICE_HRL, 'DEVICE_HRL').

%% 角色设备信息表
-record(device, {
    role_id = 0,                                      %% 角色ID
    os = <<>>,                                        %% OS
    name = <<>>,                                      %% 名字
    device_id = <<>>,                                 %% 设备ID
    mac = <<>>,                                       %% Mac地址
    ip = <<>>                                         %% IP地址
}).

-endif.
