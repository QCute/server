%% 角色充值订单表
%% recharge =====> recharge
-record(recharge, {
    recharge_no = 0,                                  %% 充值编号 
    recharge_id = 0,                                  %% 充值ID 
    account = <<>>,                                   %% 平台账号ID 
    channel_id = 0,                                   %% 渠道ID 
    server_id = 0,                                    %% 区服ID 
    role_id = 0,                                      %% 玩家ID 
    role_name = <<>>,                                 %% 玩家名称 
    money = 0.00,                                     %% 充值金额 
    gold = 0,                                         %% 金币 
    status = 0,                                       %% 状态(0:未发放/1:已发放) 
    time = 0,                                         %% 订单时间 
    receive_time = 0                                  %% 发放时间 
}).

