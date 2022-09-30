
%% 角色充值订单表
%% charge =====> charge
-record(charge, {
    charge_no = 0,                                    %% 充值编号
    charge_id = 0,                                    %% 充值ID
    order_id = <<>>,                                  %% 订单ID
    channel = <<>>,                                   %% 渠道
    role_id = 0,                                      %% 玩家ID
    role_name = <<>>,                                 %% 玩家名称
    money = 0.00,                                     %% 充值金额
    status = 0,                                       %% 状态(update_status)
    time = 0                                          %% 订单时间
}).

%% 充值配置表
%% charge_data =====> charge_data
-record(charge_data, {
    charge_id = 0,                                    %% 充值ID
    type = 0,                                         %% 类型(普通充值:0/购买月卡:1)
    limit = 0,                                        %% 限制数量
    exp = 0,                                          %% 经验
    original_price = 0.00,                            %% 原价
    now_price = 0.00,                                 %% 现价
    gold = 0,                                         %% 金币
    gift_gold = 0,                                    %% 赠送金币
    begin_open_days = 0,                              %% 结束时间，跟开服相关，填天数
    end_open_days = 0,                                %% 结束时间，跟开服相关，填天数
    sort = 0,                                         %% 排序
    icon = <<>>,                                      %% 图片
    name = <<>>,                                      %% 名字
    description = <<>>                                %% 描述
}).

