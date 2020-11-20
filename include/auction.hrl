%% 拍卖信息表
%% auction =====> auction
-record(auction, {
    auction_no = 0,                                   %% 拍品编号
    auction_id = 0,                                   %% 拍品ID
    number = 0,                                       %% 拍品数量
    type = 0,                                         %% 拍卖类型(1:全服/2:公会)
    bid_type = 0,                                     %% 竞拍类型(1:竞价/2:一口价)
    start_time = 0,                                   %% 开始时间
    end_time = 0,                                     %% 结束时间
    from = <<>>,                                      %% 物品来源
    bid_number = 0,                                   %% 加价次数
    now_price = 0,                                    %% 当前价格
    next_price = 0,                                   %% 下次出价的价格
    seller_list = [],                                 %% 卖家列表
    bidder_list = [],                                 %% 买家列表
    guild_id = 0,                                     %% 公会ID
    timer = 0,                                        %% 定时器
    flag = 0                                          %% 标识(flag)
}).

%% 拍卖配置表
%% auction_data =====> auction_data
-record(auction_data, {
    auction_id = 0,                                   %% 拍品ID
    bid_type = 0,                                     %% 竞拍类型(1:竞价/2:一口价)
    begin_price = 0,                                  %% 底价
    add_price = 0,                                    %% 每次加价
    tax = 0,                                          %% 税收
    show_time = 0,                                    %% 预览时间
    auction_time = 0,                                 %% 拍卖时间
    critical_time = 0,                                %% 临界时间(出价加时的临界时间)
    overtime = 0                                      %% 延迟时间(出价加时的时间)
}).

%% 拍卖角色表
%% auction_role =====> auction_role
-record(auction_role, {
    auction_no = 0,                                   %% 拍品编号(delete_by_no)
    server_id = 0,                                    %% 服务器ID
    role_id = 0,                                      %% 出价者ID
    role_name = <<>>,                                 %% 出价者名字
    guild_id = 0,                                     %% 出价者公会ID
    guild_name = <<>>,                                %% 出价者公会名字
    type = 0,                                         %% 角色类型(1:卖家/2:买家)
    price = 0,                                        %% 当前价格
    time = 0,                                         %% 时间
    flag = 0                                          %% 标识(flag)
}).

