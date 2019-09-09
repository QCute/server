%% 拍卖信息表
%% auction =====> auction
-record(auction, {
    unique_id = undefined,                            %% 唯一ID 
    auction_id = 0,                                   %% 拍品ID 
    number = 0,                                       %% 拍品数量 
    type = 0,                                         %% 拍卖类型(1:仙盟拍卖/2:全服拍卖/3:个人拍卖) 
    start_time = 0,                                   %% 开始时间 
    end_time = 0,                                     %% 结束时间 
    from = 0,                                         %% 物品来源 
    bid_number = 0,                                   %% 加价次数 
    price = 0,                                        %% 当前价格 
    seller_list = [],                                 %% 获得收入玩家列表[{玩家id,昵称,服id},|...] 
    bidder_list = [],                                 %% 竞拍者列表[{玩家id,昵称,服id,元宝数量},|...] 
    club_id = 0,                                      %% 帮派id 
    bidder_id = 0,                                    %% 出价者ID 
    bidder_name = <<>>,                               %% 出价者名字 
    bidder_server_id = 0,                             %% 出价者服ID 
    timer = undefined,                                %% 定时器 
    flag = undefined                                  %% 标识(flag) 
}).

%% 拍卖配置表
%% auction_data =====> auction_data
-record(auction_data, {
    auction_id = 0,                                   %% 拍品ID 
    auction_type = 0,                                 %% 竞拍类型(1:竞价/2:一口价) 
    begin_price = 0,                                  %% 底价 
    add_price = 0,                                    %% 每次加价 
    tax = 0,                                          %% 税收 
    show_time = 0,                                    %% 预览时间 
    auction_time = 0,                                 %% 拍卖时间 
    critical_time = 0,                                %% 临界时间(出价加时的临界时间) 
    overtime = 0                                      %% 延迟时间(出价加时的时间) 
}).

