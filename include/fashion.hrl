%% 时装表
%% fashion =====> fashion
-record(fashion, {
    player_id = 0,                                    %% 玩家id(select)
    fashion_id = 0,                                   %% 时装id 
    state = 0,                                        %% 时装状态(update_state)(update_time) 
    score = 0,                                        %% 积分(once) 
    point = 0,                                        %% 积分(update_point) 
    expire_time = 0,                                  %% 过期时间(update_time) 
    list = <<>>,                                      %% 列表 
    string = undefined,                               %% string(ignore) 
    extra = undefined                                 %% extra(ignore)
}).

