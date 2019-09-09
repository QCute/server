%% 角色好友表
%% friend =====> friend
-record(friend, {
    role_id = 0,                                      %% 用户ID(select) 
    friend_id = 0,                                    %% 好友ID(join(`role`.`role_id`)) 
    friend_name = <<>>,                               %% 好友名字(join(`role`.`role_name`)) 
    online = 0,                                       %% 好友在线状态(join(`role`.`online`)/default(0)) 
    state = 0,                                        %% 友好状态(0:申请/1:好友/2:黑名单) 
    time = 0,                                         %% 时间 
    flag = undefined                                  %% 标识(flag) 
}).

