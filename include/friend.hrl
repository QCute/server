
%% 角色好友表
%% friend =====> friend
-record(friend, {
    role_id = 0,                                      %% 用户ID(select) 
    friend_id = 0,                                    %% 好友ID,on(`role`.`role_id`) 
    friend_name = <<>>,                               %% 好友名字,on(`role`.`role_name`) 
    online = 0,                                       %% 好友在线状态,on(`role`.`online`),default(0) 
    state = 0,                                        %% 友好状态,(1)=>好友, (2)=>黑名单  
    time = 0,                                         %% 时间 
    flag = undefined                                  %% 标识(flag) 
}).

