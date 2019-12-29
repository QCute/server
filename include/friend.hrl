%% 角色好友表
%% friend =====> friend
-record(friend, {
    role_id = 0,                                      %% 用户ID(select) 
    friend_id = 0,                                    %% 好友ID(join(`role`.`role_id`)/join(`vip`.`role_id`)) 
    friend_name = <<>>,                               %% 好友名字(join(`role`.`role_name`)) 
    sex = 0,                                          %% 好友性别(join(`role`.`sex`)/default(0)) 
    classes = 0,                                      %% 好友职业(join(`role`.`classes`)/default(0)) 
    vip_level = 0,                                    %% VIP等级(join(`vip`.`vip_level`)/default(0)) 
    online = 0,                                       %% 好友在线状态(join(`role`.`online`)/default(0)) 
    relation = 0,                                     %% 友好状态(0:申请/1:好友/2:黑名单) 
    time = 0,                                         %% 时间 
    flag = 0                                          %% 标识(flag) 
}).

