
%% 好友状态定义
-define(FRIEND_RELATION_APPLY,                        1). %% 申请
-define(FRIEND_RELATION_FRIEND,                       2). %% 好友
-define(FRIEND_RELATION_BLACK,                        3). %% 拉黑

%% 角色好友表
%% friend =====> friend
-record(friend, {
    role_id = 0,                                      %% 用户ID(select_by_role_id)
    friend_id = 0,                                    %% 好友ID(join(`role`.`role_id`)/join(`vip`.`role_id`))
    friend_name = <<>>,                               %% 好友名字(join(`role`.`role_name`))
    sex = 0,                                          %% 好友性别(join(`role`.`sex`))
    classes = 0,                                      %% 好友职业(join(`role`.`classes`))
    vip_level = 0,                                    %% VIP等级(join(`vip`.`vip_level`))
    is_online = 0,                                    %% 好友在线状态(join(`role`.`is_online`))
    relation = 0,                                     %% 友好状态
    time = 0,                                         %% 时间
    flag = 0                                          %% 标识(flag)
}).

