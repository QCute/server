
%% 好友状态定义
-define(FRIEND_RELATION_APPLY,                        1). %% 申请
-define(FRIEND_RELATION_FRIEND,                       2). %% 好友
-define(FRIEND_RELATION_BLOCK,                        3). %% 拉黑
-define(FRIEND_RELATION_BE_BLOCK,                     4). %% 被拉黑

%% 角色好友表
%% friend =====> friend
-record(friend, {
    role_id = 0,                                      %% 用户ID(select_by_role_id)
    friend_role_id = 0,                               %% 好友角色ID(join_on(`role`.`role_id`)/join_on(`vip`.`role_id`))
    friend_name = <<>>,                               %% 好友名字(join(`role`.`role_name`))
    sex = 0,                                          %% 好友性别(join(`role`.`sex`))
    avatar = 0,                                       %% 头像(join(`role`.`avatar`))
    classes = 0,                                      %% 好友职业(join(`role`.`classes`))
    level = 0,                                        %% 等级(join(`role`.`level`))
    vip_level = 0,                                    %% VIP等级(join(`vip`.`vip_level`))
    is_online = 0,                                    %% 好友在线状态(join(`role`.`is_online`))
    relation = 0,                                     %% 友好状态(update_relation)
    time = 0,                                         %% 时间
    flag = 0                                          %% 标识(flag)
}).

