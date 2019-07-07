%% 用户好友
%% friend =====> friend
-record(friend, {
    role_id = 0,                                      %% 用户ID(select) 
    friend_id = 0,                                    %% 好友ID(`role`.`id`) 
    friend_name = <<>>,                               %% 好友名字(ignore)(`role`.`name`) 
    online = <<>>,                                    %% 好友在线状态(ignore)(`role`.`online`) 
    state = 0,                                        %% 友好状态 ,1=>好友 ,2=>黑名单 
    time = 0,                                         %% 时间 
    flag = <<>>                                       %% 标识(flag) 
}).

