-ifndef(PERMISSION_HRL).
-define(PERMISSION_HRL, 'PERMISSION_HRL').

%% 角色权限表
-record(permission, {
    permission_no = 0,                                %% 权限编号
    role_id = 0,                                      %% 角色ID
    type = 0,                                         %% 类型
    status = 0,                                       %% 状态
    begin_time = 0,                                   %% 开始时间
    end_time = 0,                                     %% 结束时间
    time = 0,                                         %% 时间
    reason = <<>>,                                    %% 原因
    remark = <<>>                                     %% 备注
}).

-endif.
