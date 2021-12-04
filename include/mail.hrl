%% 角色邮件表
%% mail =====> mail
-record(mail, {
    mail_id = 0,                                      %% 邮件ID
    role_id = 0,                                      %% 角色ID(select_by_role_id)
    receive_time = 0,                                 %% 接收时间
    expire_time = 0,                                  %% 过期时间
    read_time = 0,                                    %% 读取时间(update_read)
    receive_attachment_time = 0,                      %% 领取附件时间(update_receive)
    title = <<>>,                                     %% 标题
    content = <<>>,                                   %% 内容
    attachment = [],                                  %% 附件
    from = [],                                        %% 来源
    flag = 0                                          %% 标识(flag)
}).

