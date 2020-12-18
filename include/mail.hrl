%% 角色邮件表
%% mail =====> mail
-record(mail, {
    mail_id = 0,                                      %% 邮件ID
    role_id = 0,                                      %% 角色ID(select_by_role_id)
    receive_time = 0,                                 %% 接收时间
    is_read = 0,                                      %% 是否已经读取(update_read)
    read_time = 0,                                    %% 读取时间(update_read)
    expire_time = 0,                                  %% 过期时间
    is_receive_attachment = 0,                        %% 是否领取附件(update_receive)
    receive_attachment_time = 0,                      %% 领取附件时间(update_receive)
    title = <<>>,                                     %% 标题
    content = <<>>,                                   %% 内容
    attachment = [],                                  %% 附件
    from = [],                                        %% 来源
    flag = 0                                          %% 标识(flag)
}).

