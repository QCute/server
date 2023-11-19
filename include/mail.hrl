-ifndef(MAIL_HRL).
-define(MAIL_HRL, 'MAIL_HRL').

%% 角色邮件表
-record(mail, {
    mail_id = 0,                                      %% 邮件ID
    role_id = 0,                                      %% 角色ID
    receive_time = 0,                                 %% 接收时间
    expire_time = 0,                                  %% 过期时间
    read_time = 0,                                    %% 读取时间
    receive_attachment_time = 0,                      %% 领取附件时间
    title = <<>>,                                     %% 标题
    content = <<>>,                                   %% 内容
    attachment = [],                                  %% 附件
    from = [],                                        %% 来源
    flag = 0                                          %% 标识
}).

-endif.
