%% 角色邮件表
%% mail =====> mail
-record(mail, {
    mail_id = undefined,                              %% ID
    sender_id = 0,                                    %% 发送者 
    sender_nick = <<>>,                               %% 发送者昵称 
    receiver_id = 0,                                  %% 接收者(select) 
    receiver_nick = <<>>,                             %% 接受者昵称 
    is_read = 0,                                      %% 是否已经读取(update_read) 
    read_time = 0,                                    %% 读取时间(update_read) 
    receive_time = 0,                                 %% 接收时间 
    valid_time = 0,                                   %% 有效时间 
    from = <<>>,                                      %% 来源 
    title = <<>>,                                     %% 标题 
    content = <<>>,                                   %% 内容 
    attachment = [],                                  %% 附件(convert) 
    flag = <<>>                                       %% 标识(flag)(ignore) 
}).

