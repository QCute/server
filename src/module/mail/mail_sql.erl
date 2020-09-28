-module(mail_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("mail.hrl").
-define(INSERT_MAIL, <<"INSERT INTO `mail` (`sender_id`, `sender_nick`, `receiver_id`, `receiver_nick`, `receive_time`, `is_read`, `read_time`, `expire_time`, `is_receive_attachment`, `receive_attachment_time`, `from`, `title`, `content`, `attachment`) VALUES (~w, '~s', ~w, '~s', ~w, ~w, ~w, ~w, ~w, ~w, '~w', '~s', '~s', '~w')">>).
-define(SELECT_MAIL, <<"SELECT `mail_id`, `sender_id`, `sender_nick`, `receiver_id`, `receiver_nick`, `receive_time`, `is_read`, `read_time`, `expire_time`, `is_receive_attachment`, `receive_attachment_time`, `from`, `title`, `content`, `attachment`, 0 AS `flag` FROM `mail` WHERE `receiver_id` = ~w">>).
-define(UPDATE_MAIL, <<"UPDATE `mail` SET `sender_id` = ~w, `sender_nick` = '~s', `receiver_id` = ~w, `receiver_nick` = '~s', `receive_time` = ~w, `is_read` = ~w, `read_time` = ~w, `expire_time` = ~w, `is_receive_attachment` = ~w, `receive_attachment_time` = ~w, `from` = '~w', `title` = '~s', `content` = '~s', `attachment` = '~w' WHERE `mail_id` = ~w">>).
-define(DELETE_MAIL, <<"DELETE  FROM `mail` WHERE `mail_id` = ~w">>).
-define(INSERT_UPDATE_MAIL, {<<"INSERT INTO `mail` (`mail_id`, `sender_id`, `sender_nick`, `receiver_id`, `receiver_nick`, `receive_time`, `is_read`, `read_time`, `expire_time`, `is_receive_attachment`, `receive_attachment_time`, `from`, `title`, `content`, `attachment`) VALUES ">>, <<"(~w, ~w, '~s', ~w, '~s', ~w, ~w, ~w, ~w, ~w, ~w, '~w', '~s', '~s', '~w')">>, <<" ON DUPLICATE KEY UPDATE `sender_id` = VALUES(`sender_id`), `sender_nick` = VALUES(`sender_nick`), `receiver_id` = VALUES(`receiver_id`), `receiver_nick` = VALUES(`receiver_nick`), `receive_time` = VALUES(`receive_time`), `is_read` = VALUES(`is_read`), `read_time` = VALUES(`read_time`), `expire_time` = VALUES(`expire_time`), `is_receive_attachment` = VALUES(`is_receive_attachment`), `receive_attachment_time` = VALUES(`receive_attachment_time`), `from` = VALUES(`from`), `title` = VALUES(`title`), `content` = VALUES(`content`), `attachment` = VALUES(`attachment`)">>}).
-define(UPDATE_RECEIVE, <<"UPDATE `mail` SET `receive_attachment_time` = ~w, `is_receive_attachment` = ~w WHERE `mail_id` = ~w">>).
-define(UPDATE_READ, <<"UPDATE `mail` SET `read_time` = ~w, `is_read` = ~w WHERE `mail_id` = ~w">>).
-define(DELETE_IN_MAIL_ID, {<<"DELETE  FROM `mail` WHERE `mail_id` in (">>, <<"~w">>, <<")">>}).
-define(TRUNCATE, <<"TRUNCATE TABLE `mail`">>).

%% @doc insert
insert(Mail) ->
    Sql = parser:format(?INSERT_MAIL, [
        Mail#mail.sender_id,
        Mail#mail.sender_nick,
        Mail#mail.receiver_id,
        Mail#mail.receiver_nick,
        Mail#mail.receive_time,
        Mail#mail.is_read,
        Mail#mail.read_time,
        Mail#mail.expire_time,
        Mail#mail.is_receive_attachment,
        Mail#mail.receive_attachment_time,
        Mail#mail.from,
        Mail#mail.title,
        Mail#mail.content,
        Mail#mail.attachment
    ]),
    sql:insert(Sql).

%% @doc select
select(ReceiverId) ->
    Sql = parser:format(?SELECT_MAIL, [ReceiverId]),
    Data = sql:select(Sql),
    F = fun(Mail = #mail{from = From, attachment = Attachment}) -> Mail#mail{from = parser:to_term(From), attachment = parser:to_term(Attachment)} end,
    parser:convert(Data, mail, F).

%% @doc update
update(Mail) ->
    Sql = parser:format(?UPDATE_MAIL, [
        Mail#mail.sender_id,
        Mail#mail.sender_nick,
        Mail#mail.receiver_id,
        Mail#mail.receiver_nick,
        Mail#mail.receive_time,
        Mail#mail.is_read,
        Mail#mail.read_time,
        Mail#mail.expire_time,
        Mail#mail.is_receive_attachment,
        Mail#mail.receive_attachment_time,
        Mail#mail.from,
        Mail#mail.title,
        Mail#mail.content,
        Mail#mail.attachment,
        Mail#mail.mail_id
    ]),
    sql:update(Sql).

%% @doc delete
delete(MailId) ->
    Sql = parser:format(?DELETE_MAIL, [MailId]),
    sql:delete(Sql).


%% @doc insert_update
insert_update(Data) ->
    F = fun(Mail) -> [
        Mail#mail.mail_id,
        Mail#mail.sender_id,
        Mail#mail.sender_nick,
        Mail#mail.receiver_id,
        Mail#mail.receiver_nick,
        Mail#mail.receive_time,
        Mail#mail.is_read,
        Mail#mail.read_time,
        Mail#mail.expire_time,
        Mail#mail.is_receive_attachment,
        Mail#mail.receive_attachment_time,
        Mail#mail.from,
        Mail#mail.title,
        Mail#mail.content,
        Mail#mail.attachment
    ] end,
    {Sql, NewData} = parser:collect_into(Data, F, ?INSERT_UPDATE_MAIL, #mail.flag),
    sql:insert(Sql),
    NewData.

%% @doc update
update_receive(ThisReceiveAttachmentTime, ThisIsReceiveAttachment, MailId) ->
    Sql = parser:format(?UPDATE_RECEIVE, [ThisReceiveAttachmentTime, ThisIsReceiveAttachment, MailId]),
    sql:update(Sql).

%% @doc update
update_read(ThisReadTime, ThisIsRead, MailId) ->
    Sql = parser:format(?UPDATE_READ, [ThisReadTime, ThisIsRead, MailId]),
    sql:update(Sql).

%% @doc delete
delete_in_mail_id(MailIdList) ->
    F = fun(MailId) -> [MailId] end,
    Sql = parser:collect(MailIdList, F, ?DELETE_IN_MAIL_ID),
    sql:delete(Sql).

%% @doc truncate
truncate() ->
    Sql = parser:format(?TRUNCATE, []),
    sql:query(Sql).

