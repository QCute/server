-module(mail_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("mail.hrl").
-define(INSERT_MAIL, <<"INSERT INTO `mail` (`receiver_id`, `receive_time`, `is_read`, `read_time`, `expire_time`, `is_receive_attachment`, `receive_attachment_time`, `from`, `title`, `content`, `attachment`) VALUES (~w, ~w, ~w, ~w, ~w, ~w, ~w, '~w', '~s', '~s', '~w')">>).
-define(SELECT_MAIL, <<"SELECT `mail_id`, `receiver_id`, `receive_time`, `is_read`, `read_time`, `expire_time`, `is_receive_attachment`, `receive_attachment_time`, `from`, `title`, `content`, `attachment`, 0 AS `flag` FROM `mail` WHERE `mail_id` = ~w">>).
-define(UPDATE_MAIL, <<"UPDATE `mail` SET `receiver_id` = ~w, `receive_time` = ~w, `is_read` = ~w, `read_time` = ~w, `expire_time` = ~w, `is_receive_attachment` = ~w, `receive_attachment_time` = ~w, `from` = '~w', `title` = '~s', `content` = '~s', `attachment` = '~w' WHERE `mail_id` = ~w">>).
-define(DELETE_MAIL, <<"DELETE  FROM `mail` WHERE `mail_id` = ~w">>).
-define(INSERT_UPDATE_MAIL, {<<"INSERT INTO `mail` (`mail_id`, `receiver_id`, `receive_time`, `is_read`, `read_time`, `expire_time`, `is_receive_attachment`, `receive_attachment_time`, `from`, `title`, `content`, `attachment`) VALUES ">>, <<"(~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, '~w', '~s', '~s', '~w')">>, <<" ON DUPLICATE KEY UPDATE `receiver_id` = VALUES(`receiver_id`), `receive_time` = VALUES(`receive_time`), `is_read` = VALUES(`is_read`), `read_time` = VALUES(`read_time`), `expire_time` = VALUES(`expire_time`), `is_receive_attachment` = VALUES(`is_receive_attachment`), `receive_attachment_time` = VALUES(`receive_attachment_time`), `from` = VALUES(`from`), `title` = VALUES(`title`), `content` = VALUES(`content`), `attachment` = VALUES(`attachment`)">>}).
-define(SELECT_BY_ROLE_ID, <<"SELECT `mail_id`, `receiver_id`, `receive_time`, `is_read`, `read_time`, `expire_time`, `is_receive_attachment`, `receive_attachment_time`, `from`, `title`, `content`, `attachment`, 0 AS `flag` FROM `mail` WHERE `receiver_id` = ~w">>).
-define(SELECT_JOIN_BY_ROLE_ID, <<"SELECT `mail`.`mail_id`, `mail`.`receiver_id`, `mail`.`receive_time`, `mail`.`is_read`, `mail`.`read_time`, `mail`.`expire_time`, `mail`.`is_receive_attachment`, `mail`.`receive_attachment_time`, `mail`.`from`, `mail`.`title`, `mail`.`content`, `mail`.`attachment`, IFNULL(`mail`.`flag`, 0) AS `flag` FROM `mail` WHERE `mail`.`receiver_id` = ~w">>).
-define(UPDATE_READ, <<"UPDATE `mail` SET `read_time` = ~w, `is_read` = ~w WHERE `mail_id` = ~w">>).
-define(UPDATE_RECEIVE, <<"UPDATE `mail` SET `receive_attachment_time` = ~w, `is_receive_attachment` = ~w WHERE `mail_id` = ~w">>).
-define(DELETE_IN_MAIL_ID, {<<"DELETE  FROM `mail` WHERE `mail_id` in (">>, <<"~w">>, <<")">>}).

%% @doc insert
insert(Mail) ->
    Sql = parser:format(?INSERT_MAIL, [
        Mail#mail.receiver_id,
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
select(MailId) ->
    Sql = parser:format(?SELECT_MAIL, [MailId]),
    Data = sql:select(Sql),
    F = fun(Mail = #mail{from = From, attachment = Attachment}) -> Mail#mail{from = parser:to_term(From), attachment = parser:to_term(Attachment)} end,
    parser:convert(Data, mail, F).

%% @doc update
update(Mail) ->
    Sql = parser:format(?UPDATE_MAIL, [
        Mail#mail.receiver_id,
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
        Mail#mail.receiver_id,
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

%% @doc select
select_by_role_id(ReceiverId) ->
    Sql = parser:format(?SELECT_BY_ROLE_ID, [ReceiverId]),
    Data = sql:select(Sql),
    F = fun(Mail = #mail{from = From, attachment = Attachment}) -> Mail#mail{from = parser:to_term(From), attachment = parser:to_term(Attachment)} end,
    parser:convert(Data, mail, F).

%% @doc update
update_read(ThisReadTime, ThisIsRead, MailId) ->
    Sql = parser:format(?UPDATE_READ, [ThisReadTime, ThisIsRead, MailId]),
    sql:update(Sql).

%% @doc update
update_receive(ThisReceiveAttachmentTime, ThisIsReceiveAttachment, MailId) ->
    Sql = parser:format(?UPDATE_RECEIVE, [ThisReceiveAttachmentTime, ThisIsReceiveAttachment, MailId]),
    sql:update(Sql).

%% @doc delete
delete_in_mail_id(MailIdList) ->
    F = fun(MailId) -> [MailId] end,
    Sql = parser:collect(MailIdList, F, ?DELETE_IN_MAIL_ID),
    sql:delete(Sql).

