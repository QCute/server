-module(mail_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("mail.hrl").
-define(INSERT_MAIL, <<"INSERT INTO `mail` (`role_id`, `receive_time`, `is_read`, `read_time`, `expire_time`, `is_receive_attachment`, `receive_attachment_time`, `title`, `content`, `attachment`, `from`) VALUES (~w, ~w, ~w, ~w, ~w, ~w, ~w, '~s', '~s', '~w', '~w')">>).
-define(SELECT_MAIL, <<"SELECT `mail_id`, `role_id`, `receive_time`, `is_read`, `read_time`, `expire_time`, `is_receive_attachment`, `receive_attachment_time`, `title`, `content`, `attachment`, `from`, 0 AS `flag` FROM `mail` WHERE `mail_id` = ~w">>).
-define(UPDATE_MAIL, <<"UPDATE `mail` SET `role_id` = ~w, `receive_time` = ~w, `is_read` = ~w, `read_time` = ~w, `expire_time` = ~w, `is_receive_attachment` = ~w, `receive_attachment_time` = ~w, `title` = '~s', `content` = '~s', `attachment` = '~w', `from` = '~w' WHERE `mail_id` = ~w">>).
-define(DELETE_MAIL, <<"DELETE  FROM `mail` WHERE `mail_id` = ~w">>).
-define(INSERT_UPDATE_MAIL, {<<"INSERT INTO `mail` (`mail_id`, `role_id`, `receive_time`, `is_read`, `read_time`, `expire_time`, `is_receive_attachment`, `receive_attachment_time`, `title`, `content`, `attachment`, `from`) VALUES ">>, <<"(~w, ~w, ~w, ~w, ~w, ~w, ~w, ~w, '~s', '~s', '~w', '~w')">>, <<" ON DUPLICATE KEY UPDATE `role_id` = VALUES(`role_id`), `receive_time` = VALUES(`receive_time`), `is_read` = VALUES(`is_read`), `read_time` = VALUES(`read_time`), `expire_time` = VALUES(`expire_time`), `is_receive_attachment` = VALUES(`is_receive_attachment`), `receive_attachment_time` = VALUES(`receive_attachment_time`), `title` = VALUES(`title`), `content` = VALUES(`content`), `attachment` = VALUES(`attachment`), `from` = VALUES(`from`)">>}).
-define(SELECT_BY_ROLE_ID, <<"SELECT `mail_id`, `role_id`, `receive_time`, `is_read`, `read_time`, `expire_time`, `is_receive_attachment`, `receive_attachment_time`, `title`, `content`, `attachment`, `from`, 0 AS `flag` FROM `mail` WHERE `role_id` = ~w">>).
-define(SELECT_JOIN_BY_ROLE_ID, <<"SELECT `mail`.`mail_id`, `mail`.`role_id`, `mail`.`receive_time`, `mail`.`is_read`, `mail`.`read_time`, `mail`.`expire_time`, `mail`.`is_receive_attachment`, `mail`.`receive_attachment_time`, `mail`.`title`, `mail`.`content`, `mail`.`attachment`, `mail`.`from`, IFNULL(`mail`.`flag`, 0) AS `flag` FROM `mail` WHERE `mail`.`role_id` = ~w">>).
-define(UPDATE_READ, <<"UPDATE `mail` SET `read_time` = ~w, `is_read` = ~w WHERE `mail_id` = ~w">>).
-define(UPDATE_RECEIVE, <<"UPDATE `mail` SET `receive_attachment_time` = ~w, `is_receive_attachment` = ~w WHERE `mail_id` = ~w">>).
-define(DELETE_IN_MAIL_ID, {<<"DELETE  FROM `mail` WHERE `mail_id` in (">>, <<"~w">>, <<")">>}).

%% @doc insert
insert(Mail) ->
    Sql = parser:format(?INSERT_MAIL, [
        Mail#mail.role_id,
        Mail#mail.receive_time,
        Mail#mail.is_read,
        Mail#mail.read_time,
        Mail#mail.expire_time,
        Mail#mail.is_receive_attachment,
        Mail#mail.receive_attachment_time,
        Mail#mail.title,
        Mail#mail.content,
        Mail#mail.attachment,
        Mail#mail.from
    ]),
    db:insert(Sql).

%% @doc select
select(MailId) ->
    Sql = parser:format(?SELECT_MAIL, [MailId]),
    Data = db:select(Sql),
    F = fun(Mail = #mail{attachment = Attachment, from = From}) -> Mail#mail{attachment = parser:to_term(Attachment), from = parser:to_term(From)} end,
    parser:convert(Data, mail, F).

%% @doc update
update(Mail) ->
    Sql = parser:format(?UPDATE_MAIL, [
        Mail#mail.role_id,
        Mail#mail.receive_time,
        Mail#mail.is_read,
        Mail#mail.read_time,
        Mail#mail.expire_time,
        Mail#mail.is_receive_attachment,
        Mail#mail.receive_attachment_time,
        Mail#mail.title,
        Mail#mail.content,
        Mail#mail.attachment,
        Mail#mail.from,
        Mail#mail.mail_id
    ]),
    db:update(Sql).

%% @doc delete
delete(MailId) ->
    Sql = parser:format(?DELETE_MAIL, [MailId]),
    db:delete(Sql).


%% @doc insert_update
insert_update(Data) ->
    F = fun(Mail) -> [
        Mail#mail.mail_id,
        Mail#mail.role_id,
        Mail#mail.receive_time,
        Mail#mail.is_read,
        Mail#mail.read_time,
        Mail#mail.expire_time,
        Mail#mail.is_receive_attachment,
        Mail#mail.receive_attachment_time,
        Mail#mail.title,
        Mail#mail.content,
        Mail#mail.attachment,
        Mail#mail.from
    ] end,
    {Sql, NewData} = parser:collect_into(Data, F, ?INSERT_UPDATE_MAIL, #mail.flag),
    db:insert(Sql),
    NewData.

%% @doc select
select_by_role_id(RoleId) ->
    Sql = parser:format(?SELECT_BY_ROLE_ID, [RoleId]),
    Data = db:select(Sql),
    F = fun(Mail = #mail{attachment = Attachment, from = From}) -> Mail#mail{attachment = parser:to_term(Attachment), from = parser:to_term(From)} end,
    parser:convert(Data, mail, F).

%% @doc update
update_read(ThisReadTime, ThisIsRead, MailId) ->
    Sql = parser:format(?UPDATE_READ, [ThisReadTime, ThisIsRead, MailId]),
    db:update(Sql).

%% @doc update
update_receive(ThisReceiveAttachmentTime, ThisIsReceiveAttachment, MailId) ->
    Sql = parser:format(?UPDATE_RECEIVE, [ThisReceiveAttachmentTime, ThisIsReceiveAttachment, MailId]),
    db:update(Sql).

%% @doc delete
delete_in_mail_id(MailIdList) ->
    F = fun(MailId) -> [MailId] end,
    Sql = parser:collect(MailIdList, F, ?DELETE_IN_MAIL_ID),
    db:delete(Sql).

