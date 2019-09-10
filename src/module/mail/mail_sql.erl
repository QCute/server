-module(mail_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("mail.hrl").
-define(INSERT_MAIL, <<"INSERT INTO `mail` (`sender_id`, `sender_nick`, `receiver_id`, `receiver_nick`, `is_read`, `read_time`, `receive_time`, `valid_time`, `from`, `title`, `content`, `attachment`) VALUES ('~w', '~w', '~w', '~w', '~w', '~w', '~w', '~w', '~w', '~w', '~w', '~w')">>).
-define(SELECT_MAIL, <<"SELECT * FROM `mail` WHERE `mail_id` = '~w'">>).
-define(UPDATE_MAIL, <<"UPDATE `mail` SET `sender_id` = '~w', `sender_nick` = '~w', `receiver_id` = '~w', `receiver_nick` = '~w', `is_read` = '~w', `read_time` = '~w', `receive_time` = '~w', `valid_time` = '~w', `from` = '~w', `title` = '~w', `content` = '~w', `attachment` = '~w' WHERE `mail_id` = '~w'">>).
-define(DELETE_MAIL, <<"DELETE  FROM `mail` WHERE `mail_id` = '~w'">>).
-define(INSERT_UPDATE_MAIL, {<<"INSERT INTO `mail` (`sender_id`, `sender_nick`, `receiver_id`, `receiver_nick`, `is_read`, `read_time`, `receive_time`, `valid_time`, `from`, `title`, `content`, `attachment`) VALUES ">>, <<"('~w', '~w', '~w', '~w', '~w', '~w', '~w', '~w', '~w', '~w', '~w', '~w')">>, <<" ON DUPLICATE KEY UPDATE `sender_id` = '~w', `sender_nick` = '~w', `receiver_id` = '~w', `receiver_nick` = '~w', `is_read` = '~w', `read_time` = '~w', `receive_time` = '~w', `valid_time` = '~w', `from` = '~w', `title` = '~w', `content` = '~w', `attachment` = '~w'">>}).
-define(SELECT_JOIN_MAIL, <<"SELECT `mail`.`mail_id`, `mail`.`sender_id`, `mail`.`sender_nick`, `mail`.`receiver_id`, `mail`.`receiver_nick`, `mail`.`is_read`, `mail`.`read_time`, `mail`.`receive_time`, `mail`.`valid_time`, `mail`.`from`, `mail`.`title`, `mail`.`content`, `mail`.`attachment`, `mail`.`flag` FROM `mail` WHERE `mail`.`mail_id` = '~w'">>).
-define(UPDATE_READ, <<"UPDATE `mail` SET `read_time` = '~w', `is_read` = '~w' WHERE `mail_id` = '~w'">>).
-define(DELETE_IN_MAIL_ID, {<<"DELETE  FROM `mail` WHERE `mail_id` in (">>, <<"'~w'">>, <<")">>}).

%% @doc insert
insert(Mail) ->
    Sql = parser:format(?INSERT_MAIL, [
        Mail#mail.sender_id,
        Mail#mail.sender_nick,
        Mail#mail.receiver_id,
        Mail#mail.receiver_nick,
        Mail#mail.is_read,
        Mail#mail.read_time,
        Mail#mail.receive_time,
        Mail#mail.valid_time,
        Mail#mail.from,
        Mail#mail.title,
        Mail#mail.content,
        Mail#mail.attachment
    ]),
    sql:insert(Sql).

%% @doc select
select(MailId) ->
    Sql = parser:format(?SELECT_MAIL, [MailId]),
    sql:select(Sql).

%% @doc update
update(Mail) ->
    Sql = parser:format(?UPDATE_MAIL, [
        Mail#mail.sender_id,
        Mail#mail.sender_nick,
        Mail#mail.receiver_id,
        Mail#mail.receiver_nick,
        Mail#mail.is_read,
        Mail#mail.read_time,
        Mail#mail.receive_time,
        Mail#mail.valid_time,
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
        Mail#mail.sender_id,
        Mail#mail.sender_nick,
        Mail#mail.receiver_id,
        Mail#mail.receiver_nick,
        Mail#mail.is_read,
        Mail#mail.read_time,
        Mail#mail.receive_time,
        Mail#mail.valid_time,
        Mail#mail.from,
        Mail#mail.title,
        Mail#mail.content,
        Mail#mail.attachment
    ] end,
    {Sql, NewData} = parser:collect_into(Data, F, ?INSERT_UPDATE_MAIL, #mail.flag),
    sql:insert(Sql),
    NewData.

%% @doc select join
select_join(MailId) ->
    Sql = parser:format(?SELECT_JOIN_MAIL, [MailId]),
    sql:select(Sql).

%% @doc update
update_read(ReadTime, IsRead, MailId) ->
    Sql = parser:format(?UPDATE_READ, [ReadTime, IsRead, MailId]),
    sql:update(Sql).

%% @doc delete
delete_in_mail_id(MailIdList) ->
	F = fun(MailId) -> [MailId] end,
    Sql = parser:collect(MailIdList, F, ?DELETE_IN_MAIL_ID),
    sql:delete(Sql).

