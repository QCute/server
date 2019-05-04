-module(mail_sql).
-compile(nowarn_export_all).
-compile(export_all).
-include("mail.hrl").

-define(UPDATE_INTO_MAIL, {"INSERT INTO `mail` (`id`, `sender_id`, `sender_nick`, `receiver_id`, `receiver_nick`, `is_read`, `read_time`, `receive_time`, `valid_time`, `from`, `title`, `content`, `attachment`) VALUES ", "('~w', '~w', '~s', '~w', '~s', '~w', '~w', '~w', '~w', '~s', '~s', '~s', '~w')", " ON DUPLICATE KEY UPDATE `sender_id` = VALUES(`sender_id`), `sender_nick` = VALUES(`sender_nick`), `receiver_id` = VALUES(`receiver_id`), `receiver_nick` = VALUES(`receiver_nick`), `is_read` = VALUES(`is_read`), `read_time` = VALUES(`read_time`), `receive_time` = VALUES(`receive_time`), `valid_time` = VALUES(`valid_time`), `from` = VALUES(`from`), `title` = VALUES(`title`), `content` = VALUES(`content`), `attachment` = VALUES(`attachment`)"}).
-define(INSERT_MAIL, "INSERT INTO `mail` (`sender_id`, `sender_nick`, `receiver_id`, `receiver_nick`, `is_read`, `read_time`, `receive_time`, `valid_time`, `from`, `title`, `content`, `attachment`) VALUES ('~w', '~s', '~w', '~s', '~w', '~w', '~w', '~w', '~s', '~s', '~s', '~w')").
-define(UPDATE_MAIL, "UPDATE `mail` SET `sender_id` = '~w', `sender_nick` = '~s', `receiver_id` = '~w', `receiver_nick` = '~s', `is_read` = '~w', `read_time` = '~w', `receive_time` = '~w', `valid_time` = '~w', `from` = '~s', `title` = '~s', `content` = '~s', `attachment` = '~w' WHERE `id` = '~w'").
-define(SELECT_MAIL, "SELECT * FROM `mail` WHERE `receiver_id` = '~w'").
-define(DELETE_MAIL, "DELETE  FROM `mail` WHERE `id` = '~w'").
-define(UPDATE_MAIL_READ, "UPDATE `mail` SET `read_time` = '~w', `is_read` = '~w' WHERE `id` = '~w'").

%% @doc update_into
update_into(DataList) ->
    F = fun(Mail) -> [
        Mail#mail.id,
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
    {Sql, NewData} = parser:collect(DataList, F, ?UPDATE_INTO_MAIL, #mail.flag),
    sql:insert(Sql),
    NewData.


%% @doc insert
insert(Mail) ->
    Sql = io_lib:format(?INSERT_MAIL, [
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

%% @doc update
update(Mail) ->
    Sql = io_lib:format(?UPDATE_MAIL, [
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
        Mail#mail.id
    ]),
    sql:update(Sql).

%% @doc select
select(ReceiverId) ->
    Sql = io_lib:format(?SELECT_MAIL, [
        ReceiverId
    ]),
    sql:select(Sql).

%% @doc delete
delete(Id) ->
    Sql = io_lib:format(?DELETE_MAIL, [
        Id
    ]),
    sql:delete(Sql).

%% @doc update
update_read(ReadTime, IsRead, Id) ->
    Sql = io_lib:format(?UPDATE_MAIL_READ, [
        ReadTime,
        IsRead,
        Id
    ]),
    sql:update(Sql).

