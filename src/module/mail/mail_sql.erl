-module(mail_sql).
-export([insert/1]).
-export([select/1]).
-export([update/1]).
-export([delete/1]).
-export([insert_update/1]).
-export([select_by_role_id/1]).
-export([update_read/2]).
-export([update_receive/2]).
-export([delete_in_mail_id/1]).
-include("mail.hrl").

-define(INSERT_MAIL, <<"INSERT INTO `mail` (`role_id`, `receive_time`, `expire_time`, `read_time`, `receive_attachment_time`, `title`, `content`, `attachment`, `from`) VALUES (~i~i~w, ~w, ~w, ~w, ~w, '~s', '~s', '~w', '~w'~i)">>).
-define(SELECT_MAIL, <<"SELECT `mail_id`, `role_id`, `receive_time`, `expire_time`, `read_time`, `receive_attachment_time`, `title`, `content`, `attachment`, `from`, 0 AS `flag` FROM `mail` WHERE `mail_id` = ~w">>).
-define(UPDATE_MAIL, {<<"UPDATE `mail` SET ~i~i`role_id` = ~w, `receive_time` = ~w, `expire_time` = ~w, `read_time` = ~w, `receive_attachment_time` = ~w, `title` = '~s', `content` = '~s', `attachment` = '~w', `from` = '~w'~i ">>, <<"WHERE `mail_id` = ~w">>}).
-define(DELETE_MAIL, <<"DELETE FROM `mail` WHERE `mail_id` = ~w">>).
-define(INSERT_UPDATE_MAIL, {<<"INSERT INTO `mail` (`mail_id`, `role_id`, `receive_time`, `expire_time`, `read_time`, `receive_attachment_time`, `title`, `content`, `attachment`, `from`) VALUES ">>, <<"(~i~w, ~w, ~w, ~w, ~w, ~w, '~s', '~s', '~w', '~w'~i)">>, <<" ON DUPLICATE KEY UPDATE `role_id` = VALUES(`role_id`), `receive_time` = VALUES(`receive_time`), `expire_time` = VALUES(`expire_time`), `read_time` = VALUES(`read_time`), `receive_attachment_time` = VALUES(`receive_attachment_time`), `title` = VALUES(`title`), `content` = VALUES(`content`), `attachment` = VALUES(`attachment`), `from` = VALUES(`from`)">>}).
-define(SELECT_BY_ROLE_ID, <<"SELECT `mail_id`, `role_id`, `receive_time`, `expire_time`, `read_time`, `receive_attachment_time`, `title`, `content`, `attachment`, `from`, 0 AS `flag` FROM `mail` WHERE `role_id` = ~w">>).
-define(SELECT_JOIN_BY_ROLE_ID, <<"SELECT `mail`.`mail_id`, `mail`.`role_id`, `mail`.`receive_time`, `mail`.`expire_time`, `mail`.`read_time`, `mail`.`receive_attachment_time`, `mail`.`title`, `mail`.`content`, `mail`.`attachment`, `mail`.`from`, IFNULL(`mail`.`flag`, 0) AS `flag` FROM `mail` WHERE `mail`.`role_id` = ~w">>).
-define(UPDATE_READ, <<"UPDATE `mail` SET `read_time` = ~w WHERE `mail_id` = ~w">>).
-define(UPDATE_RECEIVE, <<"UPDATE `mail` SET `receive_attachment_time` = ~w WHERE `mail_id` = ~w">>).
-define(DELETE_IN_MAIL_ID, {<<"DELETE FROM `mail` WHERE `mail_id` in (">>, <<"~w">>, <<")">>}).

%% @doc insert
-spec insert(Mail :: #mail{}) -> InsertIdOrAffectedRows :: non_neg_integer().
insert(Mail) ->
    Sql = parser:format(?INSERT_MAIL, Mail),
    db:insert(Sql).

%% @doc select
-spec select(MailId :: integer()) -> MailList :: [#mail{}].
select(MailId) ->
    Sql = parser:format(?SELECT_MAIL, [MailId]),
    Data = db:select(Sql),
    F = fun(Mail = #mail{attachment = Attachment, from = From}) -> Mail#mail{attachment = parser:to_term(Attachment), from = parser:to_term(From)} end,
    parser:convert(Data, mail, F).

%% @doc update
-spec update(Mail :: #mail{}) -> AffectedRows :: non_neg_integer().
update(Mail) ->
    Sql = <<(parser:format(element(1, ?UPDATE_MAIL), Mail))/binary, (parser:format(element(2, ?UPDATE_MAIL), [Mail#mail.mail_id]))/binary>>,
    db:update(Sql).

%% @doc delete
-spec delete(MailId :: integer()) -> AffectedRows :: non_neg_integer().
delete(MailId) ->
    Sql = parser:format(?DELETE_MAIL, [MailId]),
    db:delete(Sql).


%% @doc insert_update
-spec insert_update(MailList :: [#mail{}] | ets:tab()) -> NewMailList :: [#mail{}].
insert_update(MailList) ->
    {Sql, NewMailList} = parser:collect_into(MailList, ?INSERT_UPDATE_MAIL, #mail.flag),
    db:insert(Sql),
    NewMailList.

%% @doc select
-spec select_by_role_id(RoleId :: integer()) -> MailList :: [#mail{}].
select_by_role_id(RoleId) ->
    Sql = parser:format(?SELECT_BY_ROLE_ID, [RoleId]),
    Data = db:select(Sql),
    F = fun(Mail = #mail{attachment = Attachment, from = From}) -> Mail#mail{attachment = parser:to_term(Attachment), from = parser:to_term(From)} end,
    parser:convert(Data, mail, F).

%% @doc update
-spec update_read(UpdateReadTime :: integer(), MailId :: integer()) -> non_neg_integer().
update_read(UpdateReadTime, MailId) ->
    Sql = parser:format(?UPDATE_READ, [UpdateReadTime, MailId]),
    db:update(Sql).

%% @doc update
-spec update_receive(UpdateReceiveAttachmentTime :: integer(), MailId :: integer()) -> non_neg_integer().
update_receive(UpdateReceiveAttachmentTime, MailId) ->
    Sql = parser:format(?UPDATE_RECEIVE, [UpdateReceiveAttachmentTime, MailId]),
    db:update(Sql).

%% @doc delete
-spec delete_in_mail_id(MailIdList :: [MailId :: integer()]) -> AffectedRows :: non_neg_integer().
delete_in_mail_id(MailIdList) ->
    Sql = parser:collect(MailIdList, ?DELETE_IN_MAIL_ID),
    db:delete(Sql).

