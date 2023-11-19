-module(mail_sql).
-export([save/1]).
-export([select/1]).
-export([update_read/1]).
-export([update_attachment/1]).
-export([delete/1]).
-export([delete_in_mail_id/1]).
-include("mail.hrl").

%% @doc insert into mail
-spec save(MailList :: [#mail{}] | ets:tab()) -> NewMailList :: [#mail{}].
save(MailList) ->
    db:save_into(<<"INSERT INTO `mail` (`mail_id`, `role_id`, `receive_time`, `expire_time`, `read_time`, `receive_attachment_time`, `title`, `content`, `attachment`, `from`) VALUES">>, <<"(:2:, :3:, :4:, :5:, :6:, :7:, :8:, :9:, :10:, :11:)">>, <<"ON DUPLICATE KEY UPDATE `mail_id` = VALUES(`mail_id`), `role_id` = VALUES(`role_id`), `receive_time` = VALUES(`receive_time`), `expire_time` = VALUES(`expire_time`), `read_time` = VALUES(`read_time`), `receive_attachment_time` = VALUES(`receive_attachment_time`), `title` = VALUES(`title`), `content` = VALUES(`content`), `attachment` = VALUES(`attachment`), `from` = VALUES(`from`)">>, MailList, #mail.flag).

%% @doc select from mail
-spec select(RoleId :: non_neg_integer()) -> Rows :: [#mail{}].
select(RoleId) ->
    Data = db:select(<<"SELECT `mail_id`, `role_id`, `receive_time`, `expire_time`, `read_time`, `receive_attachment_time`, `title`, `content`, `attachment`, `from`, `flag` FROM `mail` WHERE `role_id` = ?">>, [RoleId]),
    parser:convert(Data, mail, fun(Mail = #mail{attachment = Attachment, from = From}) -> Mail#mail{attachment = parser:to_term(Attachment), from = parser:to_term(From)} end).

%% @doc update into mail
-spec update_read(Mail :: #mail{}) -> AffectedRows :: non_neg_integer().
update_read(Mail) ->
    db:update(<<"UPDATE `mail` SET `read_time` = :6: WHERE `mail_id` = :2:">>, Mail).

%% @doc update into mail
-spec update_attachment(Mail :: #mail{}) -> AffectedRows :: non_neg_integer().
update_attachment(Mail) ->
    db:update(<<"UPDATE `mail` SET `receive_attachment_time` = :7: WHERE `mail_id` = :2:">>, Mail).

%% @doc delete row from mail
-spec delete(MailId :: non_neg_integer()) -> AffectedRows :: non_neg_integer().
delete(MailId) ->
    db:delete(<<"DELETE FROM `mail` WHERE `mail_id` = ?">>, [MailId]).

%% @doc delete row from mail
-spec delete_in_mail_id(MailId :: non_neg_integer()) -> AffectedRows :: non_neg_integer().
delete_in_mail_id(MailId) ->
    db:delete(<<"DELETE FROM `mail` WHERE `mail_id` IN (?)">>, [MailId]).
