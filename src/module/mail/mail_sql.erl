-module(mail_sql).
-export([save/1]).
-export([select/1]).
-export([update_read/1]).
-export([delete/1]).
-export([delete_in_mail_id/1]).
-include("mail.hrl").

%% @doc insert into mail
-spec save(MailList :: [#mail{}] | ets:tab()) -> NewMailList :: [#mail{}].
save(MailList) ->
    db:save(<<"INSERT INTO `mail` (`mail_id`, `role_id`, `receive_time`, `expire_time`, `read_time`, `receive_attachment_time`, `title`, `content`, `attachment`, `from`) VALUES">>, <<"(?, ?, ?, ?, ?, ?, ?, ?, ?, ?)">>, <<"">>, MailList, fun(#mail{mail_id = MailId, role_id = RoleId, receive_time = ReceiveTime, expire_time = ExpireTime, read_time = ReadTime, receive_attachment_time = ReceiveAttachmentTime, title = Title, content = Content, attachment = Attachment, from = From}) -> [MailId, RoleId, ReceiveTime, ExpireTime, ReadTime, ReceiveAttachmentTime, Title, Content, Attachment, From] end, #mail.flag).

%% @doc select from mail
-spec select(RoleId :: non_neg_integer()) -> Rows :: [#mail{}].
select(RoleId) ->
    Data = db:select(<<"SELECT `mail_id`, `role_id`, `receive_time`, `expire_time`, `read_time`, `receive_attachment_time`, `title`, `content`, `attachment`, `from` FROM `mail` WHERE `role_id` = ?">>, [RoleId]),
    parser:convert(Data, mail).

%% @doc update into mail
-spec update_read(#mail{}) -> AffectedRows :: non_neg_integer().
update_read(#mail{read_time = ReadTime, mail_id = MailId}) ->
    db:update(<<"UPDATE `mail` SET `read_time` = ? WHERE `mail_id` = ?">>, [ReadTime, MailId]).

%% @doc delete row from mail
-spec delete(MailId :: non_neg_integer()) -> AffectedRows :: non_neg_integer().
delete(MailId) ->
    db:delete(<<"DELETE FROM `mail` WHERE `mail_id` = ?">>, [MailId]).

%% @doc delete row from mail
-spec delete_in_mail_id(MailId :: non_neg_integer()) -> AffectedRows :: non_neg_integer().
delete_in_mail_id(MailId) ->
    db:delete(<<"DELETE FROM `mail` WHERE `mail_id` IN ?">>, [MailId]).
