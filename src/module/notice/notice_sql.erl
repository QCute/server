-module(notice_sql).
-export([insert/1]).
-export([select/0]).
-export([delete_in_notice_id/1]).
-include("notice.hrl").

%% @doc insert into notice
-spec insert(Notice :: #notice{}) -> InsertIdOrAffectedRows :: non_neg_integer().
insert(#notice{notice_id = NoticeId, type = Type, receive_time = ReceiveTime, expire_time = ExpireTime, title = Title, content = Content, attachment = Attachment, from = From}) ->
    db:insert(<<"INSERT INTO `notice` (`notice_id`, `type`, `receive_time`, `expire_time`, `title`, `content`, `attachment`, `from`) VALUES (?, ?, ?, ?, ?, ?, ?, ?)">>, [NoticeId, Type, ReceiveTime, ExpireTime, Title, Content, Attachment, From]).

%% @doc select from notice
-spec select() -> Rows :: [#notice{}].
select() ->
    Data = db:select(<<"SELECT `notice_id`, `type`, `receive_time`, `expire_time`, `title`, `content`, `attachment`, `from` FROM `notice`">>, []),
    parser:convert(Data, notice).

%% @doc delete row from notice
-spec delete_in_notice_id(NoticeId :: non_neg_integer()) -> AffectedRows :: non_neg_integer().
delete_in_notice_id(NoticeId) ->
    db:delete(<<"DELETE FROM `notice` WHERE `notice_id` IN ?">>, [NoticeId]).
