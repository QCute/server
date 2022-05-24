-module(notice_sql).
-export([insert/1]).
-export([select/0]).
-export([update/1]).
-export([delete/1]).
-export([delete_in_notice_id/1]).
-include("notice.hrl").

-define(INSERT_NOTICE, <<"INSERT INTO `notice` (`type`, `receive_time`, `expire_time`, `title`, `content`, `attachment`, `from`) VALUES (~i~i~w, ~w, ~w, '~s', '~s', '~w', '~w')">>).
-define(SELECT_NOTICE, <<"SELECT `notice_id`, `type`, `receive_time`, `expire_time`, `title`, `content`, `attachment`, `from` FROM `notice`">>).
-define(UPDATE_NOTICE, {<<"UPDATE `notice` SET ~i~i`type` = ~w, `receive_time` = ~w, `expire_time` = ~w, `title` = '~s', `content` = '~s', `attachment` = '~w', `from` = '~w' ">>, <<"WHERE `notice_id` = ~w">>}).
-define(DELETE_NOTICE, <<"DELETE FROM `notice` WHERE `notice_id` = ~w">>).
-define(DELETE_IN_NOTICE_ID, {<<"DELETE FROM `notice` WHERE `notice_id` in (">>, <<"~w">>, <<")">>}).

%% @doc insert
-spec insert(Notice :: #notice{}) -> InsertIdOrAffectedRows :: non_neg_integer().
insert(Notice) ->
    Sql = parser:format(?INSERT_NOTICE, Notice),
    db:insert(Sql).

%% @doc select
-spec select() -> NoticeList :: [#notice{}].
select() ->
    Sql = parser:format(?SELECT_NOTICE, []),
    Data = db:select(Sql),
    F = fun(Notice = #notice{attachment = Attachment, from = From}) -> Notice#notice{attachment = parser:to_term(Attachment), from = parser:to_term(From)} end,
    parser:convert(Data, notice, F).

%% @doc update
-spec update(Notice :: #notice{}) -> AffectedRows :: non_neg_integer().
update(Notice) ->
    Sql = <<(parser:format(element(1, ?UPDATE_NOTICE), Notice))/binary, (parser:format(element(2, ?UPDATE_NOTICE), [Notice#notice.notice_id]))/binary>>,
    db:update(Sql).

%% @doc delete
-spec delete(NoticeId :: integer()) -> AffectedRows :: non_neg_integer().
delete(NoticeId) ->
    Sql = parser:format(?DELETE_NOTICE, [NoticeId]),
    db:delete(Sql).

%% @doc delete
-spec delete_in_notice_id(NoticeIdList :: [NoticeId :: integer()]) -> AffectedRows :: non_neg_integer().
delete_in_notice_id(NoticeIdList) ->
    Sql = parser:collect(NoticeIdList, ?DELETE_IN_NOTICE_ID),
    db:delete(Sql).

