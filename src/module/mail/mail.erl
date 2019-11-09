%%%------------------------------------------------------------------
%%% @doc
%%% module mail
%%% @end
%%%------------------------------------------------------------------
-module(mail).
%% API
-export([load/1, clean/1]).
-export([query/1]).
-export([read/2, receive_attachment/2]).
-export([add/5, send/6]).
%% Includes
-include("common.hrl").
-include("user.hrl").
-include("mail.hrl").
-include("item.hrl").
-include("protocol.hrl").
%% Macros
-define(MAIL_MAX_ITEM,                                10).                  %% 单封邮件物品上限
-define(MAIL_VALID_DATE,                              ?DAY_SECONDS(15)).    %% 邮件有效时间
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc load
-spec load(User :: #user{}) -> NewUser :: #user{}.
load(User = #user{role_id = RoleId}) ->
    Mails = parser:convert(mail_sql:select(RoleId), ?MODULE, fun(M = #mail{attachment = A}) -> M#mail{attachment = parser:to_term(A)} end),
    User#user{mail = Mails}.

%% @doc clean
-spec clean(User :: #user{}) -> NewUser :: #user{}.
clean(User = #user{mail = MailList}) ->
    {Delete, Remain} = lists:splitwith(fun(#mail{is_read = IsRead, is_receive_attachment = IsReceiveAttachment, attachment = Attachment}) -> (Attachment == [] andalso IsRead == ?TRUE) orelse (IsReceiveAttachment == ?TRUE) end, MailList),
    mail_sql:delete_in_mail_id(listing:collect(#mail.mail_id, Delete)),
    User#user{mail = Remain}.

%% @doc query
-spec query(User :: #user{}) -> ok().
query(#user{mail = Mail}) ->
    {ok, [Mail]}.

%% @doc read
-spec read(User ::#user{}, MailId :: non_neg_integer()) -> ok().
read(#user{}, MailId) ->
    mail_sql:update_read(time:ts(), ?TRUE, MailId),
    {ok, [1]}.

%% @doc receive attachment
-spec receive_attachment(User ::#user{}, MailId :: non_neg_integer()) -> ok() | error().
receive_attachment(User = #user{mail = Mail}, MailId) ->
    case lists:keyfind(MailId, #mail.mail_id, Mail) of
        #mail{attachment = Attachment} ->
            %% @todo receive item empty grid check strict(now)/permissive(if need)
            [{_, Items}, {_, Bags} | _] = lists:keysort(1, item:data_classify(Attachment)),
            ItemEmpty = item:empty_grid(User, ?ITEM_TYPE_COMMON),
            BagEmpty = item:empty_grid(User, ?ITEM_TYPE_BAG),
            case length(Items) =< ItemEmpty andalso length(Bags) =< BagEmpty of
                true ->
                    mail_sql:update_receive(time:ts(), ?TRUE, MailId),
                    {ok, NewUser} = item:add(User, Items, ?MODULE),
                    {ok, 1, NewUser};
                _ ->
                    {error, 3}
            end;
        _ ->
            {error, 2}
    end.

%% @doc add (sync call)
-spec add(User :: #user{}, Title :: binary(), Content :: binary(), From :: term(), Items :: list()) -> User :: #user{}.
add(User = #user{role_id = RoleId, role_name = RoleName, mail = MailList}, Title, Content, From, Items) ->
    NewMailList = make(RoleId, RoleName, text_data:get(Title), text_data:get(Content), From, Items, []),
    user_sender:send(User, ?PROTOCOL_MAIL, [NewMailList]),
    User#user{mail = NewMailList ++ MailList}.

%% @doc send (async call)
-spec send(RoleId :: non_neg_integer(), Name :: binary(), Title :: binary(), Content :: binary(), From :: term(), Items :: list()) -> ok.
send(RoleId, Name, Title, Content, From, Items) ->
    NewMailList = make(RoleId, Name, text_data:get(Title), text_data:get(Content), From, Items, []),
    %% apply cast (async)
    user_server:apply_cast(RoleId, fun coming/2, [NewMailList]).

%% @doc coming (async send callback)
-spec coming(User :: #user{}, Mails :: list()) -> ok().
coming(User = #user{mail = MailList}, Mails) ->
    user_sender:send(User, ?PROTOCOL_MAIL, [Mails]),
    {ok, User#user{mail = Mails ++ MailList}}.

%%%==================================================================
%%% Internal functions
%%%==================================================================
%% split attachment
make(Receiver, Name, Title, Content, From, Items, Mails) when length(Items) =< ?MAIL_MAX_ITEM ->
    Mail = mail(Receiver, Name, Title, Content, From, Items),
    [Mail| Mails];
make(Receiver, Name, Title, Content, From, [A, B, C, D, E, F, G, H, I, J | Items], Mails) ->
    Mail = mail(Receiver, Name, Title, Content, From, [A, B, C, D, E, F, G, H, I, J]),
    make(Receiver, Name, Title, Content, From, Items, [Mail | Mails]).

%% make a new mail
mail(Receiver, Name, Title, Content, From, Items) ->
    Now = time:ts(),
    Mail = #mail{
        receiver_id = Receiver,
        receiver_nick = Name,
        attachment = Items,
        title = Title,
        content = Content,
        receive_time = Now,
        expire_time = Now + ?MAIL_VALID_DATE,
        from = From
    },
    MailId = mail_sql:insert(Mail),
    Mail#mail{mail_id = MailId}.
