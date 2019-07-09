%%%-------------------------------------------------------------------
%%% @doc
%%% module mail
%%% @end
%%%-------------------------------------------------------------------
-module(mail).
%% API
-export([load/1]).
-export([read/2, receive_attachment/2]).
-export([add/5, send/6, coming/2]).
%% Includes
-include("user.hrl").
-include("common.hrl").
-include("role.hrl").
-include("mail.hrl").
-include("item.hrl").
-include("protocol.hrl").
%% Macros
-define(MAIL_MAX_ITEM,                                10).                  %% 单封邮件物品上限
-define(MAIL_VALID_DATE,                              ?DAY_SECONDS * 15).   %% 有效时间
%%%===================================================================
%%% API
%%%===================================================================
%% @doc load user items
-spec load(User :: #user{}) -> NewUser :: #user{}.
load(User = #user{role_id = RoleId}) ->
    Data = mail_sql:select(RoleId),
    Mails = parser:convert(Data, mail, fun(M = #mail{attachment = A}) -> M#mail{attachment = parser:string_to_term(A)} end),
    User#user{mail = Mails}.

%% @doc read
-spec read(User ::#user{}, MailId :: non_neg_integer()) -> ok.
read(#user{role_id = RoleId}, MailId) ->
    mail_sql:update_read(1, MailId, RoleId),
    ok.

%% @doc receive attachment
-spec receive_attachment(User ::#user{}, MailId :: non_neg_integer()) -> {ok, #user{}} | {error, non_neg_integer()}.
receive_attachment(User = #user{mail = Mail}, MailId) ->
    case lists:keyfind(MailId, #mail.mail_id, Mail) of
        #mail{attachment = Attachment} ->
            %% @todo receive item empty grid check strict(now)/permissive(if need)
            [Items, Equipments | _] = item:data_classify(Attachment),
            ItemEmpty = item:empty_grid(User, ?ITEM_TYPE_COMMON),
            BagEmpty = item:empty_grid(User, ?ITEM_TYPE_EQUIPMENT),
            case length(Items) =< ItemEmpty andalso length(Equipments) =< BagEmpty of
                true ->
                    item:add(User, Items);
                _ ->
                    {error, 3}
            end;
        _ ->
            {error, 2}
    end.


%% @doc add (sync call)
-spec add(User :: #user{}, Title :: binary(), Content :: binary(), From :: term(), Items :: list()) -> User :: #user{}.
add(User = #user{role_id = RoleId, role_name = RoleName, mail = MailList}, Title, Content, From, Items) ->
    Mails = make(RoleId, RoleName, Title, Content, From, Items, []),
    user_sender:send(User, ?CMD_MAIL, [Mails]),
    User#user{mail = Mails ++ MailList}.

%% @doc send (async call)
-spec send(RoleId :: non_neg_integer(), Name :: binary(), Title :: binary(), Content :: binary(), From :: term(), Items :: list()) -> ok.
send(RoleId, Name, Title, Content, From, Items) ->
    Mails = make(RoleId, Name, Title, Content, From, Items, []),
    %% apply cast (async)
    user_server:apply_cast(RoleId, ?MODULE, coming, [Mails]),
    ok.

%% @doc coming (async send callback)
-spec coming(User :: #user{}, Mails :: list()) -> ok.
coming(User = #user{mail = MailList}, Mails) ->
    user_sender:send(User, ?CMD_MAIL, [Mails]),
    User#user{mail = Mails ++ MailList}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%% split attachment
make(Receiver, Name, Title, Content, From, Items, Mails) when length(Items) =< ?MAIL_MAX_ITEM ->
    Mail = mail(Receiver, Name, Title, Content, From, Items),
    [Mail| Mails];
make(Receiver, Name, Title, Content, From, [A, B, C, D, E | Items], Mails) ->
    Mail = mail(Receiver, Name, Title, Content, From, [A, B, C, D, E]),
    make(Receiver, Name, Title, Content, From, Items, [Mail | Mails]).

%% make a new mail
mail(Receiver, Name, Title, Content, From, Items) ->
    Mail = #mail{
        receiver_id = Receiver,
        receiver_nick = Name,
        attachment = Items,
        title = Title,
        content = Content,
        receive_time = time:ts(),
        valid_time = ?MAIL_VALID_DATE,
        from = From
    },
    MailId = mail_sql:insert(Mail),
    Mail#mail{mail_id = MailId}.
