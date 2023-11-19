%%%-------------------------------------------------------------------
%%% @doc
%%% notice server
%%% @end
%%%-------------------------------------------------------------------
-module(notice_server).
-behaviour(gen_server).
%% API
-export([start/0, start_link/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([add/4, list/0]).
%% Includes
-include("common.hrl").
-include("journal.hrl").
-include("user.hrl").
-include("online.hrl").
-include("mail.hrl").
-include("notice.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc start
-spec start() -> {ok, pid()} | {error, term()}.
start() ->
    process:start(?MODULE).

%% @doc server start
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc add notice
-spec add(Type :: non_neg_integer(), Title :: binary(), Content :: binary(), Items :: list()) -> ok.
add(Type, Title, Content, Items) ->
    gen_server:cast({local, ?MODULE}, {add_notice, Type, Title, Content, Items}).

%% @doc get notice list
-spec list() -> List :: [#notice{}].
list() ->
    ets:tab2list(?NOTICE).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
%% @doc init
-spec init(Args :: term()) -> {ok, State :: []}.
init(_) ->
    erlang:process_flag(trap_exit, true),
    NoticeList = notice_sql:select(),
    ets:new(?NOTICE, [named_table, set, public, {keypos, #notice.notice_id}, {write_concurrency, true}, {read_concurrency, true}]),
    ets:insert(?NOTICE, NoticeList),
    {ok, []}.

%% @doc handle_call
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: []) -> {reply, Reply :: term(), NewState :: []}.
handle_call(Request, From, State) ->
    try
        do_call(Request, From, State)
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?STACKTRACE(Class, Reason, ?GET_STACKTRACE(Stacktrace)),
        {reply, {error, Reason}, State}
    end.

%% @doc handle_cast
-spec handle_cast(Request :: term(), State :: []) -> {noreply, NewState :: []}.
handle_cast(Request, State) ->
    try
        do_cast(Request, State)
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?STACKTRACE(Class, Reason, ?GET_STACKTRACE(Stacktrace)),
        {noreply, State}
    end.

%% @doc handle_info
-spec handle_info(Request :: term(), State :: []) -> {noreply, NewState :: []}.
handle_info(Info, State) ->
    try
        do_info(Info, State)
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?STACKTRACE(Class, Reason, ?GET_STACKTRACE(Stacktrace)),
        {noreply, State}
    end.

%% @doc terminate
-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()), State :: []) -> {ok, NewState :: []}.
terminate(_Reason, State) ->
    {ok, State}.

%% @doc code_change
-spec code_change(OldVsn :: (term() | {down, term()}), State :: [], Extra :: term()) -> {ok, NewState :: []}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_call(_Request, _From, State) ->
    {reply, ok, State}.

do_cast({add_notice, Type, Title, Content, Attachment}, State) ->
    Notice = #notice{type = Type, receive_time = time:now(), title = Title, content = Content, attachment = Attachment},
    NoticeId = notice_sql:insert(Notice),
    ets:insert(?NOTICE, Notice#notice{notice_id = NoticeId}),
    %% @todo broadcast different notice type coming
    user_manager:foreach(fun(#online{pid = Pid}) -> user_server:apply_cast(Pid, notice, coming, [Notice]) end),
    {noreply, State};
do_cast(_Request, State) ->
    {noreply, State}.


do_info(_Info, State) ->
    {noreply, State}.

