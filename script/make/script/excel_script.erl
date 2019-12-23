%%%------------------------------------------------------------------
%%% @doc
%%% module excel script
%%% @end
%%%------------------------------------------------------------------
-module(excel_script).
-export([main/1]).
%%%==================================================================
%%% API functions
%%%==================================================================
main(Args) ->
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    io:format("~p~n", [catch parse(Args)]).

%% make xml sheet file
parse(["excel", "xml", Table]) ->
    excel_maker:to_xml(Table);
parse(["xml", Table]) ->
    excel_maker:to_xml(Table);
parse(["excel", "xml", Table, Path | _]) ->
    excel_maker:to_xml(Table, Path);
parse(["xml", Table, Path | _]) ->
    excel_maker:to_xml(Table, Path);
parse(["excel", "table", _File, "-encode" | Encode]) ->
    %% windows nt file name param list convert to integer list
    File = [list_to_integer(I) || I <- Encode],
    excel_maker:to_table(File);
parse(["table", _File, "-encode" | Encode]) ->
    %% windows nt gbk character set
    %% windows nt file name param list convert to integer list
    File = [list_to_integer(I) || I <- Encode],
    excel_maker:to_table(File);
%% import xml sheet data to database
parse(["excel", "table", Table | _]) ->
    excel_maker:to_table(Table);
parse(["table", File | _]) ->
    excel_maker:to_table(File);
%% argument error
parse(_) ->
    io:format("invalid arguments~n").
