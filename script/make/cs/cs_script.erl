%%%-------------------------------------------------------------------
%%! +pc unicode
%%% @doc
%%% cs script for cs maker
%%% @end
%%%-------------------------------------------------------------------
-module(cs_script).
-export([main/1]).
-include("../../../include/journal.hrl").
-include("../../../include/sql.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
main([]) ->
    io:setopts([{encoding, unicode}]),
    io:setopts(standard_error, [{encoding, unicode}]),
    List = [io_lib:format("{\"file\":\"~s\",\"comment\":\"~ts\"}", [File, Comment]) || #{file := File, comment := Comment} <- cs()],
    io:format("[~n~ts~n]~n", [string:join(List, ",\n")]);
main(Keys) ->
    io:setopts([{encoding, unicode}]),
    io:setopts(standard_error, [{encoding, unicode}]),
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    ets:insert(ets:new(shell_records, [set, public]), [{Tag, Form} || Form = {attribute, _, record, {Tag, _}} <- lists:append([element(2, epp:parse_file(Header, [], [])) || Header <- filelib:wildcard(filename:dirname(escript:script_name()) ++ "/../../../include/*.hrl")])]),
    Cs = [X || X <- cs(), lists:member(filename:basename(maps:get(file, X), ".cs"), Keys)],
    try
        io:format("~tp~n", [cs_maker:start(Cs)])
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?HALT(Class, Reason, Stacktrace)
    end.

%%%===================================================================
%%% cs data
%%%===================================================================
cs() ->
    [
        #{
            file => "script/make/cs/data/testData.cs",
            comment => "测试配置",
            sql => [
                %% key -> value
                #{
                    select => 'zhCN',
                    from => text_data,
                    by => key,
                    as => zhCN
                },
                %% key -> column value
                #{
                    select => {},
                    from => text_data,
                    by => key,
                    as => text
                },
                %% key -> [value]
                #{
                    select => all(monster_id),
                    from => monster_data,
                    by => type,
                    as => type
                },
                %% -> [value] (not unique)
                #{
                    select => all(level),
                    from => level_data,
                    order_by => level,
                    as => level
                },
                %% -> [value] (unique)
                #{
                    select => all(type),
                    from => monster_data,
                    group_by => type,
                    as => type_list
                },
                %% -> value
                #{
                    select => {min(exp), max(level)},
                    from => level_data,
                    as => min_exp_max_level
                },
                %% -> value
                #{
                    select => count(zhCN),
                    from => text_data,
                    as => text_count
                },
                %% -> value
                #{
                    select => {max(key), max(zhCN)},
                    from => text_data,
                    as => max_text
                },
                %% key, key, ... -> value
                #{
                    select => description,
                    from => reference_data,
                    by => [key, value],
                    as => ref
                },
                %% key, key, ... -> value in if else range
                #{
                    select => all(description),
                    from => reference_data,
                    by => #{
                        key => '=',
                        value => '<'
                    },
                    as => ref_range
                },
                %% key -> value in if else range ...
                #{
                    select => level,
                    from => level_data,
                    by => #{
                        exp => '<='
                    },
                    order_by => #{
                        exp => desc
                    },
                    limit => 1,
                    default => 0,
                    as => get_level_by_exp_asc
                },
                %% use literal filter data
                #{
                    select => value,
                    from => parameter_data,
                    by => #{
                        key => #{
                            '=' => param(),
                            like => "%size%"
                        }
                    },
                    as => get
                }
            ]
        },
        #{
            file => "script/make/cs/data/parameterData.cs",
            comment => "自定义参数配置",
            sql => [
                #{
                    select => value,
                    from => parameter_data,
                    by => key,
                    as => get
                }
            ]
        }
    ].