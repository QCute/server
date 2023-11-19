%%%-------------------------------------------------------------------
%%! +pc unicode
%%% @doc
%%% erl script for erl maker
%%% @end
%%%-------------------------------------------------------------------
-module(erl_script).
-export([main/1]).
-include("../../../include/journal.hrl").
-include("../../../include/sql.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
main([]) ->
    io:setopts([{encoding, unicode}]),
    io:setopts(standard_error, [{encoding, unicode}]),
    List = [io_lib:format("{\"file\":\"~s\",\"comment\":\"~ts\", \"tables\":~0tp}", [File, Comment, [atom_to_list(Table) || #{from := Table} <- Sql]]) || #{file := File, comment := Comment, sql := Sql} <- erl()],
    io:format("[~n~ts~n]~n", [string:join(List, ",\n")]);
main(Keys) ->
    io:setopts([{encoding, unicode}]),
    io:setopts(standard_error, [{encoding, unicode}]),
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    ets:insert(ets:new(shell_records, [set, public]), [{Tag, Form} || Form = {attribute, _, record, {Tag, _}} <- lists:append([element(2, epp:parse_file(Header, [], [])) || Header <- filelib:wildcard(filename:dirname(escript:script_name()) ++ "/../../../include/*.hrl")])]),
    Erl = [X || X <- erl(), lists:member(filename:basename(maps:get(file, X), ".erl"), Keys)],
    try
        io:format("~tp~n", [erl_maker:start(Erl)])
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?HALT(Class, Reason, Stacktrace)
    end.

%%%===================================================================
%%% data
%%%===================================================================
erl() ->
    [
        #{
            file => "script/make/erl/data/test_data.erl",
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
                %%  -> [value]
                #{
                    select => all({}),
                    from => text_data,
                    as => text_list
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
                    sort_by => #{
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
            file => "script/make/erl/data/text_data.erl",
            comment => "文本配置",
            sql => [
                #{
                    select => zhCN,
                    from => text_data,
                    by => key,
                    default => raw("SELECT 'Key'"),
                    as => zhCN
                }
            ],
            extra => [
                %% text with default lang
                "-spec text(Key :: atom()) -> Text :: binary() | Key :: atom().", "\n",
                "text(Key) ->", "\n",
                "    text(Key, parameter_data:get(language)).", "\n",
                "\n"
                %% text with spec lang
                "-spec text(Key :: atom(), Lang :: atom()) -> Text :: binary() | Key :: atom().", "\n",
                "text(Key, zhCN) ->", "\n",
                "    zhCN(Key).", "\n"
            ]
        },
        #{
            file => "script/make/erl/data/parameter_data.erl",
            comment => "自定义参数配置",
            sql => [
                #{
                    select => value,
                    from => parameter_data,
                    by => key,
                    as => get
                }
            ],
            extra => [
                %% get with default
                "-spec get(Key :: atom(), Default :: term()) -> term().", "\n",
                "get(Key, Default) ->", "\n",
                "    case parameter_data:get(Key) of", "\n",
                "        [] ->", "\n",
                "            Default;", "\n",
                "        Value ->", "\n",
                "            Value", "\n",
                "    end.", "\n"
            ]
        },
        #{
            file => "script/make/erl/data/effect_data.erl",
            comment => "效果配置",
            sql => [
                #{
                    select => record(effect_id, scope, object, operation, attribute, field),
                    from => effect_data,
                    by => effect_id,
                    as => get
                }
            ]
        },
        #{
            file => "script/make/erl/data/charge_data.erl",
            comment => "充值配置",
            sql => [
                #{
                    select => record(),
                    from => charge_data,
                    by => charge_id,
                    as => get
                }
            ]
        },
        #{
            file => "script/make/erl/data/sex_data.erl",
            comment => "角色性别配置",
            sql => [
                #{
                    select => name,
                    from => sex_data,
                    by => sex,
                    sort_by => sex,
                    default => "<<>>",
                    as => get
                }
            ]
        },
        #{
            file => "script/make/erl/data/classes_data.erl",
            comment => "角色职业配置",
            sql => [   
                #{
                    select => name,
                    from => classes_data,
                    by => classes,
                    sort_by => classes,
                    default => "<<>>",
                    as => get
                }
            ]
        },
        #{
            file => "script/make/erl/data/level_data.erl",
            comment => "角色等级配置",
            sql => [
                #{
                    select => min(level),
                    from => level_data,
                    as => min_level
                },
                #{
                    select => max(level),
                    from => level_data,
                    as => max_level
                },
                #{
                    select => level,
                    from => level_data,
                    by => #{
                        exp => '<'
                    },
                    sort_by =>#{
                        exp => desc
                    },
                    limit => 1,
                    default => 0,
                    as => level
                },
                #{
                    select => exp,
                    from => level_data,
                    by => level,
                    sort_by => level,
                    default => 0,
                    as => exp
                }
            ]
        },
        #{
            file => "script/make/erl/data/asset_data.erl",
            comment => "资产配置",
            sql => [
                #{
                    select => item_id,
                    from => asset_data,
                    by => asset,
                    sort_by => item_id,
                    default => 0,
                    as => get
                }
            ]
        },
        #{
            file => "script/make/erl/data/vip_data.erl",
            comment => "VIP配置",
            sql => [
                #{
                    select => vip_level,
                    from => vip_data,
                    by => #{
                        exp => '<='
                    },
                    sort_by => #{
                        exp => desc
                    },
                    limit => 1,
                    default => 0,
                    as => level
                }
            ]
        },
        #{
            file => "script/make/erl/data/item_data.erl",
            comment => "物品配置",
            sql => [
                #{
                    select => record(),
                    from => item_data,
                    by => item_id,
                    as => get
                }
            ]
        },
        #{
            file => "script/make/erl/data/task_data.erl",
            comment => "任务配置",
            sql => [
                #{
                    select => record(),
                    from => task_data,
                    by => task_id,
                    as => get
                }
            ]
        },
        #{
            file => "script/make/erl/data/achievement_data.erl",
            comment => "成就配置",
            sql => [
                #{
                    select => record(),
                    from => achievement_data,
                    by => achievement_id,
                    as => get
                },
                #{
                    select => min(achievement_id),
                    from => achievement_data,
                    by => type,
                    unique_by => type,
                    as => first
                },
                #{
                    select => max(achievement_id),
                    from => achievement_data,
                    by => type,
                    as => last
                },
                #{
                    select => all(achievement_id),
                    from => achievement_data,
                    by => type,
                    as => type
                }
            ]
        },
        #{
            file => "script/make/erl/data/shop_data.erl",
            comment => "商店配置",
            sql => [
                #{
                    select => record(),
                    from => shop_data,
                    by => shop_id,
                    as => get
                }
            ]
        },
        #{
            file => "script/make/erl/data/skill_data.erl",
            comment => "技能配置",
            sql => [
                #{
                    select => record(),
                    from => skill_data,
                    by => skill_id,
                    as => get
                }
            ]
        },
        #{
            file => "script/make/erl/data/buff_data.erl",
            comment => "Buff配置",
            sql => [
                #{
                    select => record(),
                    from => buff_data,
                    by => buff_id,
                    as => get
                }
            ]
        },
        #{
            file => "script/make/erl/data/fashion_data.erl",
            comment => "时装配置",
            sql => [
                #{
                    select => record(),
                    from => fashion_data,
                    by => fashion_id,
                    as => get
                }
            ]
        },
        #{
            file => "script/make/erl/data/title_data.erl",
            comment => "称号配置",
            sql => [
                #{
                    select => record(),
                    from => title_data,
                    by => title_id,
                    as => get
                }
            ]
        },
        #{
            file => "script/make/erl/data/bubble_data.erl",
            comment => "气泡配置",
            sql => [
                #{
                    select => record(),
                    from => bubble_data,
                    by => bubble_id,
                    as => get
                }
            ]
        },
        #{
            comment => "签到配置",
            file => "script/make/erl/data/sign_data.erl",
            sql => [
                #{
                    select => record(award),
                    from => sign_data,
                    by => day,
                    as => get
                }
            ]
        },
        #{
            file => "script/make/erl/data/daily_data.erl",
            comment => "日常配置",
            sql => [
                #{
                    select => record(),
                    from => daily_data,
                    by => daily_id,
                    as => get
                },
                #{
                    select => all(record()),
                    from => daily_data,
                    as => list
                }
            ]
        },
        #{
            file => "script/make/erl/data/daily_active_data.erl",
            comment => "日常阶段配置",
            sql => [
                #{
                    select => record(),
                    from => daily_active_data,
                    by => stage_id,
                    as => get
                },
                #{
                    select => all(record()),
                    from => daily_active_data,
                    as => list
                }
            ]
        },
        #{
            file => "script/make/erl/data/key_data.erl",
            comment => "激活码配置",
            sql => [
                #{
                    select => key_award_id,
                    from => key_data,
                    by => key,
                    default => 0,
                    as => get
                }
            ]
        },
        #{
            file => "script/make/erl/data/key_award_data.erl",
            comment => "激活码奖励配置",
            sql => [
                #{
                    select => record(),
                    from => key_award_data,
                    by => key_award_id,
                    as => get
                }
            ]
        },
        #{
            file => "script/make/erl/data/activity_data.erl",
            comment => "活动配置",
            sql => [
                #{
                    select => record(),
                    from => activity_data,
                    by => activity_id,
                    as => get
                },
                #{
                    select => all(activity_id),
                    from => activity_data,
                    as => list
                }
            ]
        },
        #{
            file => "script/make/erl/data/auction_data.erl",
            comment => "拍卖配置",
            sql => [
                #{
                    select => record(),
                    from => auction_data,
                    by => auction_id,
                    as => get
                }
            ]
        },
        #{
            file => "script/make/erl/data/dungeon_data.erl",
            comment => "副本配置",
            sql => [
                #{
                    select => record(),
                    from => dungeon_data,
                    by => dungeon_id,
                    as => get
                }
            ]
        },
        #{
            file => "script/make/erl/data/map_data.erl",
            comment => "地图配置",
            sql => [
                #{
                    select => record(),
                    from => map_data,
                    by => map_id,
                    as => get
                },
                #{
                    select => map_id,
                    from => map_data,
                    sort_by => map_id,
                    limit => 1,
                    as => city
                }
            ]
        },
        #{
            file => "script/make/erl/data/monster_data.erl",
            comment => "怪物配置",
            sql => [
                #{
                    select => record(),
                    from => monster_data,
                    by => monster_id,
                    as => get
                },
                #{
                    select => all(monster_id),
                    from => monster_data,
                    by => type,
                    as => type
                },
                #{
                    select => all(monster_id),
                    from => monster_data,
                    as => all
                }
            ]
        },
        #{
            file => "script/make/erl/data/guild_data.erl",
            comment => "公会配置",
            sql => [
                #{
                    select => {},
                    from => guild_create_data,
                    by => level,
                    as => create_type
                },
                #{
                    select => level,
                    from => guild_level_data,
                    by => #{
                        exp => '<'
                    },
                    sort_by => #{
                        exp => desc
                    },
                    limit => 1,
                    default => 0,
                    as => level
                }
            ]
        }
    ].
