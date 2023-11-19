%%%-------------------------------------------------------------------
%%! +pc unicode
%%% @doc
%%% sql script for sql maker
%%% @end
%%%-------------------------------------------------------------------
-module(sql_script).
-export([main/1]).
-include("../../../include/journal.hrl").
-include("../../../include/sql.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
main(Keys) ->
    io:setopts([{encoding, unicode}]),
    io:setopts(standard_error, [{encoding, unicode}]),
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
    ets:insert(ets:new(shell_records, [set, public]), [{Tag, Form} || Form = {attribute, _, record, {Tag, _}} <- lists:append([element(2, epp:parse_file(Header, [], [])) || Header <- filelib:wildcard(filename:dirname(escript:script_name()) ++ "/../../../include/*.hrl")])]),
    Sql = [X || X <- sql(), lists:member(filename:basename(maps:get(file, X), ".erl"), Keys)],
    try
        io:format("~tp~n", [sql_maker:start(Sql)])
    catch ?EXCEPTION(Class, Reason, Stacktrace) ->
        ?HALT(Class, Reason, Stacktrace)
    end.

%%%===================================================================
%%% sql data
%%%===================================================================
sql() ->
    [
        #{
            file => "script/make/sql/data/role_sql.erl",
            sql => [
                #{
                    insert => [],
                    except => role_id,
                    into => role,
                    as => insert
                },
                #{
                    select => [],
                    from => role,
                    by => role_id,
                    as => select
                },
                #{
                    update => [],
                    into => role,
                    except => role_id,
                    by => role_id,
                    as => update
                },
                #{
                    update => role_name,
                    into => role,
                    by => role_id,
                    as => update_name
                }
            ]
        },
        #{
            file => "script/make/sql/data/device_sql.erl",
            sql => [
                #{
                    insert => [],
                    into => device,
                    as => insert
                },
                #{
                    select => [],
                    from => device,
                    by => role_id,
                    as => select
                },
                #{
                    update => [],
                    into => device,
                    except => role_id,
                    by => role_id,
                    as => update
                }
            ]
        },
        #{
            file => "script/make/sql/data/permission_sql.erl",
            sql => [
                #{
                    insert => [],
                    into => permission,
                    as => insert
                },
                #{
                    insert => [],
                    duplicate => ignore,
                    into => permission,
                    as => save
                },
                #{
                    select => [],
                    from => permission,
                    by => #{
                        role_id => '=',
                        type => #{
                            '=' => login
                        }
                    },
                    order_by => #{
                        permission_no => desc
                    },
                    limit => 1,
                    as => select_login
                },
                #{
                    select => [],
                    from => permission,
                    by => #{
                        role_id => '=',
                        type => #{
                            '=' => chat
                        }
                    },
                    order_by => #{
                        permission_no => desc
                    },
                    limit => 1,
                    as => select_chat
                }
            ]
        },
        #{
            file => "script/make/sql/data/asset_sql.erl",
            sql => [
                #{
                    insert => [],
                    into => asset,
                    as => insert
                },
                #{
                    select => [],
                    from => asset,
                    by => role_id,
                    as => select
                },
                #{
                    update => [],
                    into => asset,
                    except => role_id,
                    by => role_id,
                    as => update
                }
            ]
        },
        #{
            file => "script/make/sql/data/vip_sql.erl",
            sql => [
                #{
                    insert => [],
                    into => vip,
                    as => insert
                },
                #{
                    select => [],
                    from => vip,
                    by => role_id,
                    as => select
                },
                #{
                    update => [],
                    into => vip,
                    except => role_id,
                    by => role_id,
                    as => update
                }
            ]
        },
        #{
            file => "script/make/sql/data/count_sql.erl",
            sql => [
                #{
                    insert => [],
                    into => count,
                    duplicate => update,
                    filter => flag,
                    as => save
                },
                #{
                    select => [],
                    from => count,
                    by => role_id,
                    as => select
                }
            ]
        },
        #{
            file => "script/make/sql/data/package_sql.erl",
            sql => [
                #{
                    insert => [],
                    into => package,
                    as => insert
                },
                #{
                    select => [],
                    from => package,
                    by => role_id,
                    as => select
                },
                #{
                    update => [],
                    into => package,
                    except => role_id,
                    by => role_id,
                    as => update
                }
            ]
        },
        #{
            file => "script/make/sql/data/item_sql.erl",
            sql => [
                #{
                    insert => [],
                    into => item,
                    duplicate => update,
                    filter => flag,
                    as => save
                },
                #{
                    select => [],
                    from => item,
                    by => role_id,
                    as => select
                },
                #{
                    delete => [],
                    from => item,
                    by => #{
                        item_no => in
                    },
                    as => delete_in_item_no
                }
            ]
        },
        #{
            file => "script/make/sql/data/task_sql.erl",
            sql => [
                #{
                    insert => [],
                    into => task,
                    duplicate => update,
                    filter => flag,
                    as => save
                },
                #{
                    select => [],
                    from => task,
                    by => role_id,
                    as => select
                }
            ]
        },
        #{
            file => "script/make/sql/data/achievement_sql.erl",
            sql => [
                #{
                    insert => [],
                    into => achievement,
                    duplicate => update,
                    filter => flag,
                    as => save
                },
                #{
                    select => [],
                    from => achievement,
                    by => role_id,
                    as => select
                }
            ]
        },
        #{
            file => "script/make/sql/data/shop_sql.erl",
            sql => [
                #{
                    insert => [],
                    into => shop,
                    duplicate => update,
                    filter => flag,
                    as => save
                },
                #{
                    select => [],
                    from => shop,
                    by => role_id,
                    as => select
                }
            ]
        },
        #{
            file => "script/make/sql/data/mail_sql.erl",
            sql => [
                #{
                    insert => [],
                    into => mail,
                    duplicate => update,
                    filter => flag,
                    as => save
                },
                #{
                    select => [],
                    from => mail,
                    by => role_id,
                    as => select
                },
                #{
                    update => read_time,
                    into => mail,
                    by => mail_id,
                    as => update_read
                },
                #{
                    update => receive_attachment_time,
                    into => mail,
                    by => mail_id,
                    as => update_attachment
                },
                #{
                    delete => [],
                    from => mail,
                    by => mail_id,
                    as => delete
                },
                #{
                    delete => [],
                    from => mail,
                    by => #{
                        mail_id => in
                    },
                    as => delete_in_mail_id
                }
            ]
        },
        #{
            file => "script/make/sql/data/friend_sql.erl",
            sql => [
                #{
                    insert => [],
                    into => friend,
                    duplicate => update,
                    filter => flag,
                    as => save
                },
                #{
                    select => [],
                    from => friend,
                    join => #{
                        role => #{
                            role_id => role_id
                        },
                        vip => #{
                            role_id => role_id
                        }
                    },
                    use => #{
                        friend_name => "role.role_name",
                        sex => "role.sex",
                        avatar => "role.avatar",
                        classes => "role.classes",
                        level => "role.level",
                        is_online => "role.is_online",
                        vip_level => "vip.vip_level"
                    },
                    by => "friend.role_id",
                    as => select
                },
                #{
                    update => relation,
                    into => friend,
                    by => [role_id, friend_role_id],
                    as => update_relation
                },
                #{
                    delete => [],
                    from => friend,
                    by => [role_id, friend_role_id],
                    as => delete
                }
            ]
        },
        #{
            file => "script/make/sql/data/chat_sql.erl",
            sql => [
                #{
                    insert => [],
                    into => chat,
                    as => insert
                },
                #{
                    select => [],
                    from => chat,
                    by => role_id,
                    as => select
                },
                #{
                    update => [],
                    into => chat,
                    by => role_id,
                    as => update
                }
            ]
        },
        #{
            file => "script/make/sql/data/skill_sql.erl",
            sql => [
                #{
                    insert => [],
                    into => skill,
                    duplicate => update,
                    filter => flag,
                    as => save
                },
                #{
                    select => [],
                    from => skill,
                    by => role_id,
                    as => select
                }
            ]
        },
        #{
            file => "script/make/sql/data/buff_sql.erl",
            sql => [
                #{
                    insert => [],
                    into => buff,
                    duplicate => update,
                    filter => flag,
                    as => save
                },
                #{
                    select => [],
                    from => buff,
                    by => role_id,
                    as => select
                },
                #{
                    delete => [],
                    from => buff,
                    by => [role_id, buff_id],
                    as => delete
                }
            ]
        },
        #{
            file => "script/make/sql/data/fashion_sql.erl",
            sql => [
                #{
                    insert => [],
                    into => fashion,
                    duplicate => update,
                    filter => flag,
                    as => save
                },
                #{
                    select => [],
                    from => fashion,
                    by => role_id,
                    as => select
                },
                #{
                    select => [],
                    from => fashion,
                    by => fashion_id,
                    as => select_by_fashion_id
                },
                #{
                    change => role_id,
                    into => fashion,
                    by => [role_id, fashion_id],
                    as => update_role_id
                },
                #{
                    delete => [],
                    from => fashion,
                    by => [role_id, fashion_id],
                    as => delete
                }
            ]
        },
        #{
            file => "script/make/sql/data/title_sql.erl",
            sql => [
                #{
                    insert => [],
                    into => title,
                    duplicate => update,
                    filter => flag,
                    as => save
                },
                #{
                    select => [],
                    from => title,
                    by => role_id,
                    as => select
                },
                #{
                    select => [],
                    from => title,
                    by => title_id,
                    as => select_by_title_id
                },
                #{
                    change => role_id,
                    into => title,
                    by => [role_id, title_id],
                    as => update_role_id
                },
                #{
                    delete => [],
                    from => title,
                    by => [role_id, title_id],
                    as => delete
                }
            ]
        },
        #{
            file => "script/make/sql/data/bubble_sql.erl",
            sql => [
                #{
                    insert => [],
                    into => bubble,
                    duplicate => update,
                    filter => flag,
                    as => save
                },
                #{
                    select => [],
                    from => bubble,
                    by => role_id,
                    as => select
                },
                #{
                    delete => [],
                    from => bubble,
                    by => [role_id, bubble_id],
                    as => delete
                }
            ]
        },
        #{
            file => "script/make/sql/data/sign_sql.erl",
            sql => [
                #{
                    insert => [],
                    into => sign,
                    as => insert
                },
                #{
                    select => [],
                    from => sign,
                    by => role_id,
                    as => select
                },
                #{
                    update => [],
                    into => sign,
                    except => role_id,
                    by => role_id,
                    as => update
                }
            ]
        },
        #{
            file => "script/make/sql/data/daily_sql.erl",
            sql => [
                #{
                    insert => [],
                    into => daily,
                    duplicate => update,
                    filter => flag,
                    as => save
                },
                #{
                    select => [],
                    from => daily,
                    by => role_id,
                    as => select
                }
            ]
        },
        #{
            file => "script/make/sql/data/daily_active_sql.erl",
            sql => [
                #{
                    insert => [],
                    into => daily_active,
                    as => insert
                },
                #{
                    select => [],
                    from => daily_active,
                    by => role_id,
                    as => select
                },
                #{
                    update => [],
                    into => daily_active,
                    except => role_id,
                    by => role_id,
                    as => update
                }
            ]
        },
        #{
            file => "script/make/sql/data/notice_sql.erl",
            sql => [
                #{
                    insert => [],
                    into => notice,
                    as => insert
                },
                #{
                    select => [],
                    from => notice,
                    as => select
                },
                #{
                    delete => [],
                    from => notice,
                    by => #{
                        notice_id => in
                    },
                    as => delete_in_notice_id
                }
            ]
        },
        #{
            file => "script/make/sql/data/notice_role_sql.erl",
            sql => [
                #{
                    insert => [],
                    into => notice_role,
                    duplicate => update,
                    filter => flag,
                    as => save
                },
                #{
                    select => [],
                    from => notice_role,
                    join => #{
                        notice => #{
                            notice_id => notice_id
                        }
                    },
                    use => #{
                        receive_time => "notice.receive_time",
                        expire_time => "notice.expire_time"
                    },
                    by => "notice_role.role_id",
                    as => select
                },
                #{
                    update => read_time,
                    into => notice_role,
                    by => [role_id, notice_id],
                    as => update_read
                }
            ]
        },
        #{
            file => "script/make/sql/data/charge_sql.erl",
            sql => [
                #{
                    insert => [],
                    into => charge,
                    as => insert
                },
                #{
                    select => [],
                    from => charge,
                    by => role_id,
                    as => select
                },
                #{
                    update => [],
                    into => charge,
                    by => role_id,
                    as => update
                }
            ]
        },
        #{
            file => "script/make/sql/data/charge_order_sql.erl",
            sql => [
                #{
                    insert => [],
                    into => charge_order,
                    as => insert
                },
                #{
                    select => [],
                    from => charge_order,
                    by => charge_no,
                    as => select
                },
                #{
                    update => status,
                    into => charge_order,
                    by => charge_no,
                    as => update_status
                }
            ]
        },
        #{
            file => "script/make/sql/data/key_sql.erl",
            sql => [
                #{
                    insert => [],
                    into => key,
                    as => insert
                },
                #{
                    select => [],
                    from => key,
                    by => key,
                    as => select
                }
            ]
        },
        #{
            file => "script/make/sql/data/rank_sql.erl",
            sql => [
                #{
                    insert => [],
                    into => rank,
                    duplicate => update,
                    filter => flag,
                    as => save
                },
                #{
                    select => [],
                    from => rank,
                    by => type,
                    as => select_by_type
                },
                #{
                    delete => [],
                    from => rank,
                    by => type,
                    as => delete_by_type
                }
            ]
        },
        #{
            file => "script/make/sql/data/dungeon_sql.erl",
            sql => [
                #{
                    insert => [],
                    into => dungeon,
                    duplicate => update,
                    filter => flag,
                    as => save
                },
                #{
                    select => [],
                    from => dungeon,
                    by => role_id,
                    as => select
                }
            ]
        },
        #{
            file => "script/make/sql/data/location_sql.erl",
            sql => [
                #{
                    insert => [],
                    into => location,
                    as => insert
                },
                #{
                    select => [],
                    from => location,
                    by => role_id,
                    as => select
                },
                #{
                    update => [],
                    into => location,
                    except => role_id,
                    by => role_id,
                    as => update
                }
            ]
        },
        #{
            file => "script/make/sql/data/guild_sql.erl",
            sql => [
                #{
                    insert => [],
                    into => guild,
                    duplicate => update,
                    filter => flag,
                    as => save
                },
                #{
                    select => [],
                    from => guild,
                    join => #{
                        role => #{
                            role_id => leader_role_id
                        },
                        vip => #{
                            role_id => leader_role_id
                        }
                    },
                    use => #{
                        leader_name => "role.role_name",
                        leader_sex => "role.sex",
                        leader_avatar => "role.avatar",
                        leader_class => "role.classes",
                        leader_level => "role.level",
                        leader_vip_level => "vip.vip_level"
                    },
                    as => select
                },
                #{
                    update => guild_name,
                    into => guild,
                    by => guild_id,
                    as => update_name
                },
                #{
                    update => notice,
                    into => guild,
                    by => guild_id,
                    as => update_notice
                },
                #{
                    delete => [],
                    from => guild,
                    by => guild_id,
                    as => delete
                }
            ]
        },
        #{
            file => "script/make/sql/data/guild_role_sql.erl",
            sql => [
                #{
                    insert => [],
                    into => guild_role,
                    duplicate => update,
                    filter => flag,
                    as => save
                },
                #{
                    select => [],
                    from => guild_role,
                    join => #{
                        guild => #{
                            guild_id => guild_id
                        },
                        role => #{
                            role_id => role_id
                        },
                        vip => #{
                            role_id => role_id
                        }
                    },
                    use => #{
                        guild_name => "guild.guild_name",
                        role_name => "role.role_name",
                        sex => "role.sex",
                        avatar => "role.avatar",
                        classes => "role.classes",
                        level => "role.level",
                        vip_level => "vip.vip_level"
                    },
                    as => select
                },
                #{
                    update => guild_id,
                    into => guild_role,
                    by => role_id,
                    as => update_guild_id
                }
            ]
        },
        #{
            file => "script/make/sql/data/guild_apply_sql.erl",
            sql => [
                #{
                    insert => [],
                    into => guild_apply,
                    duplicate => update,
                    filter => flag,
                    as => save
                },
                #{
                    select => [],
                    from => guild_apply,
                    join => #{
                        guild => #{
                            guild_id => guild_id
                        },
                        role => #{
                            role_id => role_id
                        },
                        vip => #{
                            role_id => role_id
                        }
                    },
                    use => #{
                        guild_name => "guild.guild_name",
                        role_name => "role.role_name",
                        sex => "role.sex",
                        avatar => "role.avatar",
                        classes => "role.classes",
                        level => "role.level",
                        vip_level => "vip.vip_level"
                    },
                    as => select
                },
                #{
                    delete => [],
                    from => guild_apply,
                    by => guild_id,
                    as => delete_by_guild_id
                },
                #{
                    delete => [],
                    from => guild_apply,
                    by => role_id,
                    as => delete_by_role_id
                },
                #{
                    delete => [],
                    from => guild_apply,
                    by => [role_id, guild_id],
                    as => delete
                }
            ]
        },
        #{
            file => "script/make/sql/data/auction_sql.erl",
            sql => [
                #{
                    insert => [],
                    into => auction,
                    duplicate => update,
                    filter => flag,
                    as => save
                },
                #{
                    select => [],
                    from => auction,
                    as => select
                },
                #{
                    delete => [],
                    from => auction,
                    by => auction_no,
                    as => delete
                }
            ]
        },
        #{
            file => "script/make/sql/data/auction_role_sql.erl",
            sql => [
                #{
                    insert => [],
                    into => auction_role,
                    duplicate => update,
                    filter => flag,
                    as => save
                },
                #{
                    select => [],
                    from => auction_role,
                    as => select
                },
                #{
                    delete => [],
                    from => auction_role,
                    by => auction_no,
                    as => delete_by_auction_no
                }
            ]
        },
        #{
            file => "script/make/sql/data/lucky_money_sql.erl",
            sql => [
                #{
                    insert => [],
                    into => lucky_money,
                    duplicate => update,
                    filter => flag,
                    as => save
                },
                #{
                    select => [],
                    from => lucky_money,
                    as => select
                },
                #{
                    delete => [],
                    from => lucky_money,
                    by => #{
                        lucky_money_no => in
                    },
                    as => delete_in_lucky_money_no
                }
            ]
        },
        #{
            file => "script/make/sql/data/lucky_money_role_sql.erl",
            sql => [
                #{
                    insert => [],
                    into => lucky_money_role,
                    duplicate => update,
                    filter => flag,
                    as => save
                },
                #{
                    select => [],
                    from => lucky_money_role,
                    as => select
                },
                #{
                    delete => [],
                    from => lucky_money_role,
                    by => lucky_money_no,
                    as => delete_by_lucky_money_no
                }
            ]
        }
    ].
