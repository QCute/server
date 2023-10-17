%%%-------------------------------------------------------------------
%%! +pc unicode
%%% @doc
%%% sql script for sql maker
%%% @end
%%%-------------------------------------------------------------------
-module(sql_script).
-export([main/1]).
-include("../../../include/journal.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
main(Keys) ->
    io:setopts([{encoding, unicode}]),
    io:setopts(standard_error, [{encoding, unicode}]),
    code:add_path(filename:dirname(escript:script_name()) ++ "/../../../beam/"),
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
            file => "src/module/role/role_sql.erl",
            table => role,
            include => ["role.hrl"]
        },
        #{
            file => "src/module/asset/asset_sql.erl",
            table => asset,
            include => ["asset.hrl"]
        },
        #{
            file => "src/module/vip/vip_sql.erl",
            table => vip,
            include => ["vip.hrl"]
        },
        #{
            file => "src/module/count/count_sql.erl",
            table => count,
            include => ["count.hrl"]
        },
        #{
            file => "src/module/item/item_sql.erl",
            table => item,
            include => ["item.hrl"]
        },
        #{
            file => "src/module/task/task_sql.erl",
            table => task,
            include => ["task.hrl"]
        },
        #{
            file => "src/module/achievement/achievement_sql.erl",
            table => achievement,
            include => ["achievement.hrl"]
        },
        #{
            file => "src/module/shop/shop_sql.erl",
            table => shop,
            include => ["shop.hrl"]
        },
        #{
            file => "src/module/mail/mail_sql.erl",
            table => mail,
            include => ["mail.hrl"]
        },
        #{
            file => "src/module/friend/friend_sql.erl",
            table => friend,
            include => ["friend.hrl"]
        },
        #{
            file => "src/module/skill/skill_sql.erl",
            table => skill,
            include => ["skill.hrl"]
        },
        #{
            file => "src/module/buff/buff_sql.erl",
            table => buff,
            include => ["buff.hrl"]
        },
        #{
            file => "src/module/fashion/fashion_sql.erl",
            table => fashion,
            include => ["fashion.hrl"]
        },
        #{
            file => "src/module/title/title_sql.erl",
            table => title,
            include => ["title.hrl"]
        },
        #{
            file => "src/module/bubble/bubble_sql.erl",
            table => bubble,
            include => ["bubble.hrl"]
        },
        #{
            file => "src/module/sign/sign_sql.erl",
            table => sign,
            include => ["sign.hrl"]
        },
        #{
            file => "src/module/daily/daily_sql.erl",
            table => daily,
            include => ["daily.hrl"]
        },
        #{
            file => "src/module/daily/daily_active_sql.erl",
            table => daily_active,
            include => ["daily.hrl"]
        },
        #{
            file => "src/module/charge/charge_sql.erl",
            table => charge,
            include => ["charge.hrl"]
        },
        #{
            file => "src/module/auction/auction_sql.erl",
            table => auction,
            include => ["auction.hrl"],
            mode => [
                {select, []}
            ]
        },
        #{
            file => "src/module/auction/auction_role_sql.erl",
            table => auction_role,
            include => ["auction.hrl"],
            mode => [
                {select, []}
            ]
        },
        #{
            file => "src/module/key/key_sql.erl",
            table => key,
            include => ["key.hrl"]
        },
        #{
            file => "src/module/rank/rank_sql.erl",
            table => rank,
            include => ["rank.hrl"]
        },
        #{
            file => "src/module/dungeon/dungeon_sql.erl",
            table => dungeon,
            include => ["dungeon.hrl"]
        },
        #{
            file => "src/module/guild/guild_sql.erl",
            table => guild,
            include => ["guild.hrl"],
            mode => [
                {select, []}
            ]
        },
        #{
            file => "src/module/guild/guild_role_sql.erl",
            table => guild_role,
            include => ["guild.hrl"],
            mode => [
                {select, []}
            ]
        },
        #{
            file => "src/module/guild/guild_apply_sql.erl",
            table => guild_apply,
            include => ["guild.hrl"],
            mode => [
                {select, []}
            ]
        },
        #{
            file => "src/module/lucky_money/lucky_money_sql.erl",
            table => lucky_money,
            include => ["lucky_money.hrl"],
            mode => [
                {select, []}
            ]
        },
        #{
            file => "src/module/lucky_money/lucky_money_role_sql.erl",
            table => lucky_money_role,
            include => ["lucky_money.hrl"],
            mode => [
                {select, []}
            ]
        }
    ].
