return {
    [17001] = {
        ["comment"] = "副本信息",
        ["write"] = {
            {name = "data", type = "tuple", comment = "", explain = {

            }}
        },
        ["read"] = {
            {name = "data", type = "list", comment = "", explain = {
                {name = "dungeon", type = "record", comment = "", explain = {
                    {name = "dungeonId", type = "u32", comment = "副本Id", explain = {}},
                    {name = "todayNumber", type = "u16", comment = "今天次数", explain = {}},
                    {name = "totalNumber", type = "u16", comment = "总次数", explain = {}}
                }}
            }}
        }
    },
    [17002] = {
        ["comment"] = "进入副本",
        ["write"] = {
            {name = "data", type = "u32", comment = "副本Id", explain = {}}
        },
        ["read"] = {
            {name = "data", type = "rst", comment = "结果", explain = {}}
        }
    },
    [17005] = {
        ["comment"] = "副本鼓舞",
        ["write"] = {
            {name = "data", type = "tuple", comment = "", explain = {

            }}
        },
        ["read"] = {
            {name = "data", type = "rst", comment = "结果", explain = {}}
        }
    }
}