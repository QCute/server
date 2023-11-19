return {
    [12201] = {
        ["comment"] = "统计列表",
        ["write"] = {name = "data", type = "map", comment = "", explain = {

        }},
        ["read"] = {name = "data", type = "list", comment = "统计列表", explain = {
            {name = "data", type = "map", comment = "", explain = {
                {name = "type", type = "u32", comment = "统计类型", explain = {}},
                {name = "totalNumber", type = "u32", comment = "总数", explain = {}}
            }}
        }}
    },
    [12202] = {
        ["comment"] = "成就列表",
        ["write"] = {name = "data", type = "map", comment = "", explain = {

        }},
        ["read"] = {name = "data", type = "list", comment = "成就列表", explain = {
            {name = "data", type = "map", comment = "", explain = {
                {name = "achievementId", type = "u32", comment = "成就ID", explain = {}},
                {name = "type", type = "u32", comment = "成就类型", explain = {}}
            }}
        }}
    },
    [12203] = {
        ["comment"] = "提交成就",
        ["write"] = {name = "data", type = "u32", comment = "成就ID", explain = {}},
        ["read"] = {name = "data", type = "ast", comment = "结果", explain = {}}
    }
}