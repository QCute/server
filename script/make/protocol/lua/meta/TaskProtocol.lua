return {
    [11201] = {
        ["comment"] = "任务列表",
        ["write"] = {name = "data", type = "map", comment = "", explain = {

        }},
        ["read"] = {name = "data", type = "list", comment = "任务列表", explain = {
            {name = "data", type = "map", comment = "", explain = {
                {name = "taskId", type = "u32", comment = "任务ID", explain = {}},
                {name = "number", type = "u16", comment = "当前数量", explain = {}},
                {name = "isAward", type = "u8", comment = "是否领取奖励", explain = {}}
            }}
        }}
    },
    [11202] = {
        ["comment"] = "接收任务",
        ["write"] = {name = "data", type = "u32", comment = "任务ID", explain = {}},
        ["read"] = {name = "data", type = "map", comment = "", explain = {
            {name = "result", type = "ast", comment = "结果", explain = {}},
            {name = "task", type = "map", comment = "", explain = {
                {name = "taskId", type = "u32", comment = "任务ID", explain = {}},
                {name = "number", type = "u16", comment = "当前数量", explain = {}},
                {name = "isAward", type = "u8", comment = "是否领取奖励", explain = {}}
            }}
        }}
    },
    [11203] = {
        ["comment"] = "提交任务",
        ["write"] = {name = "data", type = "u32", comment = "任务ID", explain = {}},
        ["read"] = {name = "data", type = "ast", comment = "结果", explain = {}}
    }
}