skillProtocol = {
    [11701] = {
        ["comment"] = "技能列表",
        ["write"] = {},
        ["read"] = {
            {name = "list", type = "list", comment = "技能列表", explain = {
                {name = "skillId", type = "u32", comment = "技能ID", explain = {}},
                {name = "level", type = "u16", comment = "技能等级", explain = {}}
            }}
        }
    },
    [11702] = {
        ["comment"] = "学习技能",
        ["write"] = {
            {name = "skillId", type = "u32", comment = "技能ID", explain = {}}
        },
        ["read"] = {
            {name = "result", type = "rst", comment = "结果", explain = {}}
        }
    }
}