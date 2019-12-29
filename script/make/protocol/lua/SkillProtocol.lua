local skillProtocol = {
    ["read"] = {
        [11701] = {
            {name = "list", type = "list", comment = "技能列表", explain = {
                {name = "skillId", type = "u32", comment = "技能ID", explain = {}},
                {name = "level", type = "u16", comment = "技能等级", explain = {}}
            }}
        },
        [11702] = {
            {name = "result", type = "rst", comment = "结果", explain = {}}
        }
    },
    ["write"] = {
        [11701] = {},
        [11702] = {
            {name = "skillId", type = "u32", comment = "技能ID", explain = {}}
        }
    }
}