return {
    [11701] = {
        ["comment"] = "技能列表",
        ["write"] = {
            {name = "data", type = "tuple", comment = "", explain = {

            }}
        },
        ["read"] = {
            {name = "data", type = "list", comment = "技能列表", explain = {
                {name = "skill", type = "record", comment = "", explain = {
                    {name = "skillId", type = "u32", comment = "技能ID", explain = {}},
                    {name = "level", type = "u16", comment = "技能等级", explain = {}}
                }}
            }}
        }
    },
    [11702] = {
        ["comment"] = "学习技能",
        ["write"] = {
            {name = "data", type = "u32", comment = "技能ID", explain = {}}
        },
        ["read"] = {
            {name = "data", type = "rst", comment = "结果", explain = {}}
        }
    }
}