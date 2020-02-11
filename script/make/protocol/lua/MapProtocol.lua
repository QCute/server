local mapProtocol = {
    ["read"] = {
        [20001] = {},
        [20002] = {},
        [20003] = {
            {name = "list", type = "list", comment = "对象列表", explain = {
                {name = "id", type = "u64", comment = "ID", explain = {}},
                {name = "type", type = "u8", comment = "类型", explain = {}},
                {name = "hp", type = "u64", comment = "血量", explain = {}},
                {name = "x", type = "u16", comment = "X坐标", explain = {}},
                {name = "y", type = "u16", comment = "Y坐标", explain = {}}
            }}
        },
        [20004] = {
            {name = "list", type = "list", comment = "对象列表", explain = {
                {name = "id", type = "u64", comment = "ID", explain = {}},
                {name = "type", type = "u8", comment = "类型", explain = {}},
                {name = "x", type = "u16", comment = "X坐标", explain = {}},
                {name = "y", type = "u16", comment = "Y坐标", explain = {}}
            }}
        },
        [20005] = {
            {name = "list", type = "list", comment = "对象列表", explain = {
                {name = "id", type = "u64", comment = "ID", explain = {}}
            }}
        },
        [20006] = {}
    },
    ["write"] = {
        [20001] = {},
        [20002] = {},
        [20006] = {
            {name = "x", type = "u16", comment = "X坐标", explain = {}},
            {name = "y", type = "u16", comment = "Y坐标", explain = {}}
        },
        [20007] = {
            {name = "skillId", type = "u32", comment = "技能Id", explain = {}},
            {name = "targetList", type = "list", comment = "对象列表", explain = {
                {name = "targetId", type = "u64", comment = "ID", explain = {}}
            }}
        }
    }
}