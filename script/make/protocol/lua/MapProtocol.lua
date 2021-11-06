local mapProtocol = {
    [20001] = {
        ["comment"] = "地图信息",
        ["write"] = {},
        ["read"] = {}
    },
    [20002] = {
        ["comment"] = "自身信息",
        ["write"] = {},
        ["read"] = {
            {name = "id", type = "u64", comment = "ID", explain = {}},
            {name = "type", type = "u8", comment = "类型", explain = {}},
            {name = "fc", type = "u64", comment = "战力", explain = {}},
            {name = "hp", type = "u64", comment = "血量", explain = {}},
            {name = "x", type = "u16", comment = "X坐标", explain = {}},
            {name = "y", type = "u16", comment = "Y坐标", explain = {}}
        }
    },
    [20003] = {
        ["comment"] = "战斗对象列表",
        ["write"] = {},
        ["read"] = {
            {name = "list", type = "list", comment = "对象列表", explain = {
                {name = "id", type = "u64", comment = "ID", explain = {}},
                {name = "type", type = "u8", comment = "类型", explain = {}},
                {name = "hp", type = "u64", comment = "血量", explain = {}},
                {name = "x", type = "u16", comment = "X坐标", explain = {}},
                {name = "y", type = "u16", comment = "Y坐标", explain = {}}
            }}
        }
    },
    [20004] = {
        ["comment"] = "战斗对象移动",
        ["write"] = {},
        ["read"] = {
            {name = "list", type = "list", comment = "对象列表", explain = {
                {name = "id", type = "u64", comment = "ID", explain = {}},
                {name = "type", type = "u8", comment = "类型", explain = {}},
                {name = "x", type = "u16", comment = "X坐标", explain = {}},
                {name = "y", type = "u16", comment = "Y坐标", explain = {}}
            }}
        }
    },
    [20005] = {
        ["comment"] = "战斗对象离开",
        ["write"] = {},
        ["read"] = {
            {name = "list", type = "list", comment = "对象列表", explain = {
                {name = "id", type = "u64", comment = "ID", explain = {}}
            }}
        }
    },
    [20006] = {
        ["comment"] = "玩家移动",
        ["write"] = {
            {name = "x", type = "u16", comment = "X坐标", explain = {}},
            {name = "y", type = "u16", comment = "Y坐标", explain = {}}
        },
        ["read"] = {}
    },
    [20007] = {
        ["comment"] = "发起战斗",
        ["write"] = {
            {name = "skillId", type = "u32", comment = "技能Id", explain = {}},
            {name = "targetList", type = "list", comment = "对象列表", explain = {
                {name = "targetId", type = "u64", comment = "ID", explain = {}}
            }}
        },
        ["read"] = {
            {name = "id", type = "u64", comment = "战斗对象Id", explain = {}},
            {name = "skillId", type = "u32", comment = "技能Id", explain = {}},
            {name = "targetList", type = "list", comment = "对象列表", explain = {
                {name = "targetId", type = "u64", comment = "ID", explain = {}}
            }}
        }
    }
}