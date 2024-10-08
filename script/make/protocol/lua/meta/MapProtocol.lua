return {
    [20001] = {
        ["comment"] = "地图信息",
        ["write"] = {
            {name = "data", type = "tuple", comment = "", explain = {

            }}
        },
        ["read"] = {
            {name = "data", type = "tuple", comment = "", explain = {

            }}
        }
    },
    [20012] = {
        ["comment"] = "战斗对象移动",
        ["write"] = {
            {name = "data", type = "tuple", comment = "", explain = {
                {name = "x", type = "u16", comment = "X坐标", explain = {}},
                {name = "y", type = "u16", comment = "Y坐标", explain = {}}
            }}
        },
        ["read"] = {
            {name = "fighter", type = "record", comment = "", explain = {
                {name = "id", type = "u64", comment = "ID", explain = {}},
                {name = "x", type = "u16", comment = "X坐标", explain = {}},
                {name = "y", type = "u16", comment = "Y坐标", explain = {}}
            }}
        }
    },
    [20014] = {
        ["comment"] = "发起战斗",
        ["write"] = {
            {name = "data", type = "tuple", comment = "", explain = {
                {name = "skillId", type = "u32", comment = "技能Id", explain = {}},
                {name = "targetList", type = "list", comment = "战斗对象ID列表", explain = 
                    {name = "", type = "u64", comment = "战斗对象ID", explain = {}}
                }
            }}
        },
        ["read"] = {
            {name = "data", type = "tuple", comment = "", explain = {
                {name = "fighterId", type = "u64", comment = "战斗对象Id", explain = {}},
                {name = "performSkillId", type = "u32", comment = "技能Id", explain = {}},
                {name = "fighterList", type = "list", comment = "", explain = {
                    {name = "fighter", type = "record", comment = "", explain = {
                        {name = "id", type = "u64", comment = "ID", explain = {}},
                        {name = "type", type = "u8", comment = "类型", explain = {}},
                        {name = "attribute", type = "record", comment = "属性", explain = {
                            {name = "fc", type = "u64", comment = "战力", explain = {}},
                            {name = "hp", type = "u64", comment = "血量", explain = {}},
                            {name = "health", type = "u64", comment = "健康", explain = {}}
                        }},
                        {name = "skill", type = "list", comment = "技能列表", explain = {
                            {name = "battleSkill", type = "record", comment = "", explain = {
                                {name = "skillId", type = "u32", comment = "技能ID", explain = {}},
                                {name = "time", type = "u32", comment = "时间", explain = {}},
                                {name = "number", type = "u32", comment = "数量", explain = {}}
                            }}
                        }},
                        {name = "buff", type = "list", comment = "Buff列表", explain = {
                            {name = "battleBuff", type = "record", comment = "", explain = {
                                {name = "buffId", type = "u32", comment = "BuffID", explain = {}},
                                {name = "expireTime", type = "u32", comment = "过期时间", explain = {}},
                                {name = "overlap", type = "u32", comment = "数量", explain = {}}
                            }}
                        }},
                        {name = "x", type = "u16", comment = "X坐标", explain = {}},
                        {name = "y", type = "u16", comment = "Y坐标", explain = {}}
                    }}
                }}
            }}
        }
    }
}