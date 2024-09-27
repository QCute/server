return {
    [20012] = {
        ["comment"] = "战斗对象移动",
        ["write"] = {
            {name = "x", type = "u16", comment = "x坐标", explain = {}},
            {name = "y", type = "u16", comment = "y坐标", explain = {}}
        },
        ["read"] = {
            {name = "fighter", type = "record", comment = "Fighter", explain = {
                {name = "id", type = "u64", comment = "ID", explain = {}},
                {name = "x", type = "u16", comment = "X坐标", explain = {}},
                {name = "y", type = "u16", comment = "Y坐标", explain = {}}
            }}
        }
    },
    [20014] = {
        ["comment"] = "发起战斗",
        ["write"] = {
            {name = "skillId", type = "u32", comment = "技能Id", explain = {}},
            {name = "targetList", type = "list", comment = "对象列表", explain = 
                {name = "targetId", type = "u64", comment = "ID", explain = {}}
            }
        },
        ["read"] = {
            {name = "fighterId", type = "u64", comment = "战斗对象Id", explain = {}},
            {name = "performSkillId", type = "u32", comment = "技能Id", explain = {}},
            {name = "list", type = "list", comment = "对象列表", explain = {
                {name = "fighter", type = "record", comment = "Fighter", explain = {
                    {name = "id", type = "u64", comment = "ID", explain = {}},
                    {name = "type", type = "u8", comment = "类型", explain = {}},
                    {name = "skill", type = "list", comment = "技能列表", explain = {
                        {name = "battleSkill", type = "record", comment = "BattleSkill", explain = {
                            {name = "skillId", type = "u32", comment = "技能ID", explain = {}},
                            {name = "time", type = "u32", comment = "时间", explain = {}},
                            {name = "number", type = "u32", comment = "数量", explain = {}}
                        }}
                    }},
                    {name = "buff", type = "list", comment = "Buff列表", explain = {
                        {name = "battleBuff", type = "record", comment = "BattleBuff", explain = {
                            {name = "buffId", type = "u32", comment = "BuffID", explain = {}},
                            {name = "expireTime", type = "u32", comment = "过期时间", explain = {}},
                            {name = "overlap", type = "u32", comment = "数量", explain = {}}
                        }}
                    }},
                    {name = "x", type = "u16", comment = "X坐标", explain = {}},
                    {name = "y", type = "u16", comment = "Y坐标", explain = {}}
                }}
            }}
        }
    }
}