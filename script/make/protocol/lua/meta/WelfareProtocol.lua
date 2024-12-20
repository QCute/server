return {
    [15001] = {
        ["comment"] = "签到",
        ["write"] = {name = "data", type = "map", comment = "", explain = {

        }},
        ["read"] = {name = "data", type = "rst", comment = "结果", explain = {}}
    },
    [15002] = {
        ["comment"] = "兑换码兑换",
        ["write"] = {name = "data", type = "bst", comment = "兑换码", explain = {}},
        ["read"] = {name = "data", type = "rst", comment = "结果", explain = {}}
    },
    [15003] = {
        ["comment"] = "红包",
        ["write"] = {name = "data", type = "u64", comment = "红包编号", explain = {}},
        ["read"] = {name = "luckyMoney", type = "map", comment = "", explain = {
            {name = "luckyMoneyNo", type = "u64", comment = "红包编号", explain = {}},
            {name = "totalGold", type = "u64", comment = "总金币", explain = {}},
            {name = "totalNumber", type = "u32", comment = "总数量", explain = {}},
            {name = "receiveNumber", type = "u16", comment = "已经领取人数", explain = {}},
            {name = "receiveList", type = "list", comment = "领取列表", explain = {
                {name = "luckyMoneyRole", type = "map", comment = "", explain = {
                    {name = "serverId", type = "u16", comment = "服务器Id", explain = {}},
                    {name = "roleId", type = "u64", comment = "角色Id", explain = {}},
                    {name = "roleName", type = "bst", comment = "角色名", explain = {}},
                    {name = "gold", type = "u64", comment = "金币", explain = {}},
                    {name = "time", type = "u32", comment = "领取时间", explain = {}}
                }}
            }},
            {name = "time", type = "u32", comment = "发送时间", explain = {}}
        }}
    },
    [15004] = {
        ["comment"] = "领取红包",
        ["write"] = {name = "data", type = "u64", comment = "红包编号", explain = {}},
        ["read"] = {name = "data", type = "map", comment = "", explain = {
            {name = "result", type = "rst", comment = "结果", explain = {}},
            {name = "gold", type = "u64", comment = "金币", explain = {}}
        }}
    }
}