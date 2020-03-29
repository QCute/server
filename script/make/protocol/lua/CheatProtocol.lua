local cheatProtocol = {
    ["read"] = {
        [60000] = {
            {name = "result", type = "rst", comment = "结果", explain = {}}
        },
        [60001] = {
            {name = "cheatList", type = "list", comment = "秘籍列表", explain = {
                {name = "description", type = "str", comment = "描述", explain = {}},
                {name = "command", type = "str", comment = "命令", explain = {}}
            }}
        }
    },
    ["write"] = {
        [60000] = {
            {name = "command", type = "str", comment = "命令", explain = {}}
        },
        [60001] = {}
    }
}