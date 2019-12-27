local keyProtocol = {
    ["read"] = {
        [15001] = {
            {name = "result", type = "rst", comment = "结果", explain = {}}
        }
    },
    ["write"] = {
        [15001] = {
            {name = "key", type = "bst", comment = "兑换码", explain = {}}
        }
    }
}