local keyProtocol = {
    ["read"] = {
        [15001] = {
            {name = "key", type = "bst", comment = "兑换码", explain = {}}
        }
    },
    ["write"] = {
        [15001] = {
            {name = "result", type = "u8", comment = "结果", explain = {}}
        }
    }
}