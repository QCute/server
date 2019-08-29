local vipProtocol = {
    ["read"] = {
        [10301] = {}
    },
    ["write"] = {
        [10301] = {
            {name = "level", type = "u8", comment = "等级", explain = {}},
            {name = "exp", type = "u64", comment = "经验", explain = {}},
            {name = "expireTime", type = "u32", comment = "过期时间", explain = {}}
        }
    }
}