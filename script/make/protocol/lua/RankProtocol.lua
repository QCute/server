local rankProtocol = {
    ["write"] = {
        [19001] = {
            {name = "list", type = "list", comment = "排行榜", explain = {
                {name = "type", type = "u16", comment = "类型", explain = {}},
                {name = "rank", type = "u64", comment = "排名", explain = {}},
                {name = "key", type = "u64", comment = "键", explain = {}},
                {name = "value", type = "u64", comment = "值", explain = {}},
                {name = "time", type = "u32", comment = "时间", explain = {}},
                {name = "name", type = "bst", comment = "名字", explain = {}},
                {name = "serverId", type = "u16", comment = "服务器ID", explain = {}}
            }}
        },
        [19002] = {
            {name = "list", type = "list", comment = "排行榜", explain = {
                {name = "type", type = "u16", comment = "类型", explain = {}},
                {name = "rank", type = "u64", comment = "排名", explain = {}},
                {name = "key", type = "u64", comment = "键", explain = {}},
                {name = "value", type = "u64", comment = "值", explain = {}},
                {name = "time", type = "u32", comment = "时间", explain = {}},
                {name = "name", type = "bst", comment = "名字", explain = {}},
                {name = "serverId", type = "u16", comment = "服务器ID", explain = {}},
                {name = "level", type = "u16", comment = "等级", explain = {}},
                {name = "classes", type = "u8", comment = "职业", explain = {}}
            }}
        },
        [19003] = {
            {name = "list", type = "list", comment = "排行榜", explain = {
                {name = "type", type = "u16", comment = "类型", explain = {}},
                {name = "rank", type = "u64", comment = "排名", explain = {}},
                {name = "key", type = "u64", comment = "键", explain = {}},
                {name = "value", type = "u64", comment = "值", explain = {}},
                {name = "time", type = "u32", comment = "时间", explain = {}},
                {name = "name", type = "bst", comment = "名字", explain = {}},
                {name = "serverId", type = "u16", comment = "服务器ID", explain = {}},
                {name = "level", type = "u16", comment = "等级", explain = {}},
                {name = "classes", type = "u8", comment = "职业", explain = {}},
                {name = "sex", type = "u8", comment = "性别", explain = {}}
            }}
        },
        [19004] = {
            {name = "list", type = "list", comment = "排行榜", explain = {
                {name = "type", type = "u16", comment = "类型", explain = {}},
                {name = "rank", type = "u64", comment = "排名", explain = {}},
                {name = "key", type = "u64", comment = "键", explain = {}},
                {name = "value", type = "u64", comment = "值", explain = {}},
                {name = "time", type = "u32", comment = "时间", explain = {}},
                {name = "name", type = "bst", comment = "名字", explain = {}},
                {name = "serverId", type = "u16", comment = "服务器ID", explain = {}},
                {name = "level", type = "u16", comment = "等级", explain = {}},
                {name = "classes", type = "u8", comment = "职业", explain = {}},
                {name = "sex", type = "u8", comment = "性别", explain = {}},
                {name = "vipLevel", type = "u8", comment = "VIP等级", explain = {}}
            }}
        },
        [19005] = {
            {name = "list", type = "list", comment = "排行榜", explain = {
                {name = "type", type = "u16", comment = "类型", explain = {}},
                {name = "rank", type = "u64", comment = "排名", explain = {}},
                {name = "key", type = "u64", comment = "键", explain = {}},
                {name = "value", type = "u64", comment = "值", explain = {}},
                {name = "time", type = "u32", comment = "时间", explain = {}},
                {name = "name", type = "bst", comment = "名字", explain = {}},
                {name = "serverId", type = "u16", comment = "服务器ID", explain = {}},
                {name = "level", type = "u16", comment = "等级", explain = {}},
                {name = "classes", type = "u8", comment = "职业", explain = {}},
                {name = "sex", type = "u8", comment = "性别", explain = {}},
                {name = "vipLevel", type = "u8", comment = "VIP等级", explain = {}},
                {name = "avatar", type = "u8", comment = "头像", explain = {}}
            }}
        }
    },
    ["read"] = {
        [19001] = {},
        [19002] = {},
        [19003] = {},
        [19004] = {},
        [19005] = {}
    }
}