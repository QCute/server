local rankCenterProtocol = {
    ["read"] = {
        [19101] = {
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
        [19102] = {
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
        [19103] = {
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
        [19104] = {
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
        [19105] = {
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
    ["write"] = {
        [19101] = {},
        [19102] = {},
        [19103] = {},
        [19104] = {},
        [19105] = {}
    }
}