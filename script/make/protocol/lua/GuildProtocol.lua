local guildProtocol = {
    ["read"] = {
        [30101] = {
            {name = "list", type = "list", comment = "公会列表", explain = {
                {name = "guildId", type = "u64", comment = "公会ID", explain = {}},
                {name = "createTime", type = "u32", comment = "创建时间", explain = {}},
                {name = "guildName", type = "bst", comment = "公会名字", explain = {}},
                {name = "leaderId", type = "u64", comment = "会长ID", explain = {}},
                {name = "leaderName", type = "bst", comment = "会长名字", explain = {}}
            }}
        },
        [30102] = {
            {name = "list", type = "list", comment = "成员列表", explain = {
                {name = "roleId", type = "u64", comment = "成员ID", explain = {}},
                {name = "job", type = "u8", comment = "职位", explain = {}},
                {name = "joinTime", type = "u32", comment = "加入时间", explain = {}},
                {name = "roleName", type = "bst", comment = "成员名字", explain = {}},
                {name = "sex", type = "u8", comment = "性别", explain = {}},
                {name = "classes", type = "u8", comment = "职业", explain = {}},
                {name = "vipLevel", type = "u8", comment = "Vip等级", explain = {}}
            }}
        },
        [30103] = {
            {name = "list", type = "list", comment = "申请列表", explain = {
                {name = "roleId", type = "u64", comment = "申请ID", explain = {}},
                {name = "applyTime", type = "u32", comment = "申请时间", explain = {}},
                {name = "roleName", type = "bst", comment = "申请名字", explain = {}},
                {name = "sex", type = "u8", comment = "性别", explain = {}},
                {name = "classes", type = "u8", comment = "职业", explain = {}},
                {name = "vipLevel", type = "u8", comment = "Vip等级", explain = {}}
            }}
        },
        [30104] = {
            {name = "guildId", type = "u64", comment = "公会ID", explain = {}},
            {name = "exp", type = "u32", comment = "经验", explain = {}},
            {name = "wealth", type = "u32", comment = "财富", explain = {}},
            {name = "level", type = "u8", comment = "等级", explain = {}},
            {name = "createTime", type = "u32", comment = "创建时间", explain = {}},
            {name = "guildName", type = "bst", comment = "公会名字", explain = {}},
            {name = "notice", type = "bst", comment = "公告", explain = {}},
            {name = "leaderId", type = "u64", comment = "会长ID", explain = {}},
            {name = "leaderName", type = "bst", comment = "会长名字", explain = {}}
        },
        [30105] = {
            {name = "roleId", type = "u64", comment = "成员ID", explain = {}},
            {name = "job", type = "u8", comment = "职位", explain = {}},
            {name = "joinTime", type = "u32", comment = "加入时间", explain = {}},
            {name = "roleName", type = "bst", comment = "成员名字", explain = {}},
            {name = "sex", type = "u8", comment = "性别", explain = {}},
            {name = "classes", type = "u8", comment = "职业", explain = {}},
            {name = "vipLevel", type = "u8", comment = "Vip等级", explain = {}}
        },
        [30106] = {
            {name = "guildId", type = "u64", comment = "公会ID", explain = {}}
        }
    },
    ["write"] = {
        [30101] = {},
        [30102] = {
            {name = "guildId", type = "u64", comment = "公会ID", explain = {}}
        },
        [30103] = {
            {name = "guildId", type = "u64", comment = "公会ID", explain = {}}
        },
        [30104] = {},
        [30105] = {},
        [30106] = {}
    }
}