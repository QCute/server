export default {
    "30101" : {
        "comment" : "公会列表",
        "write" : [],
        "read" : [
            {"name" : "list", "type" : "list", "comment" : "公会列表", "explain" : [
                {"name" : "guildId", "type" : "u64", "comment" : "公会ID", "explain" : []},
                {"name" : "guildName", "type" : "bst", "comment" : "公会名字", "explain" : []},
                {"name" : "createTime", "type" : "u32", "comment" : "创建时间", "explain" : []},
                {"name" : "leaderRoleId", "type" : "u64", "comment" : "会长角色ID", "explain" : []},
                {"name" : "leaderName", "type" : "bst", "comment" : "会长名字", "explain" : []}
            ]}
        ]
    },
    "30102" : {
        "comment" : "成员列表",
        "write" : [],
        "read" : [
            {"name" : "list", "type" : "list", "comment" : "成员列表", "explain" : [
                {"name" : "roleId", "type" : "u64", "comment" : "成员ID", "explain" : []},
                {"name" : "job", "type" : "u8", "comment" : "职位", "explain" : []},
                {"name" : "joinTime", "type" : "u32", "comment" : "加入时间", "explain" : []},
                {"name" : "roleName", "type" : "bst", "comment" : "成员名字", "explain" : []},
                {"name" : "sex", "type" : "u8", "comment" : "性别", "explain" : []},
                {"name" : "classes", "type" : "u8", "comment" : "职业", "explain" : []},
                {"name" : "vipLevel", "type" : "u8", "comment" : "Vip等级", "explain" : []}
            ]}
        ]
    },
    "30103" : {
        "comment" : "申请列表",
        "write" : [],
        "read" : [
            {"name" : "list", "type" : "list", "comment" : "申请列表", "explain" : [
                {"name" : "roleId", "type" : "u64", "comment" : "申请ID", "explain" : []},
                {"name" : "applyTime", "type" : "u32", "comment" : "申请时间", "explain" : []},
                {"name" : "roleName", "type" : "bst", "comment" : "申请名字", "explain" : []},
                {"name" : "sex", "type" : "u8", "comment" : "性别", "explain" : []},
                {"name" : "classes", "type" : "u8", "comment" : "职业", "explain" : []},
                {"name" : "vipLevel", "type" : "u8", "comment" : "Vip等级", "explain" : []}
            ]}
        ]
    },
    "30104" : {
        "comment" : "自身公会信息",
        "write" : [],
        "read" : [
            {"name" : "guildId", "type" : "u64", "comment" : "公会ID", "explain" : []},
            {"name" : "guildName", "type" : "bst", "comment" : "公会名字", "explain" : []},
            {"name" : "exp", "type" : "u32", "comment" : "经验", "explain" : []},
            {"name" : "wealth", "type" : "u32", "comment" : "财富", "explain" : []},
            {"name" : "level", "type" : "u8", "comment" : "等级", "explain" : []},
            {"name" : "createTime", "type" : "u32", "comment" : "创建时间", "explain" : []},
            {"name" : "notice", "type" : "bst", "comment" : "公告", "explain" : []},
            {"name" : "leaderRoleId", "type" : "u64", "comment" : "会长角色ID", "explain" : []},
            {"name" : "leaderName", "type" : "bst", "comment" : "会长名字", "explain" : []}
        ]
    },
    "30105" : {
        "comment" : "自身成员信息",
        "write" : [],
        "read" : [
            {"name" : "roleId", "type" : "u64", "comment" : "成员ID", "explain" : []},
            {"name" : "job", "type" : "u8", "comment" : "职位", "explain" : []},
            {"name" : "joinTime", "type" : "u32", "comment" : "加入时间", "explain" : []},
            {"name" : "roleName", "type" : "bst", "comment" : "成员名字", "explain" : []},
            {"name" : "sex", "type" : "u8", "comment" : "性别", "explain" : []},
            {"name" : "classes", "type" : "u8", "comment" : "职业", "explain" : []},
            {"name" : "vipLevel", "type" : "u8", "comment" : "Vip等级", "explain" : []}
        ]
    },
    "30106" : {
        "comment" : "自身申请信息",
        "write" : [],
        "read" : [
            {"name" : "list", "type" : "list", "comment" : "", "explain" : [
                {"name" : "guildId", "type" : "u64", "comment" : "公会ID", "explain" : []},
                {"name" : "applyTime", "type" : "u32", "comment" : "申请时间", "explain" : []},
                {"name" : "guildName", "type" : "bst", "comment" : "公会名字", "explain" : []}
            ]}
        ]
    },
    "30107" : {
        "comment" : "创建公会",
        "write" : [
            {"name" : "type", "type" : "u8", "comment" : "类型", "explain" : []},
            {"name" : "guildName", "type" : "bst", "comment" : "公会名", "explain" : []}
        ],
        "read" : [
            {"name" : "result", "type" : "rst", "comment" : "结果", "explain" : []}
        ]
    },
    "30108" : {
        "comment" : "申请",
        "write" : [
            {"name" : "guildId", "type" : "u64", "comment" : "公会ID", "explain" : []}
        ],
        "read" : [
            {"name" : "result", "type" : "rst", "comment" : "结果", "explain" : []}
        ]
    },
    "30109" : {
        "comment" : "取消申请",
        "write" : [
            {"name" : "guildId", "type" : "u64", "comment" : "公会ID", "explain" : []}
        ],
        "read" : [
            {"name" : "result", "type" : "rst", "comment" : "结果", "explain" : []}
        ]
    },
    "30110" : {
        "comment" : "取消全部申请",
        "write" : [],
        "read" : [
            {"name" : "result", "type" : "rst", "comment" : "结果", "explain" : []}
        ]
    },
    "30111" : {
        "comment" : "允许申请",
        "write" : [
            {"name" : "roleId", "type" : "u64", "comment" : "角色ID", "explain" : []}
        ],
        "read" : [
            {"name" : "result", "type" : "rst", "comment" : "结果", "explain" : []}
        ]
    },
    "30112" : {
        "comment" : "允许全部申请",
        "write" : [],
        "read" : [
            {"name" : "result", "type" : "rst", "comment" : "结果", "explain" : []}
        ]
    },
    "30113" : {
        "comment" : "拒绝申请",
        "write" : [
            {"name" : "roleId", "type" : "u64", "comment" : "角色ID", "explain" : []}
        ],
        "read" : [
            {"name" : "result", "type" : "rst", "comment" : "结果", "explain" : []}
        ]
    },
    "30114" : {
        "comment" : "拒绝全部申请",
        "write" : [],
        "read" : [
            {"name" : "result", "type" : "rst", "comment" : "结果", "explain" : []}
        ]
    },
    "30115" : {
        "comment" : "退出",
        "write" : [],
        "read" : [
            {"name" : "result", "type" : "rst", "comment" : "结果", "explain" : []}
        ]
    },
    "30116" : {
        "comment" : "解散",
        "write" : [],
        "read" : [
            {"name" : "result", "type" : "rst", "comment" : "结果", "explain" : []}
        ]
    },
    "30117" : {
        "comment" : "踢出",
        "write" : [
            {"name" : "roleId", "type" : "u64", "comment" : "角色ID", "explain" : []}
        ],
        "read" : [
            {"name" : "result", "type" : "rst", "comment" : "结果", "explain" : []}
        ]
    },
    "30118" : {
        "comment" : "调整位置",
        "write" : [
            {"name" : "roleId", "type" : "u64", "comment" : "角色ID", "explain" : []},
            {"name" : "job", "type" : "u8", "comment" : "位置", "explain" : []}
        ],
        "read" : [
            {"name" : "result", "type" : "rst", "comment" : "结果", "explain" : []}
        ]
    },
    "30119" : {
        "comment" : "升级",
        "write" : [],
        "read" : [
            {"name" : "result", "type" : "rst", "comment" : "结果", "explain" : []}
        ]
    },
    "30120" : {
        "comment" : "更改公告",
        "write" : [
            {"name" : "notice", "type" : "bst", "comment" : "公告", "explain" : []}
        ],
        "read" : [
            {"name" : "result", "type" : "rst", "comment" : "结果", "explain" : []}
        ]
    }
};