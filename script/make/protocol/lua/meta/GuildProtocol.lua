return {
    [30107] = {
        ["comment"] = "创建公会",
        ["write"] = {
            {name = "type", type = "u8", comment = "类型", explain = {}},
            {name = "guildName", type = "bst", comment = "公会名", explain = {}}
        },
        ["read"] = {
            {name = "result", type = "rst", comment = "结果", explain = {}}
        }
    },
    [30108] = {
        ["comment"] = "申请",
        ["write"] = {
            {name = "guildId", type = "u64", comment = "公会ID", explain = {}}
        },
        ["read"] = {
            {name = "result", type = "rst", comment = "结果", explain = {}}
        }
    },
    [30109] = {
        ["comment"] = "取消申请",
        ["write"] = {
            {name = "guildId", type = "u64", comment = "公会ID", explain = {}}
        },
        ["read"] = {
            {name = "result", type = "rst", comment = "结果", explain = {}}
        }
    },
    [30111] = {
        ["comment"] = "允许申请",
        ["write"] = {
            {name = "roleId", type = "u64", comment = "角色ID", explain = {}}
        },
        ["read"] = {
            {name = "result", type = "rst", comment = "结果", explain = {}}
        }
    },
    [30113] = {
        ["comment"] = "拒绝申请",
        ["write"] = {
            {name = "roleId", type = "u64", comment = "角色ID", explain = {}}
        },
        ["read"] = {
            {name = "result", type = "rst", comment = "结果", explain = {}}
        }
    },
    [30117] = {
        ["comment"] = "踢出",
        ["write"] = {
            {name = "roleId", type = "u64", comment = "角色ID", explain = {}}
        },
        ["read"] = {
            {name = "result", type = "rst", comment = "结果", explain = {}}
        }
    },
    [30118] = {
        ["comment"] = "调整位置",
        ["write"] = {
            {name = "roleId", type = "u64", comment = "角色ID", explain = {}},
            {name = "job", type = "u8", comment = "位置", explain = {}}
        },
        ["read"] = {
            {name = "result", type = "rst", comment = "结果", explain = {}}
        }
    },
    [30120] = {
        ["comment"] = "更改公告",
        ["write"] = {
            {name = "notice", type = "bst", comment = "公告", explain = {}}
        },
        ["read"] = {
            {name = "result", type = "rst", comment = "结果", explain = {}}
        }
    }
}