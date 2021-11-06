local noticeProtocol = {
    [50001] = {
        ["comment"] = "公告",
        ["write"] = {},
        ["read"] = {
            {name = "scope", type = "u8", comment = "范围", explain = {}},
            {name = "type", type = "u8", comment = "类型", explain = {}},
            {name = "title", type = "bst", comment = "标题", explain = {}},
            {name = "msg", type = "bst", comment = "消息", explain = {}}
        }
    }
}