local mailProtocol = {
    ["read"] = {
        [11401] = {},
        [11402] = {
            {name = "mailId", type = "u64", comment = "邮件ID", explain = {}}
        },
        [11403] = {
            {name = "mailId", type = "u64", comment = "邮件ID", explain = {}}
        }
    },
    ["write"] = {
        [11401] = {
            {name = "mail", type = "list", comment = "邮件列表", explain = {
                {name = "mailId", type = "u64", comment = "邮件ID", explain = {}},
                {name = "senderId", type = "u64", comment = "发送者ID", explain = {}},
                {name = "senderNick", type = "bst", comment = "发送者昵称", explain = {}},
                {name = "receiverId", type = "u64", comment = "接受者ID", explain = {}},
                {name = "receiverNick", type = "bst", comment = "接受者昵称", explain = {}},
                {name = "isRead", type = "u8", comment = "是否已经读取", explain = {}},
                {name = "readTime", type = "u32", comment = "读取时间", explain = {}},
                {name = "receiveTime", type = "u32", comment = "接收时间", explain = {}},
                {name = "validTime", type = "u32", comment = "有效时间", explain = {}},
                {name = "title", type = "bst", comment = "标题", explain = {}},
                {name = "content", type = "bst", comment = "内容", explain = {}},
                {name = "attachment", type = "list", comment = "附件列表", explain = {
                    {name = "itemId", type = "u32", comment = "物品ID", explain = {}},
                    {name = "amount", type = "u16", comment = "数量", explain = {}},
                    {name = "bind", type = "u8", comment = "是否绑定", explain = {}}
                }}
            }}
        },
        [11402] = {
            {name = "result", type = "u8", comment = "结果", explain = {}}
        },
        [11403] = {
            {name = "result", type = "u8", comment = "结果", explain = {}}
        }
    }
}