const mailProtocol = {
    "write" : {
        "11401" : [],
        "11402" : [
            {"name" : "mailId", "type" : "u64", "comment" : "邮件ID", "explain" : []}
        ],
        "11403" : [
            {"name" : "mailId", "type" : "u64", "comment" : "邮件ID", "explain" : []}
        ],
        "11404" : [
            {"name" : "mailId", "type" : "u64", "comment" : "邮件ID", "explain" : []}
        ]
    },
    "read" : {
        "11401" : [
            {"name" : "list", "type" : "list", "comment" : "邮件列表", "explain" : [
                {"name" : "mailId", "type" : "u64", "comment" : "邮件ID", "explain" : []},
                {"name" : "receiveTime", "type" : "u32", "comment" : "接收时间", "explain" : []},
                {"name" : "isRead", "type" : "u8", "comment" : "是否已经读取", "explain" : []},
                {"name" : "readTime", "type" : "u32", "comment" : "读取时间", "explain" : []},
                {"name" : "expireTime", "type" : "u32", "comment" : "有效时间", "explain" : []},
                {"name" : "title", "type" : "bst", "comment" : "标题", "explain" : []},
                {"name" : "content", "type" : "bst", "comment" : "内容", "explain" : []},
                {"name" : "attachment", "type" : "list", "comment" : "附件列表", "explain" : [
                    {"name" : "itemId", "type" : "u32", "comment" : "物品ID", "explain" : []},
                    {"name" : "number", "type" : "u16", "comment" : "数量", "explain" : []}
                ]}
            ]}
        ],
        "11402" : [
            {"name" : "result", "type" : "rst", "comment" : "结果", "explain" : []}
        ],
        "11403" : [
            {"name" : "result", "type" : "rst", "comment" : "结果", "explain" : []}
        ],
        "11404" : [
            {"name" : "result", "type" : "rst", "comment" : "结果", "explain" : []}
        ]
    }
};