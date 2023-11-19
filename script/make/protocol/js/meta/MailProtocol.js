export default {
    "11401": {
        "comment": "邮件列表",
        "write": {"name": "data", "type": "map", "comment": "", "explain": [

        ]},
        "read": {"name": "data", "type": "list", "comment": "", "explain": [
            {"name": "data", "type": "map", "comment": "", "explain": [
                {"name": "mailId", "type": "u64", "comment": "邮件ID", "explain": []},
                {"name": "receiveTime", "type": "u32", "comment": "接收时间", "explain": []},
                {"name": "expireTime", "type": "u32", "comment": "有效时间", "explain": []},
                {"name": "readTime", "type": "u32", "comment": "读取时间", "explain": []},
                {"name": "receiveAttachmentTime", "type": "u32", "comment": "领取附件时间", "explain": []},
                {"name": "title", "type": "bst", "comment": "标题", "explain": []},
                {"name": "content", "type": "bst", "comment": "内容", "explain": []},
                {"name": "attachment", "type": "list", "comment": "附件列表", "explain": [
                    {"name": "data", "type": "map", "comment": "", "explain": [
                        {"name": "itemId", "type": "u32", "comment": "物品ID", "explain": []},
                        {"name": "number", "type": "u16", "comment": "数量", "explain": []}
                    ]}
                ]}
            ]}
        ]}
    },
    "11402": {
        "comment": "阅读",
        "write": {"name": "data", "type": "u64", "comment": "邮件ID", "explain": []},
        "read": {"name": "data", "type": "ast", "comment": "结果", "explain": []}
    },
    "11403": {
        "comment": "领取附件",
        "write": {"name": "data", "type": "u64", "comment": "邮件ID", "explain": []},
        "read": {"name": "data", "type": "ast", "comment": "结果", "explain": []}
    },
    "11404": {
        "comment": "删除邮件",
        "write": {"name": "data", "type": "u64", "comment": "邮件ID", "explain": []},
        "read": {"name": "data", "type": "ast", "comment": "结果", "explain": []}
    }
};