export default {
    "11402" : {
        "comment" : "阅读",
        "write" : [
            {"name": "mailId", "type": "u64", "comment": "邮件ID", "explain": []}
        ],
        "read" : [
            {"name": "result", "type": "rst", "comment": "结果", "explain": []}
        ]
    },
    "11403" : {
        "comment" : "领取附件",
        "write" : [
            {"name": "mailId", "type": "u64", "comment": "邮件ID", "explain": []}
        ],
        "read" : [
            {"name": "result", "type": "rst", "comment": "结果", "explain": []}
        ]
    },
    "11404" : {
        "comment" : "删除邮件",
        "write" : [
            {"name": "mailId", "type": "u64", "comment": "邮件ID", "explain": []}
        ],
        "read" : [
            {"name": "result", "type": "rst", "comment": "结果", "explain": []}
        ]
    }
};