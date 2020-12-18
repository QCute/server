const friendProtocol = {
    "write" : {
        "11501" : [],
        "11502" : [
            {"name" : "friendId", "type" : "u64", "comment" : "好友ID", "explain" : []}
        ],
        "11503" : [
            {"name" : "friendId", "type" : "u64", "comment" : "好友ID", "explain" : []}
        ],
        "11504" : [
            {"name" : "friendId", "type" : "u64", "comment" : "好友ID", "explain" : []}
        ]
    },
    "read" : {
        "11501" : [
            {"name" : "list", "type" : "list", "comment" : "好友列表", "explain" : [
                {"name" : "friendId", "type" : "u64", "comment" : "好友ID", "explain" : []},
                {"name" : "friendName", "type" : "bst", "comment" : "好友名字", "explain" : []},
                {"name" : "relation", "type" : "u8", "comment" : "关系状态(申请:1/好友:2/黑名单:3)", "explain" : []},
                {"name" : "time", "type" : "u32", "comment" : "添加/修改状态时间", "explain" : []}
            ]}
        ],
        "11502" : [
            {"name" : "result", "type" : "rst", "comment" : "结果", "explain" : []}
        ],
        "11503" : [
            {"name" : "result", "type" : "rst", "comment" : "结果", "explain" : []}
        ],
        "11504" : [
            {"name" : "result", "type" : "rst", "comment" : "结果", "explain" : []},
            {"name" : "friendId", "type" : "u64", "comment" : "好友ID", "explain" : []}
        ]
    }
};