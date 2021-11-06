const dungeonProtocol = {
    "17001" : {
        "comment" : "副本信息",
        "write" : [],
        "read" : [
            {"name" : "list", "type" : "list", "comment" : "", "explain" : [
                {"name" : "dungeonId", "type" : "u32", "comment" : "副本Id", "explain" : []},
                {"name" : "todayNumber", "type" : "u16", "comment" : "今天次数", "explain" : []},
                {"name" : "totalNumber", "type" : "u16", "comment" : "总次数", "explain" : []}
            ]}
        ]
    },
    "17002" : {
        "comment" : "进入副本",
        "write" : [
            {"name" : "dungeonId", "type" : "u32", "comment" : "副本Id", "explain" : []}
        ],
        "read" : [
            {"name" : "result", "type" : "rst", "comment" : "结果", "explain" : []}
        ]
    },
    "17003" : {
        "comment" : "副本开始",
        "write" : [],
        "read" : [
            {"name" : "result", "type" : "rst", "comment" : "结果", "explain" : []}
        ]
    },
    "17004" : {
        "comment" : "副本结束",
        "write" : [],
        "read" : [
            {"name" : "result", "type" : "rst", "comment" : "结果", "explain" : []}
        ]
    },
    "17005" : {
        "comment" : "副本鼓舞",
        "write" : [],
        "read" : [
            {"name" : "result", "type" : "rst", "comment" : "结果", "explain" : []}
        ]
    }
};