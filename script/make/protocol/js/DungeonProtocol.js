const dungeonProtocol = {
    "write" : {
        "17001" : [],
        "17002" : [
            {"name" : "dungeonId", "type" : "u32", "comment" : "副本Id", "explain" : []}
        ],
        "17005" : []
    },
    "read" : {
        "17001" : [
            {"name" : "list", "type" : "list", "comment" : "", "explain" : [
                {"name" : "dungeonId", "type" : "u32", "comment" : "副本Id", "explain" : []},
                {"name" : "todayNumber", "type" : "u16", "comment" : "今天次数", "explain" : []},
                {"name" : "totalNumber", "type" : "u16", "comment" : "总次数", "explain" : []}
            ]}
        ],
        "17002" : [
            {"name" : "result", "type" : "rst", "comment" : "结果", "explain" : []}
        ],
        "17003" : [
            {"name" : "result", "type" : "rst", "comment" : "结果", "explain" : []}
        ],
        "17004" : [
            {"name" : "result", "type" : "rst", "comment" : "结果", "explain" : []}
        ],
        "17005" : [
            {"name" : "result", "type" : "rst", "comment" : "结果", "explain" : []}
        ]
    }
};