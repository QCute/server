const achievementProtocol = {
    "write" : {
        "12301" : [],
        "12202" : [],
        "12203" : [
            {"name" : "achievementId", "type" : "u32", "comment" : "成就ID", "explain" : []}
        ]
    },
    "read" : {
        "12301" : [
            {"name" : "list", "type" : "list", "comment" : "统计列表", "explain" : [
                {"name" : "type", "type" : "u32", "comment" : "统计类型", "explain" : []},
                {"name" : "totalNumber", "type" : "u32", "comment" : "总数", "explain" : []}
            ]}
        ],
        "12202" : [
            {"name" : "list", "type" : "list", "comment" : "成就列表", "explain" : [
                {"name" : "achievementId", "type" : "u32", "comment" : "成就ID", "explain" : []},
                {"name" : "type", "type" : "u32", "comment" : "成就类型", "explain" : []}
            ]}
        ],
        "12203" : [
            {"name" : "result", "type" : "rst", "comment" : "结果", "explain" : []}
        ]
    }
};