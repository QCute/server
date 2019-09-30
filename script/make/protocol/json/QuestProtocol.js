let questProtocol = {
    "read" : {
        "11201" : [],
        "11202" : [
            {"name" : "questId", "type" : "u8", "comment" : "任务ID", "explain" : []}
        ],
        "11203" : [
            {"name" : "questId", "type" : "u8", "comment" : "任务ID", "explain" : []}
        ]
    },
    "write" : {
        "11201" : [
            {"name" : "list", "type" : "list", "comment" : "任务列表", "explain" : [
                {"name" : "questId", "type" : "u32", "comment" : "任务ID", "explain" : []},
                {"name" : "number", "type" : "u16", "comment" : "当前数量", "explain" : []},
                {"name" : "award", "type" : "u8", "comment" : "是否领取奖励", "explain" : []}
            ]}
        ],
        "11202" : [
            {"name" : "result", "type" : "u8", "comment" : "结果", "explain" : []},
            {"name" : "questId", "type" : "u32", "comment" : "任务ID", "explain" : []},
            {"name" : "number", "type" : "u16", "comment" : "当前数量", "explain" : []},
            {"name" : "award", "type" : "u8", "comment" : "是否领取奖励", "explain" : []}
        ],
        "11203" : [
            {"name" : "result", "type" : "u8", "comment" : "结果", "explain" : []}
        ]
    }
};