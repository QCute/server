export default {
    "12301" : {
        "comment" : "统计列表",
        "write" : [],
        "read" : [
            {"name" : "list", "type" : "list", "comment" : "统计列表", "explain" : [
                {"name" : "type", "type" : "u32", "comment" : "统计类型", "explain" : []},
                {"name" : "todayNumber", "type" : "u32", "comment" : "今日数量", "explain" : []}
            ]}
        ]
    },
    "12302" : {
        "comment" : "日常列表",
        "write" : [],
        "read" : [
            {"name" : "list", "type" : "list", "comment" : "日常列表", "explain" : [
                {"name" : "dailyId", "type" : "u32", "comment" : "日常ID", "explain" : []},
                {"name" : "isAward", "type" : "u8", "comment" : "是否领取奖励", "explain" : []}
            ]},
            {"name" : "stageId", "type" : "u32", "comment" : "奖励阶段ID", "explain" : []},
            {"name" : "score", "type" : "u32", "comment" : "活跃度", "explain" : []}
        ]
    },
    "12303" : {
        "comment" : "领取日常奖励",
        "write" : [
            {"name" : "dailyId", "type" : "u32", "comment" : "日常ID", "explain" : []}
        ],
        "read" : [
            {"name" : "result", "type" : "rst", "comment" : "结果", "explain" : []}
        ]
    },
    "12304" : {
        "comment" : "领取活跃度阶段奖励",
        "write" : [
            {"name" : "stageId", "type" : "u32", "comment" : "阶段ID", "explain" : []}
        ],
        "read" : [
            {"name" : "result", "type" : "rst", "comment" : "结果", "explain" : []}
        ]
    }
};