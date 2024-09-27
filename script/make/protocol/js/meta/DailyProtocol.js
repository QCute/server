export default {
    "12303" : {
        "comment" : "领取日常奖励",
        "write" : [
            {"name": "dailyId", "type": "u32", "comment": "日常ID", "explain": []}
        ],
        "read" : [
            {"name": "result", "type": "rst", "comment": "结果", "explain": []}
        ]
    },
    "12304" : {
        "comment" : "领取活跃度阶段奖励",
        "write" : [
            {"name": "stageId", "type": "u32", "comment": "阶段ID", "explain": []}
        ],
        "read" : [
            {"name": "result", "type": "rst", "comment": "结果", "explain": []}
        ]
    }
};