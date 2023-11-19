export default {
    "12301": {
        "comment": "日常活跃",
        "write": {"name": "data", "type": "map", "comment": "", "explain": [

        ]},
        "read": {"name": "data", "type": "map", "comment": "", "explain": [
            {"name": "stageId", "type": "u32", "comment": "奖励阶段ID", "explain": []},
            {"name": "score", "type": "u32", "comment": "活跃度", "explain": []}
        ]}
    },
    "12302": {
        "comment": "日常列表",
        "write": {"name": "data", "type": "map", "comment": "", "explain": [

        ]},
        "read": {"name": "data", "type": "list", "comment": "", "explain": [
            {"name": "data", "type": "map", "comment": "", "explain": [
                {"name": "dailyId", "type": "u32", "comment": "日常ID", "explain": []},
                {"name": "isAward", "type": "u8", "comment": "是否领取奖励", "explain": []}
            ]}
        ]}
    },
    "12303": {
        "comment": "领取日常奖励",
        "write": {"name": "data", "type": "u32", "comment": "日常ID", "explain": []},
        "read": {"name": "data", "type": "ast", "comment": "结果", "explain": []}
    },
    "12304": {
        "comment": "领取活跃度阶段奖励",
        "write": {"name": "data", "type": "u32", "comment": "阶段ID", "explain": []},
        "read": {"name": "data", "type": "ast", "comment": "结果", "explain": []}
    }
};