export default {
    "11701": {
        "comment": "技能列表",
        "write": {"name": "data", "type": "map", "comment": "", "explain": [

        ]},
        "read": {"name": "data", "type": "list", "comment": "技能列表", "explain": [
            {"name": "data", "type": "map", "comment": "", "explain": [
                {"name": "skillId", "type": "u32", "comment": "技能ID", "explain": []},
                {"name": "level", "type": "u16", "comment": "技能等级", "explain": []}
            ]}
        ]}
    },
    "11702": {
        "comment": "学习技能",
        "write": {"name": "data", "type": "u32", "comment": "技能ID", "explain": []},
        "read": {"name": "data", "type": "ast", "comment": "结果", "explain": []}
    }
};