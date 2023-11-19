export default {
    "11901": {
        "comment": "称号列表",
        "write": {"name": "data", "type": "map", "comment": "", "explain": [

        ]},
        "read": {"name": "data", "type": "list", "comment": "称号列表", "explain": [
            {"name": "data", "type": "map", "comment": "", "explain": [
                {"name": "titleId", "type": "u32", "comment": "称号ID", "explain": []},
                {"name": "expireTime", "type": "u32", "comment": "过期时间", "explain": []}
            ]}
        ]}
    }
};