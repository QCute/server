export default {
    "12001": {
        "comment": "时装列表",
        "write": {"name": "data", "type": "map", "comment": "", "explain": [

        ]},
        "read": {"name": "data", "type": "list", "comment": "时装列表", "explain": [
            {"name": "data", "type": "map", "comment": "", "explain": [
                {"name": "fashionId", "type": "u32", "comment": "时装ID", "explain": []},
                {"name": "expireTime", "type": "u32", "comment": "过期时间", "explain": []}
            ]}
        ]}
    }
};