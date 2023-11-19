export default {
    "11501": {
        "comment": "好友列表",
        "write": {"name": "data", "type": "map", "comment": "", "explain": [

        ]},
        "read": {"name": "data", "type": "list", "comment": "", "explain": [
            {"name": "data", "type": "map", "comment": "", "explain": [
                {"name": "friendRoleId", "type": "u64", "comment": "好友角色ID", "explain": []},
                {"name": "friendName", "type": "bst", "comment": "好友名字", "explain": []},
                {"name": "relation", "type": "u8", "comment": "关系状态(申请:1/好友:2/黑名单:3)", "explain": []},
                {"name": "time", "type": "u32", "comment": "添加/修改状态时间", "explain": []}
            ]}
        ]}
    },
    "11502": {
        "comment": "申请",
        "write": {"name": "data", "type": "u64", "comment": "好友角色ID", "explain": []},
        "read": {"name": "data", "type": "ast", "comment": "结果", "explain": []}
    },
    "11503": {
        "comment": "同意",
        "write": {"name": "data", "type": "u64", "comment": "好友角色ID", "explain": []},
        "read": {"name": "data", "type": "ast", "comment": "结果", "explain": []}
    },
    "11504": {
        "comment": "删除",
        "write": {"name": "data", "type": "u64", "comment": "好友角色ID", "explain": []},
        "read": {"name": "data", "type": "map", "comment": "", "explain": [
            {"name": "result", "type": "ast", "comment": "结果", "explain": []},
            {"name": "friendRoleId", "type": "u64", "comment": "好友角色ID", "explain": []}
        ]}
    },
    "11505": {
        "comment": "拉黑",
        "write": {"name": "data", "type": "u64", "comment": "好友角色ID", "explain": []},
        "read": {"name": "data", "type": "map", "comment": "", "explain": [
            {"name": "result", "type": "ast", "comment": "结果", "explain": []},
            {"name": "friendRoleId", "type": "u64", "comment": "好友角色ID", "explain": []}
        ]}
    },
    "11506": {
        "comment": "取消拉黑",
        "write": {"name": "data", "type": "u64", "comment": "好友角色ID", "explain": []},
        "read": {"name": "data", "type": "map", "comment": "", "explain": [
            {"name": "result", "type": "ast", "comment": "结果", "explain": []},
            {"name": "friendRoleId", "type": "u64", "comment": "好友角色ID", "explain": []}
        ]}
    }
};