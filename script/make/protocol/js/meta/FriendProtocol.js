export default {
    "11502" : {
        "comment" : "申请",
        "write" : [
            {"name": "friendRoleId", "type": "u64", "comment": "好友角色ID", "explain": []}
        ],
        "read" : [
            {"name": "result", "type": "rst", "comment": "结果", "explain": []}
        ]
    },
    "11503" : {
        "comment" : "同意",
        "write" : [
            {"name": "friendRoleId", "type": "u64", "comment": "好友角色ID", "explain": []}
        ],
        "read" : [
            {"name": "result", "type": "rst", "comment": "结果", "explain": []}
        ]
    },
    "11504" : {
        "comment" : "删除",
        "write" : [
            {"name": "friendRoleId", "type": "u64", "comment": "好友角色ID", "explain": []}
        ],
        "read" : [
            {"name": "result", "type": "rst", "comment": "结果", "explain": []},
            {"name": "friendRoleId", "type": "u64", "comment": "好友角色ID", "explain": []}
        ]
    },
    "11505" : {
        "comment" : "拉黑",
        "write" : [
            {"name": "friendRoleId", "type": "u64", "comment": "好友角色ID", "explain": []}
        ],
        "read" : [
            {"name": "result", "type": "rst", "comment": "结果", "explain": []},
            {"name": "friendRoleId", "type": "u64", "comment": "好友角色ID", "explain": []}
        ]
    },
    "11506" : {
        "comment" : "取消拉黑",
        "write" : [
            {"name": "friendRoleId", "type": "u64", "comment": "好友角色ID", "explain": []}
        ],
        "read" : [
            {"name": "result", "type": "rst", "comment": "结果", "explain": []},
            {"name": "friendRoleId", "type": "u64", "comment": "好友角色ID", "explain": []}
        ]
    }
};