export default {
    "60001" : {
        "comment" : "秘籍",
        "write" : [
            {"name": "data", "type": "tuple", "comment": "", "explain": [

            ]}
        ],
        "read" : [
            {"name": "data", "type": "list", "comment": "命令列表", "explain": [
                {"name": "item", "type": "tuple", "comment": "", "explain": [
                    {"name": "description", "type": "bst", "comment": "描述", "explain": []},
                    {"name": "command", "type": "bst", "comment": "命令", "explain": []}
                ]}
            ]}
        ]
    },
    "60002" : {
        "comment" : "秘籍",
        "write" : [
            {"name": "data", "type": "bst", "comment": "命令", "explain": []}
        ],
        "read" : [
            {"name": "data", "type": "rst", "comment": "结果", "explain": []}
        ]
    }
};