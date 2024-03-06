export default {
    "50001" : {
        "comment" : "公告列表",
        "write" : [
            {"name": "data", "type": "tuple", "comment": "", "explain": [

            ]}
        ],
        "read" : [
            {"name": "data", "type": "list", "comment": "公告列表", "explain": [
                {"name": "noticeRole", "type": "record", "comment": "", "explain": [
                    {"name": "noticeId", "type": "u64", "comment": "公告ID", "explain": []},
                    {"name": "receiveTime", "type": "u32", "comment": "收到时间", "explain": []},
                    {"name": "readTime", "type": "u32", "comment": "读取时间", "explain": []},
                    {"name": "title", "type": "bst", "comment": "标题", "explain": []},
                    {"name": "content", "type": "bst", "comment": "内容", "explain": []}
                ]}
            ]}
        ]
    }
};