export default {
    "50001" : {
        "comment" : "公告列表",
        "write" : [],
        "read" : [
            {"name" : "noticeList", "type" : "list", "comment" : "公告列表", "explain" : [
                {"name" : "noticeId", "type" : "u64", "comment" : "公告ID", "explain" : []},
                {"name" : "receiveTime", "type" : "u32", "comment" : "收到时间", "explain" : []},
                {"name" : "readTime", "type" : "u32", "comment" : "读取时间", "explain" : []},
                {"name" : "title", "type" : "bst", "comment" : "标题", "explain" : []},
                {"name" : "content", "type" : "bst", "comment" : "内容", "explain" : []}
            ]}
        ]
    },
    "50002" : {
        "comment" : "公告",
        "write" : [],
        "read" : [
            {"name" : "scope", "type" : "u8", "comment" : "范围", "explain" : []},
            {"name" : "type", "type" : "u8", "comment" : "类型", "explain" : []},
            {"name" : "title", "type" : "bst", "comment" : "标题", "explain" : []},
            {"name" : "msg", "type" : "bst", "comment" : "消息", "explain" : []}
        ]
    }
};