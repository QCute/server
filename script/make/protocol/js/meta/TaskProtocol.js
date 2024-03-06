export default {
    "11202" : {
        "comment" : "接收任务",
        "write" : [
            {"name" : "taskId", "type" : "u32", "comment" : "任务ID", "explain" : []}
        ],
        "read" : [
            {"name" : "result", "type" : "rst", "comment" : "结果", "explain" : []},
            {"name" : "taskId", "type" : "u32", "comment" : "任务ID", "explain" : []},
            {"name" : "number", "type" : "u16", "comment" : "当前数量", "explain" : []},
            {"name" : "isAward", "type" : "u8", "comment" : "是否领取奖励", "explain" : []}
        ]
    },
    "11203" : {
        "comment" : "提交任务",
        "write" : [
            {"name" : "taskId", "type" : "u32", "comment" : "任务ID", "explain" : []}
        ],
        "read" : [
            {"name" : "result", "type" : "rst", "comment" : "结果", "explain" : []}
        ]
    }
};