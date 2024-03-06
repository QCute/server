using List = System.Collections.Generic.List<System.Object>;
using Map = System.Collections.Generic.Dictionary<System.String, System.Object>;

public static class TaskProtocol
{
    public static Map GetMeta()
    {
        return new Map()
        {
            {"11201", new Map() {
                {"comment", "任务列表"},
                {"write", new List() {
                    new Map() { {"name", "data"}, {"type", "tuple"}, {"comment": ""}, {"explain": new List() {

                    }}}
                }},
                {"read", new List() {
                    new Map() { {"name", "data"}, {"type", "list"}, {"comment", "任务列表"}, {"explain", 
                        new Map() { {"name", "task"}, {"type", "record"}, {"comment": ""}, {"explain": new List() {
                            new Map() { {"name", "taskId"}, {"type", "u32"}, {"comment", "任务ID"}, {"explain", new List()} },
                            new Map() { {"name", "number"}, {"type", "u16"}, {"comment", "当前数量"}, {"explain", new List()} },
                            new Map() { {"name", "isAward"}, {"type", "u8"}, {"comment", "是否领取奖励"}, {"explain", new List()} }
                        }}}
                    }}
                }}
            }},
            {"11202", new Map() {
                {"comment", "接收任务"},
                {"write", new List() {
                    new Map() { {"name", "data"}, {"type", "u32"}, {"comment", "任务ID"}, {"explain", new List()} }
                }},
                {"read", new List() {
                    new Map() { {"name", "data"}, {"type", "tuple"}, {"comment": ""}, {"explain": new List() {
                        new Map() { {"name", "result"}, {"type", "rst"}, {"comment", "结果"}, {"explain", new List()} },
                        new Map() { {"name", "task"}, {"type", "record"}, {"comment": ""}, {"explain": new List() {
                            new Map() { {"name", "taskId"}, {"type", "u32"}, {"comment", "任务ID"}, {"explain", new List()} },
                            new Map() { {"name", "number"}, {"type", "u16"}, {"comment", "当前数量"}, {"explain", new List()} },
                            new Map() { {"name", "isAward"}, {"type", "u8"}, {"comment", "是否领取奖励"}, {"explain", new List()} }
                        }}}
                    }}}
                }}
            }},
            {"11203", new Map() {
                {"comment", "提交任务"},
                {"write", new List() {
                    new Map() { {"name", "data"}, {"type", "u32"}, {"comment", "任务ID"}, {"explain", new List()} }
                }},
                {"read", new List() {
                    new Map() { {"name", "data"}, {"type", "rst"}, {"comment", "结果"}, {"explain", new List()} }
                }}
            }}
        };
    }
}