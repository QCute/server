using List = System.Collections.ArrayList;
using Map = System.Collections.Generic.Dictionary<System.String, System.Object>;

public static class DailyProtocol
{
    public static Map GetMeta()
    {
        return new Map()
        {
            {"12301", new Map() {
                {"comment", "统计列表"},
                {"write", new List() },
                {"read", new List() {
                    new Map() { {"name", "list"}, {"type", "list"}, {"comment", "统计列表"}, {"explain", new List() {
                        new Map() { {"name", "type"}, {"type", "u32"}, {"comment", "统计类型"}, {"explain", new List()} },
                        new Map() { {"name", "todayNumber"}, {"type", "u32"}, {"comment", "今日数量"}, {"explain", new List()} }
                    }}}
                }}
            }},
            {"12302", new Map() {
                {"comment", "日常列表"},
                {"write", new List() },
                {"read", new List() {
                    new Map() { {"name", "list"}, {"type", "list"}, {"comment", "日常列表"}, {"explain", new List() {
                        new Map() { {"name", "dailyId"}, {"type", "u32"}, {"comment", "日常ID"}, {"explain", new List()} },
                        new Map() { {"name", "isAward"}, {"type", "u8"}, {"comment", "是否领取奖励"}, {"explain", new List()} }
                    }}},
                    new Map() { {"name", "stageId"}, {"type", "u32"}, {"comment", "奖励阶段ID"}, {"explain", new List()} },
                    new Map() { {"name", "score"}, {"type", "u32"}, {"comment", "活跃度"}, {"explain", new List()} }
                }}
            }},
            {"12303", new Map() {
                {"comment", "领取日常奖励"},
                {"write", new List() {
                    new Map() { {"name", "dailyId"}, {"type", "u32"}, {"comment", "日常ID"}, {"explain", new List()} }
                }},
                {"read", new List() {
                    new Map() { {"name", "result"}, {"type", "rst"}, {"comment", "结果"}, {"explain", new List()} }
                }}
            }},
            {"12304", new Map() {
                {"comment", "领取活跃度阶段奖励"},
                {"write", new List() {
                    new Map() { {"name", "stageId"}, {"type", "u32"}, {"comment", "阶段ID"}, {"explain", new List()} }
                }},
                {"read", new List() {
                    new Map() { {"name", "result"}, {"type", "rst"}, {"comment", "结果"}, {"explain", new List()} }
                }}
            }}
        };
    }
}