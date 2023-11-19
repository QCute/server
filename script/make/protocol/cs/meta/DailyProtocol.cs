using List = System.Collections.Generic.List<System.Object>;
using Map = System.Collections.Generic.Dictionary<System.String, System.Object>;

public static class DailyProtocol
{
    public static Map GetMeta()
    {
        return new Map()
        {
            {"12301", new Map() {
                {"comment", "日常活跃"},
                {"write", new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {

                }}}},
                {"read", new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {
                    new Map() { {"name", "stageId"}, {"type", "u32"}, {"comment", "奖励阶段ID"}, {"explain", new List()} },
                    new Map() { {"name", "score"}, {"type", "u32"}, {"comment", "活跃度"}, {"explain", new List()} }
                }}}}
            }},
            {"12302", new Map() {
                {"comment", "日常列表"},
                {"write", new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {

                }}}},
                {"read", new Map() { {"name", "data"}, {"type", "list"}, {"comment", ""}, {"explain", new List() {
                    new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {
                        new Map() { {"name", "dailyId"}, {"type", "u32"}, {"comment", "日常ID"}, {"explain", new List()} },
                        new Map() { {"name", "isAward"}, {"type", "u8"}, {"comment", "是否领取奖励"}, {"explain", new List()} }
                    }}}
                }}}}
            }},
            {"12303", new Map() {
                {"comment", "领取日常奖励"},
                {"write", new Map() { {"name", "data"}, {"type", "u32"}, {"comment", "日常ID"}, {"explain", new List()} }},
                {"read", new Map() { {"name", "data"}, {"type", "ast"}, {"comment", "结果"}, {"explain", new List()} }}
            }},
            {"12304", new Map() {
                {"comment", "领取活跃度阶段奖励"},
                {"write", new Map() { {"name", "data"}, {"type", "u32"}, {"comment", "阶段ID"}, {"explain", new List()} }},
                {"read", new Map() { {"name", "data"}, {"type", "ast"}, {"comment", "结果"}, {"explain", new List()} }}
            }}
        };
    }
}