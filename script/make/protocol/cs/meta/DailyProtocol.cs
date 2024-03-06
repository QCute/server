using List = System.Collections.ArrayList;
using Map = System.Collections.Generic.Dictionary<System.String, System.Object>;

public static class DailyProtocol
{
    public static Map GetMeta()
    {
        return new Map()
        {
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