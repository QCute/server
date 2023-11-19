using List = System.Collections.Generic.List<System.Object>;
using Map = System.Collections.Generic.Dictionary<System.String, System.Object>;

public static class AchievementProtocol
{
    public static Map GetMeta()
    {
        return new Map()
        {
            {"12201", new Map() {
                {"comment", "统计列表"},
                {"write", new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {

                }}}},
                {"read", new Map() { {"name", "data"}, {"type", "list"}, {"comment", "统计列表"}, {"explain", new List() {
                    new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {
                        new Map() { {"name", "type"}, {"type", "u32"}, {"comment", "统计类型"}, {"explain", new List()} },
                        new Map() { {"name", "totalNumber"}, {"type", "u32"}, {"comment", "总数"}, {"explain", new List()} }
                    }}}
                }}}}
            }},
            {"12202", new Map() {
                {"comment", "成就列表"},
                {"write", new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {

                }}}},
                {"read", new Map() { {"name", "data"}, {"type", "list"}, {"comment", "成就列表"}, {"explain", new List() {
                    new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {
                        new Map() { {"name", "achievementId"}, {"type", "u32"}, {"comment", "成就ID"}, {"explain", new List()} },
                        new Map() { {"name", "type"}, {"type", "u32"}, {"comment", "成就类型"}, {"explain", new List()} }
                    }}}
                }}}}
            }},
            {"12203", new Map() {
                {"comment", "提交成就"},
                {"write", new Map() { {"name", "data"}, {"type", "u32"}, {"comment", "成就ID"}, {"explain", new List()} }},
                {"read", new Map() { {"name", "data"}, {"type", "ast"}, {"comment", "结果"}, {"explain", new List()} }}
            }}
        };
    }
}