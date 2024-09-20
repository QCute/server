using List = System.Collections.Generic.List<System.Object>;
using Map = System.Collections.Generic.Dictionary<System.String, System.Object>;

public static class AchievementProtocol
{
    public static Map GetMeta()
    {
        return new Map()
        {
            {"12203", new Map() {
                {"comment", "提交成就"},
                {"write", new List() {
                    new Map() { {"name", "achievementId"}, {"type", "u32"}, {"comment", "成就ID"}, {"explain", new List()} }
                }},
                {"read", new List() {
                    new Map() { {"name", "result"}, {"type", "rst"}, {"comment", "结果"}, {"explain", new List()} }
                }}
            }}
        };
    }
}