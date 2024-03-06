using List = System.Collections.ArrayList;
using Map = System.Collections.Generic.Dictionary<System.String, System.Object>;

public static class SkillProtocol
{
    public static Map GetMeta()
    {
        return new Map()
        {
            {"11702", new Map() {
                {"comment", "学习技能"},
                {"write", new List() {
                    new Map() { {"name", "skillId"}, {"type", "u32"}, {"comment", "技能ID"}, {"explain", new List()} }
                }},
                {"read", new List() {
                    new Map() { {"name", "result"}, {"type", "rst"}, {"comment", "结果"}, {"explain", new List()} }
                }}
            }}
        };
    }
}