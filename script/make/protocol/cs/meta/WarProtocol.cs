using List = System.Collections.ArrayList;
using Map = System.Collections.Generic.Dictionary<System.String, System.Object>;

public static class WarProtocol
{
    public static Map GetMeta()
    {
        return new Map()
        {
            {"18001", new Map() {
                {"comment", "挑战Boss"},
                {"write", new List() {
                    new Map() { {"name", "monsterId"}, {"type", "u32"}, {"comment", "怪物Id"}, {"explain", new List()} }
                }},
                {"read", new List() {
                    new Map() { {"name", "result"}, {"type", "rst"}, {"comment", "结果"}, {"explain", new List()} }
                }}
            }}
        };
    }
}