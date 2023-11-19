using List = System.Collections.Generic.List<System.Object>;
using Map = System.Collections.Generic.Dictionary<System.String, System.Object>;

public static class WarProtocol
{
    public static Map GetMeta()
    {
        return new Map()
        {
            {"18001", new Map() {
                {"comment", "挑战Boss"},
                {"write", new Map() { {"name", "data"}, {"type", "u32"}, {"comment", "怪物Id"}, {"explain", new List()} }},
                {"read", new Map() { {"name", "data"}, {"type", "ast"}, {"comment", "结果"}, {"explain", new List()} }}
            }}
        };
    }
}