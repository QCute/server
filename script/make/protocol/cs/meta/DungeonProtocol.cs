using List = System.Collections.Generic.List<System.Object>;
using Map = System.Collections.Generic.Dictionary<System.String, System.Object>;

public static class DungeonProtocol
{
    public static Map GetMeta()
    {
        return new Map()
        {
            {"17002", new Map() {
                {"comment", "进入副本"},
                {"write", new List() {
                    new Map() { {"name", "dungeonId"}, {"type", "u32"}, {"comment", "副本Id"}, {"explain", new List()} }
                }},
                {"read", new List() {
                    new Map() { {"name", "result"}, {"type", "rst"}, {"comment", "结果"}, {"explain", new List()} }
                }}
            }}
        };
    }
}