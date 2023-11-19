using List = System.Collections.Generic.List<System.Object>;
using Map = System.Collections.Generic.Dictionary<System.String, System.Object>;

public static class BuffProtocol
{
    public static Map GetMeta()
    {
        return new Map()
        {
            {"11801", new Map() {
                {"comment", "Buff列表"},
                {"write", new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {

                }}}},
                {"read", new Map() { {"name", "data"}, {"type", "list"}, {"comment", "Buff列表"}, {"explain", new List() {
                    new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {
                        new Map() { {"name", "buffId"}, {"type", "u32"}, {"comment", "BuffID"}, {"explain", new List()} },
                        new Map() { {"name", "expireTime"}, {"type", "u32"}, {"comment", "结束时间"}, {"explain", new List()} },
                        new Map() { {"name", "overlap"}, {"type", "u16"}, {"comment", "叠加数量"}, {"explain", new List()} }
                    }}}
                }}}}
            }}
        };
    }
}