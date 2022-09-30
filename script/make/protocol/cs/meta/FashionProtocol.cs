using List = System.Collections.ArrayList;
using Map = System.Collections.Generic.Dictionary<System.String, System.Object>;

public static class FashionProtocol
{
    public static Map GetMeta()
    {
        return new Map()
        {
            {"12001", new Map() {
                {"comment", "时装列表"},
                {"write", new List() },
                {"read", new List() {
                    new Map() { {"name", "list"}, {"type", "list"}, {"comment", "时装列表"}, {"explain", new List() {
                        new Map() { {"name", "fashionId"}, {"type", "u32"}, {"comment", "时装ID"}, {"explain", new List()} },
                        new Map() { {"name", "expireTime"}, {"type", "u32"}, {"comment", "过期时间"}, {"explain", new List()} }
                    }}}
                }}
            }},
            {"12002", new Map() {
                {"comment", "删除时装"},
                {"write", new List() },
                {"read", new List() {
                    new Map() { {"name", "list"}, {"type", "list"}, {"comment", "时装ID列表"}, {"explain", new List() {
                        new Map() { {"name", "fashionId"}, {"type", "u32"}, {"comment", "时装ID"}, {"explain", new List()} }
                    }}}
                }}
            }}
        };
    }
}