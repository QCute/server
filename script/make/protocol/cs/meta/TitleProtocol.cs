using List = System.Collections.ArrayList;
using Map = System.Collections.Generic.Dictionary<System.String, System.Object>;

public static class TitleProtocol
{
    public static Map GetMeta()
    {
        return new Map()
        {
            {"11901", new Map() {
                {"comment", "称号列表"},
                {"write", new List() },
                {"read", new List() {
                    new Map() { {"name", "list"}, {"type", "list"}, {"comment", "称号列表"}, {"explain", new List() {
                        new Map() { {"name", "titleId"}, {"type", "u32"}, {"comment", "称号ID"}, {"explain", new List()} },
                        new Map() { {"name", "expireTime"}, {"type", "u32"}, {"comment", "过期时间"}, {"explain", new List()} }
                    }}}
                }}
            }},
            {"11902", new Map() {
                {"comment", "删除称号"},
                {"write", new List() },
                {"read", new List() {
                    new Map() { {"name", "list"}, {"type", "list"}, {"comment", "称号ID列表"}, {"explain", new List() {
                        new Map() { {"name", "titleId"}, {"type", "u32"}, {"comment", "称号ID"}, {"explain", new List()} }
                    }}}
                }}
            }}
        };
    }
}