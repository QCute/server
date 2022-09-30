using List = System.Collections.ArrayList;
using Map = System.Collections.Generic.Dictionary<System.String, System.Object>;

public static class BubbleProtocol
{
    public static Map GetMeta()
    {
        return new Map()
        {
            {"12101", new Map() {
                {"comment", "气泡列表"},
                {"write", new List() },
                {"read", new List() {
                    new Map() { {"name", "list"}, {"type", "list"}, {"comment", "气泡列表"}, {"explain", new List() {
                        new Map() { {"name", "bubbleId"}, {"type", "u32"}, {"comment", "气泡ID"}, {"explain", new List()} },
                        new Map() { {"name", "expireTime"}, {"type", "u32"}, {"comment", "过期时间"}, {"explain", new List()} }
                    }}}
                }}
            }},
            {"12102", new Map() {
                {"comment", "删除气泡"},
                {"write", new List() },
                {"read", new List() {
                    new Map() { {"name", "list"}, {"type", "list"}, {"comment", "气泡ID列表"}, {"explain", new List() {
                        new Map() { {"name", "bubbleId"}, {"type", "u32"}, {"comment", "气泡ID"}, {"explain", new List()} }
                    }}}
                }}
            }}
        };
    }
}