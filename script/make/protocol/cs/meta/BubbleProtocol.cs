using List = System.Collections.Generic.List<System.Object>;
using Map = System.Collections.Generic.Dictionary<System.String, System.Object>;

public static class BubbleProtocol
{
    public static Map GetMeta()
    {
        return new Map()
        {
            {"12101", new Map() {
                {"comment", "气泡列表"},
                {"write", new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {

                }}}},
                {"read", new Map() { {"name", "data"}, {"type", "list"}, {"comment", "气泡列表"}, {"explain", new List() {
                    new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {
                        new Map() { {"name", "bubbleId"}, {"type", "u32"}, {"comment", "气泡ID"}, {"explain", new List()} },
                        new Map() { {"name", "expireTime"}, {"type", "u32"}, {"comment", "过期时间"}, {"explain", new List()} }
                    }}}
                }}}}
            }}
        };
    }
}