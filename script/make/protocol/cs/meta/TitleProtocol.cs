using List = System.Collections.Generic.List<System.Object>;
using Map = System.Collections.Generic.Dictionary<System.String, System.Object>;

public static class TitleProtocol
{
    public static Map GetMeta()
    {
        return new Map()
        {
            {"11901", new Map() {
                {"comment", "称号列表"},
                {"write", new List() {
                    new Map() { {"name", "data"}, {"type", "tuple"}, {"comment": ""}, {"explain": new List() {

                    }}}
                }},
                {"read", new List() {
                    new Map() { {"name", "data"}, {"type", "list"}, {"comment", "称号列表"}, {"explain", new List() {
                        new Map() { {"name", "title"}, {"type", "record"}, {"comment": ""}, {"explain": new List() {
                            new Map() { {"name", "titleId"}, {"type", "u32"}, {"comment", "称号ID"}, {"explain", new List()} },
                            new Map() { {"name", "expireTime"}, {"type", "u32"}, {"comment", "过期时间"}, {"explain", new List()} }
                        }}}
                    }}}
                }}
            }}
        };
    }
}