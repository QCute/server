using List = System.Collections.Generic.List<System.Object>;
using Map = System.Collections.Generic.Dictionary<System.String, System.Object>;

public static class FashionProtocol
{
    public static Map GetMeta()
    {
        return new Map()
        {
            {"12001", new Map() {
                {"comment", "时装列表"},
                {"write", new List() {
                    new Map() { {"name", "data"}, {"type", "tuple"}, {"comment": ""}, {"explain": new List() {

                    }}}
                }},
                {"read", new List() {
                    new Map() { {"name", "data"}, {"type", "list"}, {"comment", "时装列表"}, {"explain", 
                        new Map() { {"name", "fashion"}, {"type", "record"}, {"comment": ""}, {"explain": new List() {
                            new Map() { {"name", "fashionId"}, {"type", "u32"}, {"comment", "时装ID"}, {"explain", new List()} },
                            new Map() { {"name", "expireTime"}, {"type", "u32"}, {"comment", "过期时间"}, {"explain", new List()} }
                        }}}
                    }}
                }}
            }}
        };
    }
}