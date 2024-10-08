using List = System.Collections.Generic.List<System.Object>;
using Map = System.Collections.Generic.Dictionary<System.String, System.Object>;

public static class ShopProtocol
{
    public static Map GetMeta()
    {
        return new Map()
        {
            {"11302", new Map() {
                {"comment", "购买"},
                {"write", new List() {
                    new Map() { {"name", "shopId"}, {"type", "u32"}, {"comment", "商店ID"}, {"explain", new List()} },
                    new Map() { {"name", "number"}, {"type", "u16"}, {"comment", "数量"}, {"explain", new List()} }
                }},
                {"read", new List() {
                    new Map() { {"name", "result"}, {"type", "rst"}, {"comment", "结果"}, {"explain", new List()} }
                }}
            }}
        };
    }
}