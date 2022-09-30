using List = System.Collections.ArrayList;
using Map = System.Collections.Generic.Dictionary<System.String, System.Object>;

public static class ShopProtocol
{
    public static Map GetMeta()
    {
        return new Map()
        {
            {"11301", new Map() {
                {"comment", "已购列表"},
                {"write", new List() },
                {"read", new List() {
                    new Map() { {"name", "list"}, {"type", "list"}, {"comment", "已购买列表"}, {"explain", new List() {
                        new Map() { {"name", "shopId"}, {"type", "u32"}, {"comment", "商店ID"}, {"explain", new List()} },
                        new Map() { {"name", "number"}, {"type", "u16"}, {"comment", "数量"}, {"explain", new List()} }
                    }}}
                }}
            }},
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