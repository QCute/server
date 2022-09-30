using List = System.Collections.ArrayList;
using Map = System.Collections.Generic.Dictionary<System.String, System.Object>;

public static class ItemProtocol
{
    public static Map GetMeta()
    {
        return new Map()
        {
            {"11101", new Map() {
                {"comment", "道具列表"},
                {"write", new List() },
                {"read", new List() {
                    new Map() { {"name", "list"}, {"type", "list"}, {"comment", "道具列表"}, {"explain", new List() {
                        new Map() { {"name", "itemNo"}, {"type", "u64"}, {"comment", "物品编号"}, {"explain", new List()} },
                        new Map() { {"name", "itemId"}, {"type", "u32"}, {"comment", "物品ID"}, {"explain", new List()} },
                        new Map() { {"name", "type"}, {"type", "u8"}, {"comment", "类型"}, {"explain", new List()} },
                        new Map() { {"name", "number"}, {"type", "u16"}, {"comment", "数量"}, {"explain", new List()} }
                    }}}
                }}
            }},
            {"11102", new Map() {
                {"comment", "背包列表"},
                {"write", new List() },
                {"read", new List() {
                    new Map() { {"name", "list"}, {"type", "list"}, {"comment", "背包列表"}, {"explain", new List() {
                        new Map() { {"name", "itemNo"}, {"type", "u64"}, {"comment", "物品编号"}, {"explain", new List()} },
                        new Map() { {"name", "itemId"}, {"type", "u32"}, {"comment", "物品ID"}, {"explain", new List()} },
                        new Map() { {"name", "type"}, {"type", "u8"}, {"comment", "类型"}, {"explain", new List()} },
                        new Map() { {"name", "number"}, {"type", "u16"}, {"comment", "数量"}, {"explain", new List()} }
                    }}}
                }}
            }},
            {"11103", new Map() {
                {"comment", "仓库列表"},
                {"write", new List() },
                {"read", new List() {
                    new Map() { {"name", "list"}, {"type", "list"}, {"comment", "仓库列表"}, {"explain", new List() {
                        new Map() { {"name", "itemNo"}, {"type", "u64"}, {"comment", "物品编号"}, {"explain", new List()} },
                        new Map() { {"name", "itemId"}, {"type", "u32"}, {"comment", "物品ID"}, {"explain", new List()} },
                        new Map() { {"name", "type"}, {"type", "u8"}, {"comment", "类型"}, {"explain", new List()} },
                        new Map() { {"name", "number"}, {"type", "u16"}, {"comment", "数量"}, {"explain", new List()} }
                    }}}
                }}
            }},
            {"11104", new Map() {
                {"comment", "删除物品"},
                {"write", new List() },
                {"read", new List() {
                    new Map() { {"name", "list"}, {"type", "list"}, {"comment", "删除列表"}, {"explain", new List() {
                        new Map() { {"name", "itemNo"}, {"type", "u64"}, {"comment", "物品编号"}, {"explain", new List()} },
                        new Map() { {"name", "type"}, {"type", "u8"}, {"comment", "类型"}, {"explain", new List()} }
                    }}}
                }}
            }},
            {"11106", new Map() {
                {"comment", "使用物品"},
                {"write", new List() {
                    new Map() { {"name", "itemNo"}, {"type", "u64"}, {"comment", "物品编号"}, {"explain", new List()} },
                    new Map() { {"name", "number"}, {"type", "u16"}, {"comment", "数量"}, {"explain", new List()} },
                    new Map() { {"name", "type"}, {"type", "u8"}, {"comment", "类型"}, {"explain", new List()} }
                }},
                {"read", new List() {
                    new Map() { {"name", "result"}, {"type", "rst"}, {"comment", "结果"}, {"explain", new List()} }
                }}
            }}
        };
    }
}