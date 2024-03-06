using List = System.Collections.Generic.List<System.Object>;
using Map = System.Collections.Generic.Dictionary<System.String, System.Object>;

public static class ItemProtocol
{
    public static Map GetMeta()
    {
        return new Map()
        {
            {"11101", new Map() {
                {"comment", "道具列表"},
                {"write", new List() {
                    new Map() { {"name", "data"}, {"type", "tuple"}, {"comment": ""}, {"explain": new List() {

                    }}}
                }},
                {"read", new List() {
                    new Map() { {"name", "data"}, {"type", "list"}, {"comment", ""}, {"explain", new List() {
                        new Map() { {"name", "item"}, {"type", "record"}, {"comment": ""}, {"explain": new List() {
                            new Map() { {"name", "itemNo"}, {"type", "u64"}, {"comment", "物品编号"}, {"explain", new List()} },
                            new Map() { {"name", "itemId"}, {"type", "u64"}, {"comment", "物品ID"}, {"explain", new List()} },
                            new Map() { {"name", "type"}, {"type", "u8"}, {"comment", "类型"}, {"explain", new List()} },
                            new Map() { {"name", "number"}, {"type", "u16"}, {"comment", "数量"}, {"explain", new List()} }
                        }}}
                    }}}
                }}
            }},
            {"11102", new Map() {
                {"comment", "背包列表"},
                {"write", new List() {
                    new Map() { {"name", "data"}, {"type", "tuple"}, {"comment": ""}, {"explain": new List() {

                    }}}
                }},
                {"read", new List() {
                    new Map() { {"name", "data"}, {"type", "list"}, {"comment", ""}, {"explain", new List() {
                        new Map() { {"name", "item"}, {"type", "record"}, {"comment": ""}, {"explain": new List() {
                            new Map() { {"name", "itemNo"}, {"type", "u64"}, {"comment", "物品编号"}, {"explain", new List()} },
                            new Map() { {"name", "itemId"}, {"type", "u64"}, {"comment", "物品ID"}, {"explain", new List()} },
                            new Map() { {"name", "type"}, {"type", "u8"}, {"comment", "类型"}, {"explain", new List()} },
                            new Map() { {"name", "number"}, {"type", "u16"}, {"comment", "数量"}, {"explain", new List()} }
                        }}}
                    }}}
                }}
            }},
            {"11103", new Map() {
                {"comment", "仓库列表"},
                {"write", new List() {
                    new Map() { {"name", "data"}, {"type", "tuple"}, {"comment": ""}, {"explain": new List() {

                    }}}
                }},
                {"read", new List() {
                    new Map() { {"name", "data"}, {"type", "list"}, {"comment", ""}, {"explain", new List() {
                        new Map() { {"name", "item"}, {"type", "record"}, {"comment": ""}, {"explain": new List() {
                            new Map() { {"name", "itemNo"}, {"type", "u64"}, {"comment", "物品编号"}, {"explain", new List()} },
                            new Map() { {"name", "itemId"}, {"type", "u64"}, {"comment", "物品ID"}, {"explain", new List()} },
                            new Map() { {"name", "type"}, {"type", "u8"}, {"comment", "类型"}, {"explain", new List()} },
                            new Map() { {"name", "number"}, {"type", "u16"}, {"comment", "数量"}, {"explain", new List()} }
                        }}}
                    }}}
                }}
            }},
            {"11106", new Map() {
                {"comment", "使用物品"},
                {"write", new List() {
                    new Map() { {"name", "data"}, {"type", "tuple"}, {"comment": ""}, {"explain": new List() {
                        new Map() { {"name", "itemNo"}, {"type", "u64"}, {"comment", "物品编号"}, {"explain", new List()} },
                        new Map() { {"name", "number"}, {"type", "u16"}, {"comment", "数量"}, {"explain", new List()} },
                        new Map() { {"name", "type"}, {"type", "u8"}, {"comment", "类型"}, {"explain", new List()} }
                    }}}
                }},
                {"read", new List() {
                    new Map() { {"name", "data"}, {"type", "rst"}, {"comment", "结果"}, {"explain", new List()} }
                }}
            }}
        };
    }
}