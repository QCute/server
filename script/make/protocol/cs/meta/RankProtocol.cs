using List = System.Collections.Generic.List<System.Object>;
using Map = System.Collections.Generic.Dictionary<System.String, System.Object>;

public static class RankProtocol
{
    public static Map GetMeta()
    {
        return new Map()
        {
            {"19001", new Map() {
                {"comment", "等级榜"},
                {"write", new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {

                }}}},
                {"read", new Map() { {"name", "data"}, {"type", "list"}, {"comment", ""}, {"explain", new List() {
                    new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {
                        new Map() { {"name", "type"}, {"type", "u16"}, {"comment", "类型"}, {"explain", new List()} },
                        new Map() { {"name", "order"}, {"type", "u64"}, {"comment", "排名"}, {"explain", new List()} },
                        new Map() { {"name", "key"}, {"type", "u64"}, {"comment", "键"}, {"explain", new List()} },
                        new Map() { {"name", "value"}, {"type", "u64"}, {"comment", "值"}, {"explain", new List()} },
                        new Map() { {"name", "time"}, {"type", "u32"}, {"comment", "时间"}, {"explain", new List()} },
                        new Map() { {"name", "name"}, {"type", "bst"}, {"comment", "名字"}, {"explain", new List()} },
                        new Map() { {"name", "serverId"}, {"type", "u16"}, {"comment", "服务器ID"}, {"explain", new List()} }
                    }}}
                }}}}
            }},
            {"19002", new Map() {
                {"comment", "战力榜"},
                {"write", new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {

                }}}},
                {"read", new Map() { {"name", "data"}, {"type", "list"}, {"comment", ""}, {"explain", new List() {
                    new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {
                        new Map() { {"name", "type"}, {"type", "u16"}, {"comment", "类型"}, {"explain", new List()} },
                        new Map() { {"name", "order"}, {"type", "u64"}, {"comment", "排名"}, {"explain", new List()} },
                        new Map() { {"name", "key"}, {"type", "u64"}, {"comment", "键"}, {"explain", new List()} },
                        new Map() { {"name", "value"}, {"type", "u64"}, {"comment", "值"}, {"explain", new List()} },
                        new Map() { {"name", "time"}, {"type", "u32"}, {"comment", "时间"}, {"explain", new List()} },
                        new Map() { {"name", "name"}, {"type", "bst"}, {"comment", "名字"}, {"explain", new List()} },
                        new Map() { {"name", "serverId"}, {"type", "u16"}, {"comment", "服务器ID"}, {"explain", new List()} },
                        new Map() { {"name", "other"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {
                            new Map() { {"name", "level"}, {"type", "u16"}, {"comment", "等级"}, {"explain", new List()} },
                            new Map() { {"name", "classes"}, {"type", "u8"}, {"comment", "职业"}, {"explain", new List()} }
                        }}}
                    }}}
                }}}}
            }},
            {"19003", new Map() {
                {"comment", "成就榜"},
                {"write", new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {

                }}}},
                {"read", new Map() { {"name", "data"}, {"type", "list"}, {"comment", ""}, {"explain", new List() {
                    new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {
                        new Map() { {"name", "type"}, {"type", "u16"}, {"comment", "类型"}, {"explain", new List()} },
                        new Map() { {"name", "order"}, {"type", "u64"}, {"comment", "排名"}, {"explain", new List()} },
                        new Map() { {"name", "key"}, {"type", "u64"}, {"comment", "键"}, {"explain", new List()} },
                        new Map() { {"name", "value"}, {"type", "u64"}, {"comment", "值"}, {"explain", new List()} },
                        new Map() { {"name", "time"}, {"type", "u32"}, {"comment", "时间"}, {"explain", new List()} },
                        new Map() { {"name", "name"}, {"type", "bst"}, {"comment", "名字"}, {"explain", new List()} },
                        new Map() { {"name", "serverId"}, {"type", "u16"}, {"comment", "服务器ID"}, {"explain", new List()} },
                        new Map() { {"name", "other"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {
                            new Map() { {"name", "level"}, {"type", "u16"}, {"comment", "等级"}, {"explain", new List()} },
                            new Map() { {"name", "classes"}, {"type", "u8"}, {"comment", "职业"}, {"explain", new List()} },
                            new Map() { {"name", "sex"}, {"type", "u8"}, {"comment", "性别"}, {"explain", new List()} }
                        }}}
                    }}}
                }}}}
            }},
            {"19004", new Map() {
                {"comment", "财富榜"},
                {"write", new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {

                }}}},
                {"read", new Map() { {"name", "data"}, {"type", "list"}, {"comment", ""}, {"explain", new List() {
                    new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {
                        new Map() { {"name", "type"}, {"type", "u16"}, {"comment", "类型"}, {"explain", new List()} },
                        new Map() { {"name", "order"}, {"type", "u64"}, {"comment", "排名"}, {"explain", new List()} },
                        new Map() { {"name", "key"}, {"type", "u64"}, {"comment", "键"}, {"explain", new List()} },
                        new Map() { {"name", "value"}, {"type", "u64"}, {"comment", "值"}, {"explain", new List()} },
                        new Map() { {"name", "time"}, {"type", "u32"}, {"comment", "时间"}, {"explain", new List()} },
                        new Map() { {"name", "name"}, {"type", "bst"}, {"comment", "名字"}, {"explain", new List()} },
                        new Map() { {"name", "serverId"}, {"type", "u16"}, {"comment", "服务器ID"}, {"explain", new List()} },
                        new Map() { {"name", "other"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {
                            new Map() { {"name", "level"}, {"type", "u16"}, {"comment", "等级"}, {"explain", new List()} },
                            new Map() { {"name", "classes"}, {"type", "u8"}, {"comment", "职业"}, {"explain", new List()} },
                            new Map() { {"name", "sex"}, {"type", "u8"}, {"comment", "性别"}, {"explain", new List()} },
                            new Map() { {"name", "vipLevel"}, {"type", "u8"}, {"comment", "VIP等级"}, {"explain", new List()} }
                        }}}
                    }}}
                }}}}
            }},
            {"19005", new Map() {
                {"comment", "职业榜"},
                {"write", new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {

                }}}},
                {"read", new Map() { {"name", "data"}, {"type", "list"}, {"comment", ""}, {"explain", new List() {
                    new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {
                        new Map() { {"name", "type"}, {"type", "u16"}, {"comment", "类型"}, {"explain", new List()} },
                        new Map() { {"name", "order"}, {"type", "u64"}, {"comment", "排名"}, {"explain", new List()} },
                        new Map() { {"name", "key"}, {"type", "u64"}, {"comment", "键"}, {"explain", new List()} },
                        new Map() { {"name", "value"}, {"type", "u64"}, {"comment", "值"}, {"explain", new List()} },
                        new Map() { {"name", "time"}, {"type", "u32"}, {"comment", "时间"}, {"explain", new List()} },
                        new Map() { {"name", "name"}, {"type", "bst"}, {"comment", "名字"}, {"explain", new List()} },
                        new Map() { {"name", "serverId"}, {"type", "u16"}, {"comment", "服务器ID"}, {"explain", new List()} },
                        new Map() { {"name", "other"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {
                            new Map() { {"name", "level"}, {"type", "u16"}, {"comment", "等级"}, {"explain", new List()} },
                            new Map() { {"name", "classes"}, {"type", "u8"}, {"comment", "职业"}, {"explain", new List()} },
                            new Map() { {"name", "sex"}, {"type", "u8"}, {"comment", "性别"}, {"explain", new List()} },
                            new Map() { {"name", "vipLevel"}, {"type", "u8"}, {"comment", "VIP等级"}, {"explain", new List()} },
                            new Map() { {"name", "avatar"}, {"type", "u8"}, {"comment", "头像"}, {"explain", new List()} }
                        }}}
                    }}}
                }}}}
            }}
        };
    }
}