using List = System.Collections.Generic.List<System.Object>;
using Map = System.Collections.Generic.Dictionary<System.String, System.Object>;

public static class FriendProtocol
{
    public static Map GetMeta()
    {
        return new Map()
        {
            {"11501", new Map() {
                {"comment", "好友列表"},
                {"write", new List() {
                    new Map() { {"name", "data"}, {"type", "tuple"}, {"comment": ""}, {"explain": new List() {

                    }}}
                }},
                {"read", new List() {
                    new Map() { {"name", "data"}, {"type", "list"}, {"comment", ""}, {"explain", new List() {
                        new Map() { {"name", "friend"}, {"type", "record"}, {"comment": ""}, {"explain": new List() {
                            new Map() { {"name", "friendRoleId"}, {"type", "u64"}, {"comment", "好友角色ID"}, {"explain", new List()} },
                            new Map() { {"name", "friendName"}, {"type", "bst"}, {"comment", "好友名字"}, {"explain", new List()} },
                            new Map() { {"name", "relation"}, {"type", "u8"}, {"comment", "关系状态(申请:1/好友:2/黑名单:3)"}, {"explain", new List()} },
                            new Map() { {"name", "time"}, {"type", "u32"}, {"comment", "添加/修改状态时间"}, {"explain", new List()} }
                        }}}
                    }}}
                }}
            }},
            {"11502", new Map() {
                {"comment", "申请"},
                {"write", new List() {
                    new Map() { {"name", "data"}, {"type", "u64"}, {"comment", "好友角色ID"}, {"explain", new List()} }
                }},
                {"read", new List() {
                    new Map() { {"name", "data"}, {"type", "rst"}, {"comment", "结果"}, {"explain", new List()} }
                }}
            }},
            {"11503", new Map() {
                {"comment", "同意"},
                {"write", new List() {
                    new Map() { {"name", "data"}, {"type", "u64"}, {"comment", "好友角色ID"}, {"explain", new List()} }
                }},
                {"read", new List() {
                    new Map() { {"name", "data"}, {"type", "rst"}, {"comment", "结果"}, {"explain", new List()} }
                }}
            }},
            {"11504", new Map() {
                {"comment", "删除"},
                {"write", new List() {
                    new Map() { {"name", "data"}, {"type", "u64"}, {"comment", "好友角色ID"}, {"explain", new List()} }
                }},
                {"read", new List() {
                    new Map() { {"name", "data"}, {"type", "tuple"}, {"comment": ""}, {"explain": new List() {
                        new Map() { {"name", "result"}, {"type", "rst"}, {"comment", "结果"}, {"explain", new List()} },
                        new Map() { {"name", "friendRoleId"}, {"type", "u64"}, {"comment", "好友角色ID"}, {"explain", new List()} }
                    }}}
                }}
            }},
            {"11505", new Map() {
                {"comment", "拉黑"},
                {"write", new List() {
                    new Map() { {"name", "data"}, {"type", "u64"}, {"comment", "好友角色ID"}, {"explain", new List()} }
                }},
                {"read", new List() {
                    new Map() { {"name", "data"}, {"type", "tuple"}, {"comment": ""}, {"explain": new List() {
                        new Map() { {"name", "result"}, {"type", "rst"}, {"comment", "结果"}, {"explain", new List()} },
                        new Map() { {"name", "friendRoleId"}, {"type", "u64"}, {"comment", "好友角色ID"}, {"explain", new List()} }
                    }}}
                }}
            }},
            {"11506", new Map() {
                {"comment", "取消拉黑"},
                {"write", new List() {
                    new Map() { {"name", "data"}, {"type", "u64"}, {"comment", "好友角色ID"}, {"explain", new List()} }
                }},
                {"read", new List() {
                    new Map() { {"name", "data"}, {"type", "tuple"}, {"comment": ""}, {"explain": new List() {
                        new Map() { {"name", "result"}, {"type", "rst"}, {"comment", "结果"}, {"explain", new List()} },
                        new Map() { {"name", "friendRoleId"}, {"type", "u64"}, {"comment", "好友角色ID"}, {"explain", new List()} }
                    }}}
                }}
            }}
        };
    }
}