using List = System.Collections.ArrayList;
using Map = System.Collections.Generic.Dictionary<System.String, System.Object>;

public static class ChatProtocol
{
    public static Map GetMeta()
    {
        return new Map()
        {
            {"11602", new Map() {
                {"comment", "系统公告列表"},
                {"write", new List() {
                    new Map() { {"name", "page"}, {"type", "u16"}, {"comment", "页"}, {"explain", new List()} }
                }},
                {"read", new List() {
                    new Map() { {"name", "list"}, {"type", "list"}, {"comment", ""}, {"explain", new List() {
                        new Map() { {"name", "id"}, {"type", "u64"}, {"comment", "ID"}, {"explain", new List()} },
                        new Map() { {"name", "roleId"}, {"type", "u64"}, {"comment", "角色ID"}, {"explain", new List()} },
                        new Map() { {"name", "roleName"}, {"type", "bst"}, {"comment", "角色名字"}, {"explain", new List()} },
                        new Map() { {"name", "type"}, {"type", "u8"}, {"comment", "类型"}, {"explain", new List()} },
                        new Map() { {"name", "message"}, {"type", "bst"}, {"comment", "消息内容"}, {"explain", new List()} }
                    }}}
                }}
            }},
            {"11603", new Map() {
                {"comment", "世界聊天"},
                {"write", new List() {
                    new Map() { {"name", "type"}, {"type", "u8"}, {"comment", "类型"}, {"explain", new List()} },
                    new Map() { {"name", "message"}, {"type", "bst"}, {"comment", "消息"}, {"explain", new List()} }
                }},
                {"read", new List() {
                    new Map() { {"name", "result"}, {"type", "rst"}, {"comment", "结果"}, {"explain", new List()} },
                    new Map() { {"name", "id"}, {"type", "u64"}, {"comment", "ID"}, {"explain", new List()} },
                    new Map() { {"name", "roleId"}, {"type", "u64"}, {"comment", "角色ID"}, {"explain", new List()} },
                    new Map() { {"name", "roleName"}, {"type", "bst"}, {"comment", "角色名字"}, {"explain", new List()} },
                    new Map() { {"name", "type"}, {"type", "u8"}, {"comment", "类型"}, {"explain", new List()} },
                    new Map() { {"name", "message"}, {"type", "bst"}, {"comment", "消息内容"}, {"explain", new List()} }
                }}
            }},
            {"11604", new Map() {
                {"comment", "世界聊天列表"},
                {"write", new List() {
                    new Map() { {"name", "page"}, {"type", "u16"}, {"comment", "页"}, {"explain", new List()} }
                }},
                {"read", new List() {
                    new Map() { {"name", "list"}, {"type", "list"}, {"comment", ""}, {"explain", new List() {
                        new Map() { {"name", "id"}, {"type", "u64"}, {"comment", "ID"}, {"explain", new List()} },
                        new Map() { {"name", "roleId"}, {"type", "u64"}, {"comment", "角色ID"}, {"explain", new List()} },
                        new Map() { {"name", "roleName"}, {"type", "bst"}, {"comment", "角色名字"}, {"explain", new List()} },
                        new Map() { {"name", "type"}, {"type", "u8"}, {"comment", "类型"}, {"explain", new List()} },
                        new Map() { {"name", "message"}, {"type", "bst"}, {"comment", "消息内容"}, {"explain", new List()} }
                    }}}
                }}
            }},
            {"11605", new Map() {
                {"comment", "公会聊天"},
                {"write", new List() {
                    new Map() { {"name", "type"}, {"type", "u8"}, {"comment", "类型"}, {"explain", new List()} },
                    new Map() { {"name", "message"}, {"type", "bst"}, {"comment", "消息"}, {"explain", new List()} }
                }},
                {"read", new List() {
                    new Map() { {"name", "result"}, {"type", "rst"}, {"comment", "结果"}, {"explain", new List()} },
                    new Map() { {"name", "id"}, {"type", "u64"}, {"comment", "ID"}, {"explain", new List()} },
                    new Map() { {"name", "roleId"}, {"type", "u64"}, {"comment", "角色ID"}, {"explain", new List()} },
                    new Map() { {"name", "roleName"}, {"type", "bst"}, {"comment", "角色名字"}, {"explain", new List()} },
                    new Map() { {"name", "type"}, {"type", "u8"}, {"comment", "类型"}, {"explain", new List()} },
                    new Map() { {"name", "message"}, {"type", "bst"}, {"comment", "消息内容"}, {"explain", new List()} }
                }}
            }},
            {"11606", new Map() {
                {"comment", "公会聊天列表"},
                {"write", new List() {
                    new Map() { {"name", "page"}, {"type", "u16"}, {"comment", "页"}, {"explain", new List()} }
                }},
                {"read", new List() {
                    new Map() { {"name", "list"}, {"type", "list"}, {"comment", ""}, {"explain", new List() {
                        new Map() { {"name", "id"}, {"type", "u64"}, {"comment", "ID"}, {"explain", new List()} },
                        new Map() { {"name", "roleId"}, {"type", "u64"}, {"comment", "角色ID"}, {"explain", new List()} },
                        new Map() { {"name", "roleName"}, {"type", "bst"}, {"comment", "角色名字"}, {"explain", new List()} },
                        new Map() { {"name", "type"}, {"type", "u8"}, {"comment", "类型"}, {"explain", new List()} },
                        new Map() { {"name", "message"}, {"type", "bst"}, {"comment", "消息内容"}, {"explain", new List()} }
                    }}}
                }}
            }},
            {"11607", new Map() {
                {"comment", "私聊"},
                {"write", new List() {
                    new Map() { {"name", "roleId"}, {"type", "u64"}, {"comment", "角色ID"}, {"explain", new List()} },
                    new Map() { {"name", "type"}, {"type", "u8"}, {"comment", "类型"}, {"explain", new List()} },
                    new Map() { {"name", "message"}, {"type", "bst"}, {"comment", "消息"}, {"explain", new List()} }
                }},
                {"read", new List() {
                    new Map() { {"name", "result"}, {"type", "rst"}, {"comment", "结果"}, {"explain", new List()} },
                    new Map() { {"name", "senderId"}, {"type", "u64"}, {"comment", "发送者角色ID"}, {"explain", new List()} },
                    new Map() { {"name", "receiverId"}, {"type", "u64"}, {"comment", "接收者角色ID"}, {"explain", new List()} },
                    new Map() { {"name", "type"}, {"type", "u8"}, {"comment", "类型"}, {"explain", new List()} },
                    new Map() { {"name", "message"}, {"type", "bst"}, {"comment", "消息内容"}, {"explain", new List()} }
                }}
            }},
            {"11608", new Map() {
                {"comment", "私聊列表"},
                {"write", new List() {
                    new Map() { {"name", "roleId"}, {"type", "u64"}, {"comment", "角色ID"}, {"explain", new List()} },
                    new Map() { {"name", "page"}, {"type", "u16"}, {"comment", "页"}, {"explain", new List()} }
                }},
                {"read", new List() {
                    new Map() { {"name", "list"}, {"type", "list"}, {"comment", ""}, {"explain", new List() {
                        new Map() { {"name", "senderId"}, {"type", "u64"}, {"comment", "发送者角色ID"}, {"explain", new List()} },
                        new Map() { {"name", "receiverId"}, {"type", "u64"}, {"comment", "接收者角色ID"}, {"explain", new List()} },
                        new Map() { {"name", "type"}, {"type", "u8"}, {"comment", "类型"}, {"explain", new List()} },
                        new Map() { {"name", "message"}, {"type", "bst"}, {"comment", "消息内容"}, {"explain", new List()} }
                    }}}
                }}
            }}
        };
    }
}