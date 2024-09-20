using List = System.Collections.Generic.List<System.Object>;
using Map = System.Collections.Generic.Dictionary<System.String, System.Object>;

public static class GuildProtocol
{
    public static Map GetMeta()
    {
        return new Map()
        {
            {"30107", new Map() {
                {"comment", "创建公会"},
                {"write", new List() {
                    new Map() { {"name", "type"}, {"type", "u8"}, {"comment", "类型"}, {"explain", new List()} },
                    new Map() { {"name", "guildName"}, {"type", "bst"}, {"comment", "公会名"}, {"explain", new List()} }
                }},
                {"read", new List() {
                    new Map() { {"name", "result"}, {"type", "rst"}, {"comment", "结果"}, {"explain", new List()} }
                }}
            }},
            {"30108", new Map() {
                {"comment", "申请"},
                {"write", new List() {
                    new Map() { {"name", "guildId"}, {"type", "u64"}, {"comment", "公会ID"}, {"explain", new List()} }
                }},
                {"read", new List() {
                    new Map() { {"name", "result"}, {"type", "rst"}, {"comment", "结果"}, {"explain", new List()} }
                }}
            }},
            {"30109", new Map() {
                {"comment", "取消申请"},
                {"write", new List() {
                    new Map() { {"name", "guildId"}, {"type", "u64"}, {"comment", "公会ID"}, {"explain", new List()} }
                }},
                {"read", new List() {
                    new Map() { {"name", "result"}, {"type", "rst"}, {"comment", "结果"}, {"explain", new List()} }
                }}
            }},
            {"30111", new Map() {
                {"comment", "允许申请"},
                {"write", new List() {
                    new Map() { {"name", "roleId"}, {"type", "u64"}, {"comment", "角色ID"}, {"explain", new List()} }
                }},
                {"read", new List() {
                    new Map() { {"name", "result"}, {"type", "rst"}, {"comment", "结果"}, {"explain", new List()} }
                }}
            }},
            {"30113", new Map() {
                {"comment", "拒绝申请"},
                {"write", new List() {
                    new Map() { {"name", "roleId"}, {"type", "u64"}, {"comment", "角色ID"}, {"explain", new List()} }
                }},
                {"read", new List() {
                    new Map() { {"name", "result"}, {"type", "rst"}, {"comment", "结果"}, {"explain", new List()} }
                }}
            }},
            {"30117", new Map() {
                {"comment", "踢出"},
                {"write", new List() {
                    new Map() { {"name", "roleId"}, {"type", "u64"}, {"comment", "角色ID"}, {"explain", new List()} }
                }},
                {"read", new List() {
                    new Map() { {"name", "result"}, {"type", "rst"}, {"comment", "结果"}, {"explain", new List()} }
                }}
            }},
            {"30118", new Map() {
                {"comment", "调整位置"},
                {"write", new List() {
                    new Map() { {"name", "roleId"}, {"type", "u64"}, {"comment", "角色ID"}, {"explain", new List()} },
                    new Map() { {"name", "job"}, {"type", "u8"}, {"comment", "位置"}, {"explain", new List()} }
                }},
                {"read", new List() {
                    new Map() { {"name", "result"}, {"type", "rst"}, {"comment", "结果"}, {"explain", new List()} }
                }}
            }},
            {"30120", new Map() {
                {"comment", "更改公告"},
                {"write", new List() {
                    new Map() { {"name", "notice"}, {"type", "bst"}, {"comment", "公告"}, {"explain", new List()} }
                }},
                {"read", new List() {
                    new Map() { {"name", "result"}, {"type", "rst"}, {"comment", "结果"}, {"explain", new List()} }
                }}
            }}
        };
    }
}