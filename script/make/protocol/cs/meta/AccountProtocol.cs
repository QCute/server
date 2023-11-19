using List = System.Collections.Generic.List<System.Object>;
using Map = System.Collections.Generic.Dictionary<System.String, System.Object>;

public static class AccountProtocol
{
    public static Map GetMeta()
    {
        return new Map()
        {
            {"10000", new Map() {
                {"comment", "心跳包"},
                {"write", new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {

                }}}},
                {"read", new Map() { {"name", "data"}, {"type", "ast"}, {"comment", "结果"}, {"explain", new List()} }}
            }},
            {"10001", new Map() {
                {"comment", "查询账户"},
                {"write", new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {
                    new Map() { {"name", "serverId"}, {"type", "u16"}, {"comment", "服务器ID"}, {"explain", new List()} },
                    new Map() { {"name", "accountName"}, {"type", "bst"}, {"comment", "账户名"}, {"explain", new List()} }
                }}}},
                {"read", new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {
                    new Map() { {"name", "result"}, {"type", "ast"}, {"comment", "结果"}, {"explain", new List()} },
                    new Map() { {"name", "list"}, {"type", "list"}, {"comment", "角色名列表"}, {"explain", new List() {
                        new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {
                            new Map() { {"name", "roleId"}, {"type", "u64"}, {"comment", "角色ID"}, {"explain", new List()} },
                            new Map() { {"name", "roleName"}, {"type", "bst"}, {"comment", "角色名"}, {"explain", new List()} }
                        }}}
                    }}}
                }}}}
            }},
            {"10002", new Map() {
                {"comment", "创建账户"},
                {"write", new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {
                    new Map() { {"name", "roleName"}, {"type", "bst"}, {"comment", "角色名"}, {"explain", new List()} },
                    new Map() { {"name", "serverId"}, {"type", "u16"}, {"comment", "服务器ID"}, {"explain", new List()} },
                    new Map() { {"name", "accountName"}, {"type", "bst"}, {"comment", "账户名"}, {"explain", new List()} },
                    new Map() { {"name", "sex"}, {"type", "u8"}, {"comment", "性别"}, {"explain", new List()} },
                    new Map() { {"name", "classes"}, {"type", "u8"}, {"comment", "职业"}, {"explain", new List()} },
                    new Map() { {"name", "channel"}, {"type", "bst"}, {"comment", "渠道"}, {"explain", new List()} },
                    new Map() { {"name", "deviceId"}, {"type", "bst"}, {"comment", "设备"}, {"explain", new List()} },
                    new Map() { {"name", "mac"}, {"type", "bst"}, {"comment", "mac地址"}, {"explain", new List()} },
                    new Map() { {"name", "deviceType"}, {"type", "bst"}, {"comment", "设备类型"}, {"explain", new List()} }
                }}}},
                {"read", new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {
                    new Map() { {"name", "result"}, {"type", "ast"}, {"comment", "结果"}, {"explain", new List()} },
                    new Map() { {"name", "roleId"}, {"type", "u64"}, {"comment", "角色ID"}, {"explain", new List()} },
                    new Map() { {"name", "roleName"}, {"type", "bst"}, {"comment", "角色名"}, {"explain", new List()} }
                }}}}
            }},
            {"10003", new Map() {
                {"comment", "登录"},
                {"write", new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {
                    new Map() { {"name", "roleId"}, {"type", "u64"}, {"comment", "角色ID"}, {"explain", new List()} },
                    new Map() { {"name", "roleName"}, {"type", "bst"}, {"comment", "角色名"}, {"explain", new List()} },
                    new Map() { {"name", "serverId"}, {"type", "u16"}, {"comment", "服务器ID"}, {"explain", new List()} },
                    new Map() { {"name", "accountName"}, {"type", "bst"}, {"comment", "账户名"}, {"explain", new List()} }
                }}}},
                {"read", new Map() { {"name", "data"}, {"type", "ast"}, {"comment", "结果"}, {"explain", new List()} }}
            }},
            {"10004", new Map() {
                {"comment", "退出"},
                {"write", new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {

                }}}},
                {"read", new Map() { {"name", "data"}, {"type", "ast"}, {"comment", "结果"}, {"explain", new List()} }}
            }}
        };
    }
}