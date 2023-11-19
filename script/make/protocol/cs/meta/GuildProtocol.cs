using List = System.Collections.Generic.List<System.Object>;
using Map = System.Collections.Generic.Dictionary<System.String, System.Object>;

public static class GuildProtocol
{
    public static Map GetMeta()
    {
        return new Map()
        {
            {"30101", new Map() {
                {"comment", "公会列表"},
                {"write", new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {

                }}}},
                {"read", new Map() { {"name", "data"}, {"type", "list"}, {"comment", ""}, {"key", ""}, {"explain", new List() {
                    new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {
                        new Map() { {"name", "guildId"}, {"type", "u64"}, {"comment", "公会ID"}, {"explain", new List()} },
                        new Map() { {"name", "guildName"}, {"type", "bst"}, {"comment", "公会名字"}, {"explain", new List()} },
                        new Map() { {"name", "createTime"}, {"type", "u32"}, {"comment", "创建时间"}, {"explain", new List()} },
                        new Map() { {"name", "leaderRoleId"}, {"type", "u64"}, {"comment", "会长角色ID"}, {"explain", new List()} },
                        new Map() { {"name", "leaderName"}, {"type", "bst"}, {"comment", "会长名字"}, {"explain", new List()} }
                    }}}
                }}}}
            }},
            {"30102", new Map() {
                {"comment", "成员列表"},
                {"write", new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {

                }}}},
                {"read", new Map() { {"name", "data"}, {"type", "list"}, {"comment", ""}, {"key", ""}, {"explain", new List() {
                    new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {
                        new Map() { {"name", "roleId"}, {"type", "u64"}, {"comment", "成员ID"}, {"explain", new List()} },
                        new Map() { {"name", "job"}, {"type", "u8"}, {"comment", "职位"}, {"explain", new List()} },
                        new Map() { {"name", "joinTime"}, {"type", "u32"}, {"comment", "加入时间"}, {"explain", new List()} },
                        new Map() { {"name", "roleName"}, {"type", "bst"}, {"comment", "成员名字"}, {"explain", new List()} },
                        new Map() { {"name", "sex"}, {"type", "u8"}, {"comment", "性别"}, {"explain", new List()} },
                        new Map() { {"name", "classes"}, {"type", "u8"}, {"comment", "职业"}, {"explain", new List()} },
                        new Map() { {"name", "vipLevel"}, {"type", "u8"}, {"comment", "Vip等级"}, {"explain", new List()} }
                    }}}
                }}}}
            }},
            {"30103", new Map() {
                {"comment", "申请列表"},
                {"write", new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {

                }}}},
                {"read", new Map() { {"name", "data"}, {"type", "list"}, {"comment", ""}, {"key", ""}, {"explain", new List() {
                    new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {
                        new Map() { {"name", "roleId"}, {"type", "u64"}, {"comment", "申请ID"}, {"explain", new List()} },
                        new Map() { {"name", "applyTime"}, {"type", "u32"}, {"comment", "申请时间"}, {"explain", new List()} },
                        new Map() { {"name", "roleName"}, {"type", "bst"}, {"comment", "申请名字"}, {"explain", new List()} },
                        new Map() { {"name", "sex"}, {"type", "u8"}, {"comment", "性别"}, {"explain", new List()} },
                        new Map() { {"name", "classes"}, {"type", "u8"}, {"comment", "职业"}, {"explain", new List()} },
                        new Map() { {"name", "vipLevel"}, {"type", "u8"}, {"comment", "Vip等级"}, {"explain", new List()} }
                    }}}
                }}}}
            }},
            {"30104", new Map() {
                {"comment", "自身公会信息"},
                {"write", new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {

                }}}},
                {"read", new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {
                    new Map() { {"name", "guildId"}, {"type", "u64"}, {"comment", "公会ID"}, {"explain", new List()} },
                    new Map() { {"name", "guildName"}, {"type", "bst"}, {"comment", "公会名字"}, {"explain", new List()} },
                    new Map() { {"name", "exp"}, {"type", "u32"}, {"comment", "经验"}, {"explain", new List()} },
                    new Map() { {"name", "wealth"}, {"type", "u32"}, {"comment", "财富"}, {"explain", new List()} },
                    new Map() { {"name", "level"}, {"type", "u8"}, {"comment", "等级"}, {"explain", new List()} },
                    new Map() { {"name", "createTime"}, {"type", "u32"}, {"comment", "创建时间"}, {"explain", new List()} },
                    new Map() { {"name", "notice"}, {"type", "bst"}, {"comment", "公告"}, {"explain", new List()} },
                    new Map() { {"name", "leaderRoleId"}, {"type", "u64"}, {"comment", "会长角色ID"}, {"explain", new List()} },
                    new Map() { {"name", "leaderName"}, {"type", "bst"}, {"comment", "会长名字"}, {"explain", new List()} }
                }}}}
            }},
            {"30105", new Map() {
                {"comment", "自身成员信息"},
                {"write", new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {

                }}}},
                {"read", new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {
                    new Map() { {"name", "roleId"}, {"type", "u64"}, {"comment", "成员ID"}, {"explain", new List()} },
                    new Map() { {"name", "job"}, {"type", "u8"}, {"comment", "职位"}, {"explain", new List()} },
                    new Map() { {"name", "joinTime"}, {"type", "u32"}, {"comment", "加入时间"}, {"explain", new List()} },
                    new Map() { {"name", "roleName"}, {"type", "bst"}, {"comment", "成员名字"}, {"explain", new List()} },
                    new Map() { {"name", "sex"}, {"type", "u8"}, {"comment", "性别"}, {"explain", new List()} },
                    new Map() { {"name", "classes"}, {"type", "u8"}, {"comment", "职业"}, {"explain", new List()} },
                    new Map() { {"name", "vipLevel"}, {"type", "u8"}, {"comment", "Vip等级"}, {"explain", new List()} }
                }}}}
            }},
            {"30106", new Map() {
                {"comment", "自身申请信息列表"},
                {"write", new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {

                }}}},
                {"read", new Map() { {"name", "data"}, {"type", "list"}, {"comment", ""}, {"explain", new List() {
                    new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {
                        new Map() { {"name", "guildId"}, {"type", "u64"}, {"comment", "公会ID"}, {"explain", new List()} },
                        new Map() { {"name", "applyTime"}, {"type", "u32"}, {"comment", "申请时间"}, {"explain", new List()} },
                        new Map() { {"name", "guildName"}, {"type", "bst"}, {"comment", "公会名字"}, {"explain", new List()} }
                    }}}
                }}}}
            }},
            {"30107", new Map() {
                {"comment", "创建公会"},
                {"write", new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {
                    new Map() { {"name", "type"}, {"type", "u8"}, {"comment", "类型"}, {"explain", new List()} },
                    new Map() { {"name", "guildName"}, {"type", "bst"}, {"comment", "公会名"}, {"explain", new List()} }
                }}}},
                {"read", new Map() { {"name", "data"}, {"type", "ast"}, {"comment", "结果"}, {"explain", new List()} }}
            }},
            {"30108", new Map() {
                {"comment", "申请"},
                {"write", new Map() { {"name", "data"}, {"type", "u64"}, {"comment", "公会ID"}, {"explain", new List()} }},
                {"read", new Map() { {"name", "data"}, {"type", "ast"}, {"comment", "结果"}, {"explain", new List()} }}
            }},
            {"30109", new Map() {
                {"comment", "取消申请"},
                {"write", new Map() { {"name", "data"}, {"type", "u64"}, {"comment", "公会ID"}, {"explain", new List()} }},
                {"read", new Map() { {"name", "data"}, {"type", "ast"}, {"comment", "结果"}, {"explain", new List()} }}
            }},
            {"30110", new Map() {
                {"comment", "取消全部申请"},
                {"write", new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {

                }}}},
                {"read", new Map() { {"name", "data"}, {"type", "ast"}, {"comment", "结果"}, {"explain", new List()} }}
            }},
            {"30111", new Map() {
                {"comment", "允许申请"},
                {"write", new Map() { {"name", "data"}, {"type", "u64"}, {"comment", "角色ID"}, {"explain", new List()} }},
                {"read", new Map() { {"name", "data"}, {"type", "ast"}, {"comment", "结果"}, {"explain", new List()} }}
            }},
            {"30112", new Map() {
                {"comment", "允许全部申请"},
                {"write", new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {

                }}}},
                {"read", new Map() { {"name", "data"}, {"type", "ast"}, {"comment", "结果"}, {"explain", new List()} }}
            }},
            {"30113", new Map() {
                {"comment", "拒绝申请"},
                {"write", new Map() { {"name", "data"}, {"type", "u64"}, {"comment", "角色ID"}, {"explain", new List()} }},
                {"read", new Map() { {"name", "data"}, {"type", "ast"}, {"comment", "结果"}, {"explain", new List()} }}
            }},
            {"30114", new Map() {
                {"comment", "拒绝全部申请"},
                {"write", new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {

                }}}},
                {"read", new Map() { {"name", "data"}, {"type", "ast"}, {"comment", "结果"}, {"explain", new List()} }}
            }},
            {"30115", new Map() {
                {"comment", "退出"},
                {"write", new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {

                }}}},
                {"read", new Map() { {"name", "data"}, {"type", "ast"}, {"comment", "结果"}, {"explain", new List()} }}
            }},
            {"30116", new Map() {
                {"comment", "解散"},
                {"write", new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {

                }}}},
                {"read", new Map() { {"name", "data"}, {"type", "ast"}, {"comment", "结果"}, {"explain", new List()} }}
            }},
            {"30117", new Map() {
                {"comment", "踢出"},
                {"write", new Map() { {"name", "data"}, {"type", "u64"}, {"comment", "角色ID"}, {"explain", new List()} }},
                {"read", new Map() { {"name", "data"}, {"type", "ast"}, {"comment", "结果"}, {"explain", new List()} }}
            }},
            {"30118", new Map() {
                {"comment", "调整位置"},
                {"write", new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {
                    new Map() { {"name", "roleId"}, {"type", "u64"}, {"comment", "角色ID"}, {"explain", new List()} },
                    new Map() { {"name", "job"}, {"type", "u8"}, {"comment", "位置"}, {"explain", new List()} }
                }}}},
                {"read", new Map() { {"name", "data"}, {"type", "ast"}, {"comment", "结果"}, {"explain", new List()} }}
            }},
            {"30119", new Map() {
                {"comment", "升级"},
                {"write", new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {

                }}}},
                {"read", new Map() { {"name", "data"}, {"type", "ast"}, {"comment", "结果"}, {"explain", new List()} }}
            }},
            {"30120", new Map() {
                {"comment", "更改公告"},
                {"write", new Map() { {"name", "data"}, {"type", "bst"}, {"comment", "公告"}, {"explain", new List()} }},
                {"read", new Map() { {"name", "data"}, {"type", "ast"}, {"comment", "结果"}, {"explain", new List()} }}
            }}
        };
    }
}