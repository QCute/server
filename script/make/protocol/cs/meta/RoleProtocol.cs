using List = System.Collections.Generic.List<System.Object>;
using Map = System.Collections.Generic.Dictionary<System.String, System.Object>;

public static class RoleProtocol
{
    public static Map GetMeta()
    {
        return new Map()
        {
            {"10101", new Map() {
                {"comment", "角色"},
                {"write", new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {

                }}}},
                {"read", new Map() { {"name", "role"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {
                    new Map() { {"name", "roleId"}, {"type", "u64"}, {"comment", "角色ID"}, {"explain", new List()} },
                    new Map() { {"name", "roleName"}, {"type", "bst"}, {"comment", "角色名"}, {"explain", new List()} },
                    new Map() { {"name", "sex"}, {"type", "u8"}, {"comment", "性别"}, {"explain", new List()} },
                    new Map() { {"name", "classes"}, {"type", "u8"}, {"comment", "职业"}, {"explain", new List()} },
                    new Map() { {"name", "level"}, {"type", "u64"}, {"comment", "等级"}, {"explain", new List()} }
                }}}}
            }},
            {"10102", new Map() {
                {"comment", "资产"},
                {"write", new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {

                }}}},
                {"read", new Map() { {"name", "asset"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {
                    new Map() { {"name", "gold"}, {"type", "u64"}, {"comment", "金币"}, {"explain", new List()} },
                    new Map() { {"name", "silver"}, {"type", "u32"}, {"comment", "银币"}, {"explain", new List()} },
                    new Map() { {"name", "copper"}, {"type", "u64"}, {"comment", "铜币"}, {"explain", new List()} },
                    new Map() { {"name", "exp"}, {"type", "u64"}, {"comment", "经验"}, {"explain", new List()} }
                }}}}
            }},
            {"10103", new Map() {
                {"comment", "vip"},
                {"write", new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {

                }}}},
                {"read", new Map() { {"name", "vip"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {
                    new Map() { {"name", "vipLevel"}, {"type", "u8"}, {"comment", "等级"}, {"explain", new List()} },
                    new Map() { {"name", "exp"}, {"type", "u64"}, {"comment", "经验"}, {"explain", new List()} },
                    new Map() { {"name", "expireTime"}, {"type", "u32"}, {"comment", "过期时间"}, {"explain", new List()} }
                }}}}
            }}
        };
    }
}