using List = System.Collections.Generic.List<System.Object>;
using Map = System.Collections.Generic.Dictionary<System.String, System.Object>;

public static class FriendProtocol
{
    public static Map GetMeta()
    {
        return new Map()
        {
            {"11502", new Map() {
                {"comment", "申请"},
                {"write", new List() {
                    new Map() { {"name", "friendRoleId"}, {"type", "u64"}, {"comment", "好友角色ID"}, {"explain", new List()} }
                }},
                {"read", new List() {
                    new Map() { {"name", "result"}, {"type", "rst"}, {"comment", "结果"}, {"explain", new List()} }
                }}
            }},
            {"11503", new Map() {
                {"comment", "同意"},
                {"write", new List() {
                    new Map() { {"name", "friendRoleId"}, {"type", "u64"}, {"comment", "好友角色ID"}, {"explain", new List()} }
                }},
                {"read", new List() {
                    new Map() { {"name", "result"}, {"type", "rst"}, {"comment", "结果"}, {"explain", new List()} }
                }}
            }},
            {"11504", new Map() {
                {"comment", "删除"},
                {"write", new List() {
                    new Map() { {"name", "friendRoleId"}, {"type", "u64"}, {"comment", "好友角色ID"}, {"explain", new List()} }
                }},
                {"read", new List() {
                    new Map() { {"name", "result"}, {"type", "rst"}, {"comment", "结果"}, {"explain", new List()} },
                    new Map() { {"name", "friendRoleId"}, {"type", "u64"}, {"comment", "好友角色ID"}, {"explain", new List()} }
                }}
            }},
            {"11505", new Map() {
                {"comment", "拉黑"},
                {"write", new List() {
                    new Map() { {"name", "friendRoleId"}, {"type", "u64"}, {"comment", "好友角色ID"}, {"explain", new List()} }
                }},
                {"read", new List() {
                    new Map() { {"name", "result"}, {"type", "rst"}, {"comment", "结果"}, {"explain", new List()} },
                    new Map() { {"name", "friendRoleId"}, {"type", "u64"}, {"comment", "好友角色ID"}, {"explain", new List()} }
                }}
            }},
            {"11506", new Map() {
                {"comment", "取消拉黑"},
                {"write", new List() {
                    new Map() { {"name", "friendRoleId"}, {"type", "u64"}, {"comment", "好友角色ID"}, {"explain", new List()} }
                }},
                {"read", new List() {
                    new Map() { {"name", "result"}, {"type", "rst"}, {"comment", "结果"}, {"explain", new List()} },
                    new Map() { {"name", "friendRoleId"}, {"type", "u64"}, {"comment", "好友角色ID"}, {"explain", new List()} }
                }}
            }}
        };
    }
}