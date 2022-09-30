using List = System.Collections.ArrayList;
using Map = System.Collections.Generic.Dictionary<System.String, System.Object>;

public static class WelfareProtocol
{
    public static Map GetMeta()
    {
        return new Map()
        {
            {"15001", new Map() {
                {"comment", "签到"},
                {"write", new List() },
                {"read", new List() {
                    new Map() { {"name", "result"}, {"type", "rst"}, {"comment", "结果"}, {"explain", new List()} }
                }}
            }},
            {"15002", new Map() {
                {"comment", "兑换码兑换"},
                {"write", new List() {
                    new Map() { {"name", "key"}, {"type", "bst"}, {"comment", "兑换码"}, {"explain", new List()} }
                }},
                {"read", new List() {
                    new Map() { {"name", "result"}, {"type", "rst"}, {"comment", "结果"}, {"explain", new List()} }
                }}
            }},
            {"15003", new Map() {
                {"comment", "红包"},
                {"write", new List() {
                    new Map() { {"name", "luckyMoneyNo"}, {"type", "u64"}, {"comment", "红包编号"}, {"explain", new List()} }
                }},
                {"read", new List() {
                    new Map() { {"name", "luckyMoneyNo"}, {"type", "u64"}, {"comment", "红包编号"}, {"explain", new List()} },
                    new Map() { {"name", "totalGold"}, {"type", "u64"}, {"comment", "总金币"}, {"explain", new List()} },
                    new Map() { {"name", "totalNumber"}, {"type", "u32"}, {"comment", "总数量"}, {"explain", new List()} },
                    new Map() { {"name", "receiveNumber"}, {"type", "u16"}, {"comment", "已经领取人数"}, {"explain", new List()} },
                    new Map() { {"name", "receiveList"}, {"type", "list"}, {"comment", "领取列表"}, {"explain", new List() {
                        new Map() { {"name", "serverId"}, {"type", "u16"}, {"comment", "服务器Id"}, {"explain", new List()} },
                        new Map() { {"name", "roleId"}, {"type", "u64"}, {"comment", "角色Id"}, {"explain", new List()} },
                        new Map() { {"name", "roleName"}, {"type", "bst"}, {"comment", "角色名"}, {"explain", new List()} },
                        new Map() { {"name", "gold"}, {"type", "u64"}, {"comment", "金币"}, {"explain", new List()} },
                        new Map() { {"name", "receiveTime"}, {"type", "u32"}, {"comment", "领取时间"}, {"explain", new List()} }
                    }}},
                    new Map() { {"name", "sendTime"}, {"type", "u32"}, {"comment", "发送时间"}, {"explain", new List()} }
                }}
            }},
            {"15004", new Map() {
                {"comment", "领取红包"},
                {"write", new List() {
                    new Map() { {"name", "luckyMoneyNo"}, {"type", "u64"}, {"comment", "红包编号"}, {"explain", new List()} }
                }},
                {"read", new List() {
                    new Map() { {"name", "result"}, {"type", "rst"}, {"comment", "结果"}, {"explain", new List()} },
                    new Map() { {"name", "gold"}, {"type", "u64"}, {"comment", "金币"}, {"explain", new List()} }
                }}
            }},
            {"15005", new Map() {
                {"comment", "新到红包"},
                {"write", new List() },
                {"read", new List() }
            }}
        };
    }
}