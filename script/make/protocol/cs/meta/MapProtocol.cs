using List = System.Collections.ArrayList;
using Map = System.Collections.Generic.Dictionary<System.String, System.Object>;

public static class MapProtocol
{
    public static Map GetMeta()
    {
        return new Map()
        {
            {"20001", new Map() {
                {"comment", "地图信息"},
                {"write", new List() },
                {"read", new List() }
            }},
            {"20011", new Map() {
                {"comment", "战斗对象列表"},
                {"write", new List() },
                {"read", new List() {
                    new Map() { {"name", "list"}, {"type", "list"}, {"comment", "对象列表"}, {"explain", new List() {
                        new Map() { {"name", "id"}, {"type", "u64"}, {"comment", "ID"}, {"explain", new List()} },
                        new Map() { {"name", "type"}, {"type", "u8"}, {"comment", "类型"}, {"explain", new List()} },
                        new Map() { {"name", "fc"}, {"type", "u64"}, {"comment", "战力"}, {"explain", new List()} },
                        new Map() { {"name", "hp"}, {"type", "u64"}, {"comment", "血量"}, {"explain", new List()} },
                        new Map() { {"name", "health"}, {"type", "u64"}, {"comment", "健康"}, {"explain", new List()} },
                        new Map() { {"name", "skill"}, {"type", "list"}, {"comment", "技能列表"}, {"explain", new List() {
                            new Map() { {"name", "skillId"}, {"type", "u32"}, {"comment", "技能ID"}, {"explain", new List()} },
                            new Map() { {"name", "time"}, {"type", "u32"}, {"comment", "时间"}, {"explain", new List()} },
                            new Map() { {"name", "number"}, {"type", "u32"}, {"comment", "数量"}, {"explain", new List()} }
                        }}},
                        new Map() { {"name", "buff"}, {"type", "list"}, {"comment", "Buff列表"}, {"explain", new List() {
                            new Map() { {"name", "buffId"}, {"type", "u32"}, {"comment", "BuffID"}, {"explain", new List()} },
                            new Map() { {"name", "expireTime"}, {"type", "u32"}, {"comment", "过期时间"}, {"explain", new List()} },
                            new Map() { {"name", "overlap"}, {"type", "u32"}, {"comment", "数量"}, {"explain", new List()} }
                        }}},
                        new Map() { {"name", "x"}, {"type", "u16"}, {"comment", "X坐标"}, {"explain", new List()} },
                        new Map() { {"name", "y"}, {"type", "u16"}, {"comment", "Y坐标"}, {"explain", new List()} }
                    }}}
                }}
            }},
            {"20012", new Map() {
                {"comment", "战斗对象移动"},
                {"write", new List() {
                    new Map() { {"name", "x"}, {"type", "u16"}, {"comment", "x坐标"}, {"explain", new List()} },
                    new Map() { {"name", "y"}, {"type", "u16"}, {"comment", "y坐标"}, {"explain", new List()} }
                }},
                {"read", new List() {
                    new Map() { {"name", "id"}, {"type", "u64"}, {"comment", "ID"}, {"explain", new List()} },
                    new Map() { {"name", "x"}, {"type", "u16"}, {"comment", "X坐标"}, {"explain", new List()} },
                    new Map() { {"name", "y"}, {"type", "u16"}, {"comment", "Y坐标"}, {"explain", new List()} }
                }}
            }},
            {"20013", new Map() {
                {"comment", "战斗对象离开"},
                {"write", new List() },
                {"read", new List() {
                    new Map() { {"name", "id"}, {"type", "u64"}, {"comment", "ID"}, {"explain", new List()} }
                }}
            }},
            {"20014", new Map() {
                {"comment", "发起战斗"},
                {"write", new List() {
                    new Map() { {"name", "skillId"}, {"type", "u32"}, {"comment", "技能Id"}, {"explain", new List()} },
                    new Map() { {"name", "targetList"}, {"type", "list"}, {"comment", "对象列表"}, {"explain", new List() {
                        new Map() { {"name", "targetId"}, {"type", "u64"}, {"comment", "ID"}, {"explain", new List()} }
                    }}}
                }},
                {"read", new List() {
                    new Map() { {"name", "fighterId"}, {"type", "u64"}, {"comment", "战斗对象Id"}, {"explain", new List()} },
                    new Map() { {"name", "performSkillId"}, {"type", "u32"}, {"comment", "技能Id"}, {"explain", new List()} },
                    new Map() { {"name", "list"}, {"type", "list"}, {"comment", "对象列表"}, {"explain", new List() {
                        new Map() { {"name", "id"}, {"type", "u64"}, {"comment", "ID"}, {"explain", new List()} },
                        new Map() { {"name", "type"}, {"type", "u8"}, {"comment", "类型"}, {"explain", new List()} },
                        new Map() { {"name", "fc"}, {"type", "u64"}, {"comment", "战力"}, {"explain", new List()} },
                        new Map() { {"name", "hp"}, {"type", "u64"}, {"comment", "血量"}, {"explain", new List()} },
                        new Map() { {"name", "health"}, {"type", "u64"}, {"comment", "健康"}, {"explain", new List()} },
                        new Map() { {"name", "skill"}, {"type", "list"}, {"comment", "技能列表"}, {"explain", new List() {
                            new Map() { {"name", "skillId"}, {"type", "u32"}, {"comment", "技能ID"}, {"explain", new List()} },
                            new Map() { {"name", "time"}, {"type", "u32"}, {"comment", "时间"}, {"explain", new List()} },
                            new Map() { {"name", "number"}, {"type", "u32"}, {"comment", "数量"}, {"explain", new List()} }
                        }}},
                        new Map() { {"name", "buff"}, {"type", "list"}, {"comment", "Buff列表"}, {"explain", new List() {
                            new Map() { {"name", "buffId"}, {"type", "u32"}, {"comment", "BuffID"}, {"explain", new List()} },
                            new Map() { {"name", "expireTime"}, {"type", "u32"}, {"comment", "过期时间"}, {"explain", new List()} },
                            new Map() { {"name", "overlap"}, {"type", "u32"}, {"comment", "数量"}, {"explain", new List()} }
                        }}},
                        new Map() { {"name", "x"}, {"type", "u16"}, {"comment", "X坐标"}, {"explain", new List()} },
                        new Map() { {"name", "y"}, {"type", "u16"}, {"comment", "Y坐标"}, {"explain", new List()} }
                    }}}
                }}
            }}
        };
    }
}