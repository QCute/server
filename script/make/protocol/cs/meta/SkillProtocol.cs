using List = System.Collections.Generic.List<System.Object>;
using Map = System.Collections.Generic.Dictionary<System.String, System.Object>;

public static class SkillProtocol
{
    public static Map GetMeta()
    {
        return new Map()
        {
            {"11701", new Map() {
                {"comment", "技能列表"},
                {"write", new List() {
                    new Map() { {"name", "data"}, {"type", "tuple"}, {"comment": ""}, {"explain": new List() {

                    }}}
                }},
                {"read", new List() {
                    new Map() { {"name", "data"}, {"type", "list"}, {"comment", "技能列表"}, {"explain", 
                        new Map() { {"name", "skill"}, {"type", "record"}, {"comment": ""}, {"explain": new List() {
                            new Map() { {"name", "skillId"}, {"type", "u32"}, {"comment", "技能ID"}, {"explain", new List()} },
                            new Map() { {"name", "level"}, {"type", "u16"}, {"comment", "技能等级"}, {"explain", new List()} }
                        }}}
                    }}
                }}
            }},
            {"11702", new Map() {
                {"comment", "学习技能"},
                {"write", new List() {
                    new Map() { {"name", "data"}, {"type", "u32"}, {"comment", "技能ID"}, {"explain", new List()} }
                }},
                {"read", new List() {
                    new Map() { {"name", "data"}, {"type", "rst"}, {"comment", "结果"}, {"explain", new List()} }
                }}
            }}
        };
    }
}