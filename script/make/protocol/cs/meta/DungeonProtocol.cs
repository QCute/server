using List = System.Collections.Generic.List<System.Object>;
using Map = System.Collections.Generic.Dictionary<System.String, System.Object>;

public static class DungeonProtocol
{
    public static Map GetMeta()
    {
        return new Map()
        {
            {"17001", new Map() {
                {"comment", "副本信息"},
                {"write", new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {

                }}}},
                {"read", new Map() { {"name", "data"}, {"type", "list"}, {"comment", ""}, {"explain", new List() {
                    new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {
                        new Map() { {"name", "dungeonId"}, {"type", "u32"}, {"comment", "副本Id"}, {"explain", new List()} },
                        new Map() { {"name", "todayNumber"}, {"type", "u16"}, {"comment", "今天次数"}, {"explain", new List()} },
                        new Map() { {"name", "totalNumber"}, {"type", "u16"}, {"comment", "总次数"}, {"explain", new List()} }
                    }}}
                }}}}
            }},
            {"17002", new Map() {
                {"comment", "进入副本"},
                {"write", new Map() { {"name", "data"}, {"type", "u32"}, {"comment", "副本Id"}, {"explain", new List()} }},
                {"read", new Map() { {"name", "data"}, {"type", "ast"}, {"comment", "结果"}, {"explain", new List()} }}
            }},
            {"17005", new Map() {
                {"comment", "副本鼓舞"},
                {"write", new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {

                }}}},
                {"read", new Map() { {"name", "data"}, {"type", "ast"}, {"comment", "结果"}, {"explain", new List()} }}
            }}
        };
    }
}