using List = System.Collections.Generic.List<System.Object>;
using Map = System.Collections.Generic.Dictionary<System.String, System.Object>;

public static class CheatProtocol
{
    public static Map GetMeta()
    {
        return new Map()
        {
            {"60001", new Map() {
                {"comment", "秘籍"},
                {"write", new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {

                }}}},
                {"read", new Map() { {"name", "data"}, {"type", "list"}, {"comment", "命令列表"}, {"explain", new List() {
                    new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {
                        new Map() { {"name", "description"}, {"type", "bst"}, {"comment", "描述"}, {"explain", new List()} },
                        new Map() { {"name", "command"}, {"type", "bst"}, {"comment", "命令"}, {"explain", new List()} }
                    }}}
                }}}}
            }},
            {"60002", new Map() {
                {"comment", "秘籍"},
                {"write", new Map() { {"name", "data"}, {"type", "bst"}, {"comment", "命令"}, {"explain", new List()} }},
                {"read", new Map() { {"name", "data"}, {"type", "ast"}, {"comment", "结果"}, {"explain", new List()} }}
            }}
        };
    }
}