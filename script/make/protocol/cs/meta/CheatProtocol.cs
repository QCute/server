using List = System.Collections.Generic.List<System.Object>;
using Map = System.Collections.Generic.Dictionary<System.String, System.Object>;

public static class CheatProtocol
{
    public static Map GetMeta()
    {
        return new Map()
        {
            {"60002", new Map() {
                {"comment", "秘籍"},
                {"write", new List() {
                    new Map() { {"name", "command"}, {"type", "bst"}, {"comment", "命令"}, {"explain", new List()} }
                }},
                {"read", new List() {
                    new Map() { {"name", "result"}, {"type", "rst"}, {"comment", "结果"}, {"explain", new List()} }
                }}
            }}
        };
    }
}