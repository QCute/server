using List = System.Collections.Generic.List<System.Object>;
using Map = System.Collections.Generic.Dictionary<System.String, System.Object>;

public static class NoticeProtocol
{
    public static Map GetMeta()
    {
        return new Map()
        {
            {"50001", new Map() {
                {"comment", "公告列表"},
                {"write", new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {

                }}}},
                {"read", new Map() { {"name", "data"}, {"type", "list"}, {"comment", "公告列表"}, {"explain", new List() {
                    new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {
                        new Map() { {"name", "noticeId"}, {"type", "u64"}, {"comment", "公告ID"}, {"explain", new List()} },
                        new Map() { {"name", "receiveTime"}, {"type", "u32"}, {"comment", "收到时间"}, {"explain", new List()} },
                        new Map() { {"name", "readTime"}, {"type", "u32"}, {"comment", "读取时间"}, {"explain", new List()} },
                        new Map() { {"name", "title"}, {"type", "bst"}, {"comment", "标题"}, {"explain", new List()} },
                        new Map() { {"name", "content"}, {"type", "bst"}, {"comment", "内容"}, {"explain", new List()} }
                    }}}
                }}}}
            }}
        };
    }
}