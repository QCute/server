using List = System.Collections.ArrayList;
using Map = System.Collections.Generic.Dictionary<System.String, System.Object>;

public static class NoticeProtocol
{
    public static Map GetMeta()
    {
        return new Map()
        {
            {"50001", new Map() {
                {"comment", "公告列表"},
                {"write", new List() },
                {"read", new List() {
                    new Map() { {"name", "noticeList"}, {"type", "list"}, {"comment", "公告列表"}, {"explain", new List() {
                        new Map() { {"name", "noticeId"}, {"type", "u64"}, {"comment", "公告ID"}, {"explain", new List()} },
                        new Map() { {"name", "receiveTime"}, {"type", "u32"}, {"comment", "收到时间"}, {"explain", new List()} },
                        new Map() { {"name", "readTime"}, {"type", "u32"}, {"comment", "读取时间"}, {"explain", new List()} },
                        new Map() { {"name", "title"}, {"type", "bst"}, {"comment", "标题"}, {"explain", new List()} },
                        new Map() { {"name", "content"}, {"type", "bst"}, {"comment", "内容"}, {"explain", new List()} }
                    }}}
                }}
            }},
            {"50002", new Map() {
                {"comment", "公告"},
                {"write", new List() },
                {"read", new List() {
                    new Map() { {"name", "scope"}, {"type", "u8"}, {"comment", "范围"}, {"explain", new List()} },
                    new Map() { {"name", "type"}, {"type", "u8"}, {"comment", "类型"}, {"explain", new List()} },
                    new Map() { {"name", "title"}, {"type", "bst"}, {"comment", "标题"}, {"explain", new List()} },
                    new Map() { {"name", "msg"}, {"type", "bst"}, {"comment", "消息"}, {"explain", new List()} }
                }}
            }}
        };
    }
}