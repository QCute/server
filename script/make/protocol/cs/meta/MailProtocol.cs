using List = System.Collections.Generic.List<System.Object>;
using Map = System.Collections.Generic.Dictionary<System.String, System.Object>;

public static class MailProtocol
{
    public static Map GetMeta()
    {
        return new Map()
        {
            {"11401", new Map() {
                {"comment", "邮件列表"},
                {"write", new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {

                }}}},
                {"read", new Map() { {"name", "data"}, {"type", "list"}, {"comment", ""}, {"explain", new List() {
                    new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {
                        new Map() { {"name", "mailId"}, {"type", "u64"}, {"comment", "邮件ID"}, {"explain", new List()} },
                        new Map() { {"name", "receiveTime"}, {"type", "u32"}, {"comment", "接收时间"}, {"explain", new List()} },
                        new Map() { {"name", "expireTime"}, {"type", "u32"}, {"comment", "有效时间"}, {"explain", new List()} },
                        new Map() { {"name", "readTime"}, {"type", "u32"}, {"comment", "读取时间"}, {"explain", new List()} },
                        new Map() { {"name", "receiveAttachmentTime"}, {"type", "u32"}, {"comment", "领取附件时间"}, {"explain", new List()} },
                        new Map() { {"name", "title"}, {"type", "bst"}, {"comment", "标题"}, {"explain", new List()} },
                        new Map() { {"name", "content"}, {"type", "bst"}, {"comment", "内容"}, {"explain", new List()} },
                        new Map() { {"name", "attachment"}, {"type", "list"}, {"comment", "附件列表"}, {"explain", new List() {
                            new Map() { {"name", "data"}, {"type", "map"}, {"comment", ""}, {"explain", new List() {
                                new Map() { {"name", "itemId"}, {"type", "u32"}, {"comment", "物品ID"}, {"explain", new List()} },
                                new Map() { {"name", "number"}, {"type", "u16"}, {"comment", "数量"}, {"explain", new List()} }
                            }}}
                        }}}
                    }}}
                }}}}
            }},
            {"11402", new Map() {
                {"comment", "阅读"},
                {"write", new Map() { {"name", "data"}, {"type", "u64"}, {"comment", "邮件ID"}, {"explain", new List()} }},
                {"read", new Map() { {"name", "data"}, {"type", "ast"}, {"comment", "结果"}, {"explain", new List()} }}
            }},
            {"11403", new Map() {
                {"comment", "领取附件"},
                {"write", new Map() { {"name", "data"}, {"type", "u64"}, {"comment", "邮件ID"}, {"explain", new List()} }},
                {"read", new Map() { {"name", "data"}, {"type", "ast"}, {"comment", "结果"}, {"explain", new List()} }}
            }},
            {"11404", new Map() {
                {"comment", "删除邮件"},
                {"write", new Map() { {"name", "data"}, {"type", "u64"}, {"comment", "邮件ID"}, {"explain", new List()} }},
                {"read", new Map() { {"name", "data"}, {"type", "ast"}, {"comment", "结果"}, {"explain", new List()} }}
            }}
        };
    }
}