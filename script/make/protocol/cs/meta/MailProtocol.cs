using List = System.Collections.Generic.List<System.Object>;
using Map = System.Collections.Generic.Dictionary<System.String, System.Object>;

public static class MailProtocol
{
    public static Map GetMeta()
    {
        return new Map()
        {
            {"11402", new Map() {
                {"comment", "阅读"},
                {"write", new List() {
                    new Map() { {"name", "mailId"}, {"type", "u64"}, {"comment", "邮件ID"}, {"explain", new List()} }
                }},
                {"read", new List() {
                    new Map() { {"name", "result"}, {"type", "rst"}, {"comment", "结果"}, {"explain", new List()} }
                }}
            }},
            {"11403", new Map() {
                {"comment", "领取附件"},
                {"write", new List() {
                    new Map() { {"name", "mailId"}, {"type", "u64"}, {"comment", "邮件ID"}, {"explain", new List()} }
                }},
                {"read", new List() {
                    new Map() { {"name", "result"}, {"type", "rst"}, {"comment", "结果"}, {"explain", new List()} }
                }}
            }},
            {"11404", new Map() {
                {"comment", "删除邮件"},
                {"write", new List() {
                    new Map() { {"name", "mailId"}, {"type", "u64"}, {"comment", "邮件ID"}, {"explain", new List()} }
                }},
                {"read", new List() {
                    new Map() { {"name", "result"}, {"type", "rst"}, {"comment", "结果"}, {"explain", new List()} }
                }}
            }}
        };
    }
}