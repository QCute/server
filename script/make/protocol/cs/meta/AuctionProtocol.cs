using List = System.Collections.ArrayList;
using Map = System.Collections.Generic.Dictionary<System.String, System.Object>;

public static class AuctionProtocol
{
    public static Map GetMeta()
    {
        return new Map()
        {
            {"16101", new Map() {
                {"comment", "拍品列表"},
                {"write", new List() },
                {"read", new List() {
                    new Map() { {"name", "list"}, {"type", "list"}, {"comment", "拍品列表"}, {"explain", new List() {
                        new Map() { {"name", "auctionNo"}, {"type", "u64"}, {"comment", "拍品编号"}, {"explain", new List()} },
                        new Map() { {"name", "auctionId"}, {"type", "u32"}, {"comment", "拍品ID"}, {"explain", new List()} },
                        new Map() { {"name", "number"}, {"type", "u16"}, {"comment", "数量"}, {"explain", new List()} },
                        new Map() { {"name", "type"}, {"type", "u8"}, {"comment", "拍卖类型(1:全服/2:公会)"}, {"explain", new List()} },
                        new Map() { {"name", "endTime"}, {"type", "u32"}, {"comment", "结束时间"}, {"explain", new List()} },
                        new Map() { {"name", "nowPrice"}, {"type", "u32"}, {"comment", "当前价格"}, {"explain", new List()} },
                        new Map() { {"name", "nextPrice"}, {"type", "u32"}, {"comment", "下次出价的价格"}, {"explain", new List()} }
                    }}}
                }}
            }},
            {"16102", new Map() {
                {"comment", "竞价"},
                {"write", new List() {
                    new Map() { {"name", "auctionNo"}, {"type", "u64"}, {"comment", "拍品编号"}, {"explain", new List()} },
                    new Map() { {"name", "nextPrice"}, {"type", "u32"}, {"comment", "新的价格"}, {"explain", new List()} }
                }},
                {"read", new List() {
                    new Map() { {"name", "result"}, {"type", "rst"}, {"comment", "结果"}, {"explain", new List()} },
                    new Map() { {"name", "newPrice"}, {"type", "u32"}, {"comment", "新的价格"}, {"explain", new List()} },
                    new Map() { {"name", "auctionNo"}, {"type", "u64"}, {"comment", "拍品编号"}, {"explain", new List()} },
                    new Map() { {"name", "auctionId"}, {"type", "u32"}, {"comment", "拍品ID"}, {"explain", new List()} },
                    new Map() { {"name", "type"}, {"type", "u8"}, {"comment", "拍卖类型(1:全服/2:公会)"}, {"explain", new List()} },
                    new Map() { {"name", "endTime"}, {"type", "u32"}, {"comment", "结束时间"}, {"explain", new List()} },
                    new Map() { {"name", "nowPrice"}, {"type", "u32"}, {"comment", "当前价格"}, {"explain", new List()} },
                    new Map() { {"name", "nextPrice"}, {"type", "u32"}, {"comment", "下次出价的价格"}, {"explain", new List()} }
                }}
            }}
        };
    }
}