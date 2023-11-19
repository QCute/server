public class AuctionQueryRequest
{
    public System.UInt16 protocol = 16101;
    public Empty data;
}

public class AuctionQueryResponse
{
    public System.UInt16 protocol = 16101;
    public System.Collections.Generic.List<(
        System.UInt64 auctionNo,                                                // 拍品编号
        System.UInt32 auctionId,                                                // 拍品ID
        System.UInt16 number,                                                   // 数量
        System.Byte type,                                                       // 拍卖类型(1:全服/2:公会)
        System.UInt32 endTime,                                                  // 结束时间
        System.UInt32 nowPrice,                                                 // 当前价格
        System.UInt32 nextPrice                                                 // 下次出价的价格
    )> data;
}

public class AuctionBidRequest
{
    public System.UInt16 protocol = 16102;
    public (
        System.UInt64 auctionNo,                                                // 拍品编号
        System.UInt32 nextPrice                                                 // 新的价格
    ) data;
}

public class AuctionBidResponse
{
    public System.UInt16 protocol = 16102;
    public (
        System.String result,                                                   // 结果
        System.UInt32 newPrice,                                                 // 新的价格
        (
            System.UInt64 auctionNo,                                            // 拍品编号
            System.UInt32 auctionId,                                            // 拍品ID
            System.Byte type,                                                   // 拍卖类型(1:全服/2:公会)
            System.UInt32 endTime,                                              // 结束时间
            System.UInt32 nowPrice,                                             // 当前价格
            System.UInt32 nextPrice                                             // 下次出价的价格
        ) auction                                                               // 拍品
    ) data;
}

public static class AuctionProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, System.Object dataRaw) 
    {
        switch (protocol) 
        {
            case 16101:
            {
                #pragma warning disable
                var data = (Empty)dataRaw;
                #pragma warning restore

                return;
            }
            case 16102:
            {
                var data = ((System.UInt64 auctionNo, System.UInt32 nextPrice))dataRaw;

                // 拍品编号
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)data.auctionNo));
                // 新的价格
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int32)(System.UInt32)data.nextPrice));
                return;
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }

    public static System.Object Decode(System.Text.Encoding encoding, System.IO.BinaryReader reader, System.UInt16 protocol) 
    {
        switch (protocol) 
        {
            case 16101:
            {
                // 拍品列表
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<(System.UInt64 auctionNo, System.UInt32 auctionId, System.UInt16 number, System.Byte type, System.UInt32 endTime, System.UInt32 nowPrice, System.UInt32 nextPrice)>(dataLength);
                while (dataLength-- > 0)
                {
                    // 拍品
                    // 拍品编号
                    var dataDataAuctionNo = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 拍品ID
                    var dataDataAuctionId = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // 数量
                    var dataDataNumber = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // 拍卖类型(1:全服/2:公会)
                    var dataDataType = reader.ReadByte();
                    // 结束时间
                    var dataDataEndTime = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // 当前价格
                    var dataDataNowPrice = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // 下次出价的价格
                    var dataDataNextPrice = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // object
                    var dataData = (auctionNo: dataDataAuctionNo, auctionId: dataDataAuctionId, number: dataDataNumber, type: dataDataType, endTime: dataDataEndTime, nowPrice: dataDataNowPrice, nextPrice: dataDataNextPrice);
                    // add
                    data.Add(dataData);
                }
                return (protocol: 16101, data: data);
            }
            case 16102:
            {
                // 
                // 结果
                var dataResultLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var dataResult = encoding.GetString(reader.ReadBytes(dataResultLength));
                // 新的价格
                var dataNewPrice = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                // 拍品
                // 拍品编号
                var dataAuctionAuctionNo = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // 拍品ID
                var dataAuctionAuctionId = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                // 拍卖类型(1:全服/2:公会)
                var dataAuctionType = reader.ReadByte();
                // 结束时间
                var dataAuctionEndTime = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                // 当前价格
                var dataAuctionNowPrice = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                // 下次出价的价格
                var dataAuctionNextPrice = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                // object
                var dataAuction = (auctionNo: dataAuctionAuctionNo, auctionId: dataAuctionAuctionId, type: dataAuctionType, endTime: dataAuctionEndTime, nowPrice: dataAuctionNowPrice, nextPrice: dataAuctionNextPrice);
                // object
                var data = (result: dataResult, newPrice: dataNewPrice, auction: dataAuction);
                return (protocol: 16102, data: data);
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }
}