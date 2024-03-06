public static class AuctionProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, System.Object data) 
    {
        switch (protocol) 
        {
            case 16101:
            {
                return;
            }
            case 16102:
            {
                // convert
                var dataCast = (System.Collections.Generic.Dictionary<System.String, System.Object>)data;
                // 拍品编号
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)dataCast["auctionNo"]));
                // 新的价格
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int32)(System.UInt32)dataCast["nextPrice"]));
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
                var data = new System.Collections.Generic.List<System.Object>(dataLength);
                while (dataLength-- > 0)
                {
                    // 拍品
                    // 拍品编号
                    var auctionNo = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 拍品ID
                    var auctionId = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // 数量
                    var number = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // 拍卖类型(1:全服/2:公会)
                    var type = reader.ReadByte();
                    // 结束时间
                    var endTime = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // 当前价格
                    var nowPrice = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // 下次出价的价格
                    var nextPrice = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // object
                    var auction = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"auction_no", auctionNo}, {"auction_id", auctionId}, {"number", number}, {"type", type}, {"end_time", endTime}, {"now_price", nowPrice}, {"next_price", nextPrice}};
                    // add
                    data.Add(auction);
                }
                return data;
            }
            case 16102:
            {
                // 
                // 结果
                var resultLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var result = encoding.GetString(reader.ReadBytes(resultLength));
                // 新的价格
                var newPrice = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                // 拍品
                // 拍品编号
                var auctionAuctionNo = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // 拍品ID
                var auctionAuctionId = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                // 拍卖类型(1:全服/2:公会)
                var auctionType = reader.ReadByte();
                // 结束时间
                var auctionEndTime = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                // 当前价格
                var auctionNowPrice = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                // 下次出价的价格
                var auctionNextPrice = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                // object
                var auction = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"auction_no", auctionAuctionNo}, {"auction_id", auctionAuctionId}, {"type", auctionType}, {"end_time", auctionEndTime}, {"now_price", auctionNowPrice}, {"next_price", auctionNextPrice}};
                // object
                var data = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"result", result}, {"new_price", newPrice}, {"auction", auction}};
                return data;
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }
}