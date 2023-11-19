public static class AuctionProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, dynamic data) 
    {
        switch (protocol) 
        {
            case 16101:
            {

                return;
            }
            case 16102:
            {

                // 拍品编号
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)data["auctionNo"]));
                // 新的价格
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int32)(System.UInt32)data["nextPrice"]));
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
                    var dataData = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"auctionNo", dataDataAuctionNo}, {"auctionId", dataDataAuctionId}, {"number", dataDataNumber}, {"type", dataDataType}, {"endTime", dataDataEndTime}, {"nowPrice", dataDataNowPrice}, {"nextPrice", dataDataNextPrice}};
                    // add
                    data.Add(dataData);
                }
                return data;
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
                var dataAuction = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"auctionNo", dataAuctionAuctionNo}, {"auctionId", dataAuctionAuctionId}, {"type", dataAuctionType}, {"endTime", dataAuctionEndTime}, {"nowPrice", dataAuctionNowPrice}, {"nextPrice", dataAuctionNextPrice}};
                // object
                var data = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"result", dataResult}, {"newPrice", dataNewPrice}, {"auction", dataAuction}};
                return data;
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }
}