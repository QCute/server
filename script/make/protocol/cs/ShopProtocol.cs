public static class ShopProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, System.Object data) 
    {
        switch (protocol) 
        {
            case 11301:
            {
                return;
            }
            case 11302:
            {
                // convert
                var dataCast = (System.Collections.Generic.Dictionary<System.String, System.Object>)data;
                // 商店ID
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int32)(System.UInt32)dataCast["shopId"]));
                // 数量
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)(System.UInt16)dataCast["number"]));
                return;
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }

    public static System.Object Decode(System.Text.Encoding encoding, System.IO.BinaryReader reader, System.UInt16 protocol) 
    {
        switch (protocol) 
        {
            case 11301:
            {
                // 已购买列表
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<System.Object>(dataLength);
                while (dataLength-- > 0)
                {
                    // 
                    // 商店ID
                    var shopId = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // 数量
                    var number = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // object
                    var shop = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"shopId", shopId}, {"number", number}};
                    // add
                    data.Add(shop);
                }
                return data;
            }
            case 11302:
            {
                // 结果
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = encoding.GetString(reader.ReadBytes(dataLength));
                return data;
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }
}