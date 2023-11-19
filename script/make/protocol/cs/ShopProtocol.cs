public class ShopQueryRequest
{
    public System.UInt16 protocol = 11301;
    public Empty data;
}

public class ShopQueryResponse
{
    public System.UInt16 protocol = 11301;
    public System.Collections.Generic.List<(
        System.UInt32 shopId,                                                   // 商店ID
        System.UInt16 number                                                    // 数量
    )> data;
}

public class ShopBuyRequest
{
    public System.UInt16 protocol = 11302;
    public (
        System.UInt32 shopId,                                                   // 商店ID
        System.UInt16 number                                                    // 数量
    ) data;
}

public class ShopBuyResponse
{
    public System.UInt16 protocol = 11302;
    public System.String data;
}

public static class ShopProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, System.Object dataRaw) 
    {
        switch (protocol) 
        {
            case 11301:
            {
                #pragma warning disable
                var data = (Empty)dataRaw;
                #pragma warning restore

                return;
            }
            case 11302:
            {
                var data = ((System.UInt32 shopId, System.UInt16 number))dataRaw;

                // 商店ID
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int32)(System.UInt32)data.shopId));
                // 数量
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)(System.UInt16)data.number));
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
                var data = new System.Collections.Generic.List<(System.UInt32 shopId, System.UInt16 number)>(dataLength);
                while (dataLength-- > 0)
                {
                    // 
                    // 商店ID
                    var dataDataShopId = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // 数量
                    var dataDataNumber = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // object
                    var dataData = (shopId: dataDataShopId, number: dataDataNumber);
                    // add
                    data.Add(dataData);
                }
                return (protocol: 11301, data: data);
            }
            case 11302:
            {
                // 结果
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = encoding.GetString(reader.ReadBytes(dataLength));
                return (protocol: 11302, data: data);
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }
}