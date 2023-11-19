public class FashionQueryRequest
{
    public System.UInt16 protocol = 12001;
    public Empty data;
}

public class FashionQueryResponse
{
    public System.UInt16 protocol = 12001;
    public System.Collections.Generic.List<(
        System.UInt32 fashionId,                                                // 时装ID
        System.UInt32 expireTime                                                // 过期时间
    )> data;
}



public class FashionDeleteResponse
{
    public System.UInt16 protocol = 12002;
    public System.Collections.Generic.List<System.UInt32> data;
}

public static class FashionProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, System.Object dataRaw) 
    {
        switch (protocol) 
        {
            case 12001:
            {
                #pragma warning disable
                var data = (Empty)dataRaw;
                #pragma warning restore

                return;
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }

    public static System.Object Decode(System.Text.Encoding encoding, System.IO.BinaryReader reader, System.UInt16 protocol) 
    {
        switch (protocol) 
        {
            case 12001:
            {
                // 时装列表
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<(System.UInt32 fashionId, System.UInt32 expireTime)>(dataLength);
                while (dataLength-- > 0)
                {
                    // 
                    // 时装ID
                    var dataDataFashionId = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // 过期时间
                    var dataDataExpireTime = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // object
                    var dataData = (fashionId: dataDataFashionId, expireTime: dataDataExpireTime);
                    // add
                    data.Add(dataData);
                }
                return (protocol: 12001, data: data);
            }
            case 12002:
            {
                // 时装ID列表
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<System.UInt32>(dataLength);
                while (dataLength-- > 0)
                {
                    // 时装ID
                    var dataData = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // add
                    data.Add(dataData);
                }
                return (protocol: 12002, data: data);
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }
}