public class BuffQueryRequest
{
    public System.UInt16 protocol = 11801;
    public Empty data;
}

public class BuffQueryResponse
{
    public System.UInt16 protocol = 11801;
    public System.Collections.Generic.List<(
        System.UInt32 buffId,                                                   // BuffID
        System.UInt32 expireTime,                                               // 结束时间
        System.UInt16 overlap                                                   // 叠加数量
    )> data;
}



public class BuffDeleteResponse
{
    public System.UInt16 protocol = 11802;
    public System.Collections.Generic.List<System.UInt32> data;
}

public static class BuffProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, System.Object dataRaw) 
    {
        switch (protocol) 
        {
            case 11801:
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
            case 11801:
            {
                // Buff列表
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<(System.UInt32 buffId, System.UInt32 expireTime, System.UInt16 overlap)>(dataLength);
                while (dataLength-- > 0)
                {
                    // 
                    // BuffID
                    var dataDataBuffId = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // 结束时间
                    var dataDataExpireTime = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // 叠加数量
                    var dataDataOverlap = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // object
                    var dataData = (buffId: dataDataBuffId, expireTime: dataDataExpireTime, overlap: dataDataOverlap);
                    // add
                    data.Add(dataData);
                }
                return (protocol: 11801, data: data);
            }
            case 11802:
            {
                // BuffID列表
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<System.UInt32>(dataLength);
                while (dataLength-- > 0)
                {
                    // BuffID
                    var dataData = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // add
                    data.Add(dataData);
                }
                return (protocol: 11802, data: data);
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }
}