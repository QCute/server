public class BubbleQueryRequest
{
    public System.UInt16 protocol = 12101;
    public Empty data;
}

public class BubbleQueryResponse
{
    public System.UInt16 protocol = 12101;
    public System.Collections.Generic.List<(
        System.UInt32 bubbleId,                                                 // 气泡ID
        System.UInt32 expireTime                                                // 过期时间
    )> data;
}



public class BubbleDeleteResponse
{
    public System.UInt16 protocol = 12102;
    public System.Collections.Generic.List<System.UInt32> data;
}

public static class BubbleProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, System.Object dataRaw) 
    {
        switch (protocol) 
        {
            case 12101:
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
            case 12101:
            {
                // 气泡列表
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<(System.UInt32 bubbleId, System.UInt32 expireTime)>(dataLength);
                while (dataLength-- > 0)
                {
                    // 
                    // 气泡ID
                    var dataDataBubbleId = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // 过期时间
                    var dataDataExpireTime = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // object
                    var dataData = (bubbleId: dataDataBubbleId, expireTime: dataDataExpireTime);
                    // add
                    data.Add(dataData);
                }
                return (protocol: 12101, data: data);
            }
            case 12102:
            {
                // 气泡ID列表
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<System.UInt32>(dataLength);
                while (dataLength-- > 0)
                {
                    // 气泡ID
                    var dataData = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // add
                    data.Add(dataData);
                }
                return (protocol: 12102, data: data);
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }
}