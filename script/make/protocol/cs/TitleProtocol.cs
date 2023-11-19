public class TitleQueryRequest
{
    public System.UInt16 protocol = 11901;
    public Empty data;
}

public class TitleQueryResponse
{
    public System.UInt16 protocol = 11901;
    public System.Collections.Generic.List<(
        System.UInt32 titleId,                                                  // 称号ID
        System.UInt32 expireTime                                                // 过期时间
    )> data;
}



public class TitleDeleteResponse
{
    public System.UInt16 protocol = 11902;
    public System.Collections.Generic.List<System.UInt32> data;
}

public static class TitleProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, System.Object dataRaw) 
    {
        switch (protocol) 
        {
            case 11901:
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
            case 11901:
            {
                // 称号列表
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<(System.UInt32 titleId, System.UInt32 expireTime)>(dataLength);
                while (dataLength-- > 0)
                {
                    // 
                    // 称号ID
                    var dataDataTitleId = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // 过期时间
                    var dataDataExpireTime = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // object
                    var dataData = (titleId: dataDataTitleId, expireTime: dataDataExpireTime);
                    // add
                    data.Add(dataData);
                }
                return (protocol: 11901, data: data);
            }
            case 11902:
            {
                // 称号ID列表
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<System.UInt32>(dataLength);
                while (dataLength-- > 0)
                {
                    // 称号ID
                    var dataData = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // add
                    data.Add(dataData);
                }
                return (protocol: 11902, data: data);
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }
}