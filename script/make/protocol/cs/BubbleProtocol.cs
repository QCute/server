public static class BubbleProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, System.Object data) 
    {
        switch (protocol) 
        {
            case 12101:
            {
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
                var data = new System.Collections.Generic.List<System.Object>(dataLength);
                while (dataLength-- > 0)
                {
                    // 
                    // 气泡ID
                    var bubbleId = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // 过期时间
                    var expireTime = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // object
                    var bubble = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"bubbleId", bubbleId}, {"expireTime", expireTime}};
                    // add
                    data.Add(bubble);
                }
                return data;
            }
            case 12102:
            {
                // 气泡ID列表
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<System.Object>(dataLength);
                while (dataLength-- > 0)
                {
                    // 气泡ID
                    var item = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // add
                    data.Add(item);
                }
                return data;
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }
}