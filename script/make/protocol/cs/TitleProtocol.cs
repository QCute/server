public static class TitleProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, System.Collections.Generic.Dictionary<System.String, System.Object> data) 
    {
        switch (protocol) 
        {
            case 11901:
            {
                return;
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }

    public static System.Collections.Generic.Dictionary<System.String, System.Object> Decode(System.Text.Encoding encoding, System.IO.BinaryReader reader, System.UInt16 protocol) 
    {
        switch (protocol) 
        {
            case 11901:
            {
                // 称号列表
                var listLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var list = new System.Collections.Generic.List<System.Object>(listLength);
                while (listLength-- > 0)
                {
                    // Title
                    // 称号ID
                    var titleId = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // 过期时间
                    var expireTime = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // object
                    var title = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"titleId", titleId}, {"expireTime", expireTime}};
                    // add
                    list.Add(title);
                }
                return new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"list", list}};
            }
            case 11902:
            {
                // 称号ID列表
                var listLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var list = new System.Collections.Generic.List<System.Object>(listLength);
                while (listLength-- > 0)
                {
                    // Title
                    // 称号ID
                    var titleId = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // object
                    var title = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"titleId", titleId}};
                    // add
                    list.Add(title);
                }
                return new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"list", list}};
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }
}