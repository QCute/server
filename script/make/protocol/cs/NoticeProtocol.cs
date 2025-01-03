public static class NoticeProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, System.Object data) 
    {
        switch (protocol) 
        {
            case 50001:
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
            case 50001:
            {
                // 公告列表
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<System.Object>(dataLength);
                while (dataLength-- > 0)
                {
                    // 
                    // 公告ID
                    var noticeId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 收到时间
                    var receiveTime = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // 读取时间
                    var readTime = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // 标题
                    var titleLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var title = encoding.GetString(reader.ReadBytes(titleLength));
                    // 内容
                    var contentLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var content = encoding.GetString(reader.ReadBytes(contentLength));
                    // object
                    var noticeRole = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"noticeId", noticeId}, {"receiveTime", receiveTime}, {"readTime", readTime}, {"title", title}, {"content", content}};
                    // add
                    data.Add(noticeRole);
                }
                return data;
            }
            case 50002:
            {
                // 
                // 范围
                var scope = reader.ReadByte();
                // 类型
                var type = reader.ReadByte();
                // 标题
                var titleLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var title = encoding.GetString(reader.ReadBytes(titleLength));
                // 消息
                var msgLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var msg = encoding.GetString(reader.ReadBytes(msgLength));
                // object
                var data = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"scope", scope}, {"type", type}, {"title", title}, {"msg", msg}};
                return data;
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }
}