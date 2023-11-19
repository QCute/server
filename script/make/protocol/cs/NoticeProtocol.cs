public class NoticeQueryRequest
{
    public System.UInt16 protocol = 50001;
    public Empty data;
}

public class NoticeQueryResponse
{
    public System.UInt16 protocol = 50001;
    public System.Collections.Generic.List<(
        System.UInt64 noticeId,                                                 // 公告ID
        System.UInt32 receiveTime,                                              // 收到时间
        System.UInt32 readTime,                                                 // 读取时间
        System.String title,                                                    // 标题
        System.String content                                                   // 内容
    )> data;
}



public class NoticeBroadcastResponse
{
    public System.UInt16 protocol = 50002;
    public (
        System.Byte scope,                                                      // 范围
        System.Byte type,                                                       // 类型
        System.String title,                                                    // 标题
        System.String msg                                                       // 消息
    ) data;
}

public static class NoticeProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, System.Object dataRaw) 
    {
        switch (protocol) 
        {
            case 50001:
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
            case 50001:
            {
                // 公告列表
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<(System.UInt64 noticeId, System.UInt32 receiveTime, System.UInt32 readTime, System.String title, System.String content)>(dataLength);
                while (dataLength-- > 0)
                {
                    // 
                    // 公告ID
                    var dataDataNoticeId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 收到时间
                    var dataDataReceiveTime = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // 读取时间
                    var dataDataReadTime = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // 标题
                    var dataDataTitleLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var dataDataTitle = encoding.GetString(reader.ReadBytes(dataDataTitleLength));
                    // 内容
                    var dataDataContentLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var dataDataContent = encoding.GetString(reader.ReadBytes(dataDataContentLength));
                    // object
                    var dataData = (noticeId: dataDataNoticeId, receiveTime: dataDataReceiveTime, readTime: dataDataReadTime, title: dataDataTitle, content: dataDataContent);
                    // add
                    data.Add(dataData);
                }
                return (protocol: 50001, data: data);
            }
            case 50002:
            {
                // 
                // 范围
                var dataScope = reader.ReadByte();
                // 类型
                var dataType = reader.ReadByte();
                // 标题
                var dataTitleLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var dataTitle = encoding.GetString(reader.ReadBytes(dataTitleLength));
                // 消息
                var dataMsgLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var dataMsg = encoding.GetString(reader.ReadBytes(dataMsgLength));
                // object
                var data = (scope: dataScope, type: dataType, title: dataTitle, msg: dataMsg);
                return (protocol: 50002, data: data);
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }
}