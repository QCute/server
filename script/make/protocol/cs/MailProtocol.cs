public class MailQueryRequest
{
    public System.UInt16 protocol = 11401;
    public Empty data;
}

public class MailQueryResponse
{
    public System.UInt16 protocol = 11401;
    public System.Collections.Generic.List<(
        System.UInt64 mailId,                                                   // 邮件ID
        System.UInt32 receiveTime,                                              // 接收时间
        System.UInt32 expireTime,                                               // 有效时间
        System.UInt32 readTime,                                                 // 读取时间
        System.UInt32 receiveAttachmentTime,                                    // 领取附件时间
        System.String title,                                                    // 标题
        System.String content,                                                  // 内容
        System.Collections.Generic.List<(
            System.UInt32 itemId,                                               // 物品ID
            System.UInt16 number                                                // 数量
        )> attachment                                                           // 附件列表
    )> data;
}

public class MailReadRequest
{
    public System.UInt16 protocol = 11402;
    public System.UInt64 data;
}

public class MailReadResponse
{
    public System.UInt16 protocol = 11402;
    public System.String data;
}

public class MailReceiveAttachmentRequest
{
    public System.UInt16 protocol = 11403;
    public System.UInt64 data;
}

public class MailReceiveAttachmentResponse
{
    public System.UInt16 protocol = 11403;
    public System.String data;
}

public class MailDeleteRequest
{
    public System.UInt16 protocol = 11404;
    public System.UInt64 data;
}

public class MailDeleteResponse
{
    public System.UInt16 protocol = 11404;
    public System.String data;
}

public static class MailProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, System.Object dataRaw) 
    {
        switch (protocol) 
        {
            case 11401:
            {
                #pragma warning disable
                var data = (Empty)dataRaw;
                #pragma warning restore

                return;
            }
            case 11402:
            {
                var data = (System.UInt64)dataRaw;
                // 邮件ID
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)data));
                return;
            }
            case 11403:
            {
                var data = (System.UInt64)dataRaw;
                // 邮件ID
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)data));
                return;
            }
            case 11404:
            {
                var data = (System.UInt64)dataRaw;
                // 邮件ID
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)data));
                return;
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }

    public static System.Object Decode(System.Text.Encoding encoding, System.IO.BinaryReader reader, System.UInt16 protocol) 
    {
        switch (protocol) 
        {
            case 11401:
            {
                // 
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<(System.UInt64 mailId, System.UInt32 receiveTime, System.UInt32 expireTime, System.UInt32 readTime, System.UInt32 receiveAttachmentTime, System.String title, System.String content, System.Collections.Generic.List<(System.UInt32 itemId, System.UInt16 number)> attachment)>(dataLength);
                while (dataLength-- > 0)
                {
                    // 
                    // 邮件ID
                    var dataDataMailId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 接收时间
                    var dataDataReceiveTime = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // 有效时间
                    var dataDataExpireTime = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // 读取时间
                    var dataDataReadTime = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // 领取附件时间
                    var dataDataReceiveAttachmentTime = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // 标题
                    var dataDataTitleLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var dataDataTitle = encoding.GetString(reader.ReadBytes(dataDataTitleLength));
                    // 内容
                    var dataDataContentLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var dataDataContent = encoding.GetString(reader.ReadBytes(dataDataContentLength));
                    // 附件列表
                    var dataDataAttachmentLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var dataDataAttachment = new System.Collections.Generic.List<(System.UInt32 itemId, System.UInt16 number)>(dataDataAttachmentLength);
                    while (dataDataAttachmentLength-- > 0)
                    {
                        // 
                        // 物品ID
                        var dataDataAttachmentDataItemId = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                        // 数量
                        var dataDataAttachmentDataNumber = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                        // object
                        var dataDataAttachmentData = (itemId: dataDataAttachmentDataItemId, number: dataDataAttachmentDataNumber);
                        // add
                        dataDataAttachment.Add(dataDataAttachmentData);
                    }
                    // object
                    var dataData = (mailId: dataDataMailId, receiveTime: dataDataReceiveTime, expireTime: dataDataExpireTime, readTime: dataDataReadTime, receiveAttachmentTime: dataDataReceiveAttachmentTime, title: dataDataTitle, content: dataDataContent, attachment: dataDataAttachment);
                    // add
                    data.Add(dataData);
                }
                return (protocol: 11401, data: data);
            }
            case 11402:
            {
                // 结果
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = encoding.GetString(reader.ReadBytes(dataLength));
                return (protocol: 11402, data: data);
            }
            case 11403:
            {
                // 结果
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = encoding.GetString(reader.ReadBytes(dataLength));
                return (protocol: 11403, data: data);
            }
            case 11404:
            {
                // 结果
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = encoding.GetString(reader.ReadBytes(dataLength));
                return (protocol: 11404, data: data);
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }
}