public static class MailProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, dynamic data) 
    {
        switch (protocol) 
        {
            case 11401:
            {

                return;
            }
            case 11402:
            {
                // 邮件ID
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)data));
                return;
            }
            case 11403:
            {
                // 邮件ID
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)data));
                return;
            }
            case 11404:
            {
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
                var data = new System.Collections.Generic.List<System.Object>(dataLength);
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
                    var dataDataAttachment = new System.Collections.Generic.List<System.Object>(dataDataAttachmentLength);
                    while (dataDataAttachmentLength-- > 0)
                    {
                        // 
                        // 物品ID
                        var dataDataAttachmentDataItemId = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                        // 数量
                        var dataDataAttachmentDataNumber = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                        // object
                        var dataDataAttachmentData = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"itemId", dataDataAttachmentDataItemId}, {"number", dataDataAttachmentDataNumber}};
                        // add
                        dataDataAttachment.Add(dataDataAttachmentData);
                    }
                    // object
                    var dataData = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"mailId", dataDataMailId}, {"receiveTime", dataDataReceiveTime}, {"expireTime", dataDataExpireTime}, {"readTime", dataDataReadTime}, {"receiveAttachmentTime", dataDataReceiveAttachmentTime}, {"title", dataDataTitle}, {"content", dataDataContent}, {"attachment", dataDataAttachment}};
                    // add
                    data.Add(dataData);
                }
                return data;
            }
            case 11402:
            {
                // 结果
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = encoding.GetString(reader.ReadBytes(dataLength));
                return data;
            }
            case 11403:
            {
                // 结果
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = encoding.GetString(reader.ReadBytes(dataLength));
                return data;
            }
            case 11404:
            {
                // 结果
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = encoding.GetString(reader.ReadBytes(dataLength));
                return data;
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }
}