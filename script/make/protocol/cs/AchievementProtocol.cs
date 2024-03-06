public static class AchievementProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, System.Object data) 
    {
        switch (protocol) 
        {
            case 12201:
            {
                return;
            }
            case 12202:
            {
                return;
            }
            case 12203:
            {
                // 成就ID
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int32)(System.UInt32)data));
                return;
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }

    public static System.Object Decode(System.Text.Encoding encoding, System.IO.BinaryReader reader, System.UInt16 protocol) 
    {
        switch (protocol) 
        {
            case 12201:
            {
                // 统计列表
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<System.Object>(dataLength);
                while (dataLength-- > 0)
                {
                    // 
                    // 统计类型
                    var type = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // 总数
                    var totalNumber = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // object
                    var count = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"type", type}, {"totalNumber", totalNumber}};
                    // add
                    data.Add(count);
                }
                return data;
            }
            case 12202:
            {
                // 成就列表
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<System.Object>(dataLength);
                while (dataLength-- > 0)
                {
                    // 
                    // 成就ID
                    var achievementId = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // 成就类型
                    var type = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // object
                    var achievement = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"achievementId", achievementId}, {"type", type}};
                    // add
                    data.Add(achievement);
                }
                return data;
            }
            case 12203:
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