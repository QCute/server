public class AchievementQueryCountRequest
{
    public System.UInt16 protocol = 12201;
    public Empty data;
}

public class AchievementQueryCountResponse
{
    public System.UInt16 protocol = 12201;
    public System.Collections.Generic.List<(
        System.UInt32 type,                                                     // 统计类型
        System.UInt32 totalNumber                                               // 总数
    )> data;
}

public class AchievementQueryRequest
{
    public System.UInt16 protocol = 12202;
    public Empty data;
}

public class AchievementQueryResponse
{
    public System.UInt16 protocol = 12202;
    public System.Collections.Generic.List<(
        System.UInt32 achievementId,                                            // 成就ID
        System.UInt32 type                                                      // 成就类型
    )> data;
}

public class AchievementAwardRequest
{
    public System.UInt16 protocol = 12203;
    public System.UInt32 data;
}

public class AchievementAwardResponse
{
    public System.UInt16 protocol = 12203;
    public System.String data;
}

public static class AchievementProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, System.Object dataRaw) 
    {
        switch (protocol) 
        {
            case 12201:
            {
                #pragma warning disable
                var data = (Empty)dataRaw;
                #pragma warning restore

                return;
            }
            case 12202:
            {
                #pragma warning disable
                var data = (Empty)dataRaw;
                #pragma warning restore

                return;
            }
            case 12203:
            {
                var data = (System.UInt32)dataRaw;
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
                var data = new System.Collections.Generic.List<(System.UInt32 type, System.UInt32 totalNumber)>(dataLength);
                while (dataLength-- > 0)
                {
                    // 
                    // 统计类型
                    var dataDataType = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // 总数
                    var dataDataTotalNumber = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // object
                    var dataData = (type: dataDataType, totalNumber: dataDataTotalNumber);
                    // add
                    data.Add(dataData);
                }
                return (protocol: 12201, data: data);
            }
            case 12202:
            {
                // 成就列表
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<(System.UInt32 achievementId, System.UInt32 type)>(dataLength);
                while (dataLength-- > 0)
                {
                    // 
                    // 成就ID
                    var dataDataAchievementId = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // 成就类型
                    var dataDataType = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // object
                    var dataData = (achievementId: dataDataAchievementId, type: dataDataType);
                    // add
                    data.Add(dataData);
                }
                return (protocol: 12202, data: data);
            }
            case 12203:
            {
                // 结果
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = encoding.GetString(reader.ReadBytes(dataLength));
                return (protocol: 12203, data: data);
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }
}