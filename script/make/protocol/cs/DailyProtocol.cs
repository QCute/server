public class DailyQueryActiveRequest
{
    public System.UInt16 protocol = 12301;
    public Empty data;
}

public class DailyQueryActiveResponse
{
    public System.UInt16 protocol = 12301;
    public (
        System.UInt32 stageId,                                                  // 奖励阶段ID
        System.UInt32 score                                                     // 活跃度
    ) data;
}

public class DailyQueryRequest
{
    public System.UInt16 protocol = 12302;
    public Empty data;
}

public class DailyQueryResponse
{
    public System.UInt16 protocol = 12302;
    public System.Collections.Generic.List<(
        System.UInt32 dailyId,                                                  // 日常ID
        System.Byte isAward                                                     // 是否领取奖励
    )> data;
}

public class DailyAwardRequest
{
    public System.UInt16 protocol = 12303;
    public System.UInt32 data;
}

public class DailyAwardResponse
{
    public System.UInt16 protocol = 12303;
    public System.String data;
}

public class DailyAwardActiveRequest
{
    public System.UInt16 protocol = 12304;
    public System.UInt32 data;
}

public class DailyAwardActiveResponse
{
    public System.UInt16 protocol = 12304;
    public System.String data;
}

public static class DailyProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, System.Object dataRaw) 
    {
        switch (protocol) 
        {
            case 12301:
            {
                #pragma warning disable
                var data = (Empty)dataRaw;
                #pragma warning restore

                return;
            }
            case 12302:
            {
                #pragma warning disable
                var data = (Empty)dataRaw;
                #pragma warning restore

                return;
            }
            case 12303:
            {
                var data = (System.UInt32)dataRaw;
                // 日常ID
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int32)(System.UInt32)data));
                return;
            }
            case 12304:
            {
                var data = (System.UInt32)dataRaw;
                // 阶段ID
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
            case 12301:
            {
                // 
                // 奖励阶段ID
                var dataStageId = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                // 活跃度
                var dataScore = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                // object
                var data = (stageId: dataStageId, score: dataScore);
                return (protocol: 12301, data: data);
            }
            case 12302:
            {
                // 
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<(System.UInt32 dailyId, System.Byte isAward)>(dataLength);
                while (dataLength-- > 0)
                {
                    // 
                    // 日常ID
                    var dataDataDailyId = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // 是否领取奖励
                    var dataDataIsAward = reader.ReadByte();
                    // object
                    var dataData = (dailyId: dataDataDailyId, isAward: dataDataIsAward);
                    // add
                    data.Add(dataData);
                }
                return (protocol: 12302, data: data);
            }
            case 12303:
            {
                // 结果
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = encoding.GetString(reader.ReadBytes(dataLength));
                return (protocol: 12303, data: data);
            }
            case 12304:
            {
                // 结果
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = encoding.GetString(reader.ReadBytes(dataLength));
                return (protocol: 12304, data: data);
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }
}