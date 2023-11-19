public static class DailyProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, dynamic data) 
    {
        switch (protocol) 
        {
            case 12301:
            {

                return;
            }
            case 12302:
            {

                return;
            }
            case 12303:
            {
                // 日常ID
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int32)(System.UInt32)data));
                return;
            }
            case 12304:
            {
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
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<System.Object>(dataLength);
                while (dataLength-- > 0)
                {
                    // 
                    // 统计类型
                    var dataDataType = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // 今日数量
                    var dataDataTodayNumber = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // object
                    var dataData = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"type", dataDataType}, {"todayNumber", dataDataTodayNumber}};
                    // add
                    data.Add(dataData);
                }
                return data;
            }
            case 12302:
            {
                // 
                // 
                // 日常ID
                var dataDailyDailyId = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                // 是否领取奖励
                var dataDailyIsAward = reader.ReadByte();
                // object
                var dataDaily = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"dailyId", dataDailyDailyId}, {"isAward", dataDailyIsAward}};
                // 
                // 奖励阶段ID
                var dataDailyActiveStageId = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                // 活跃度
                var dataDailyActiveScore = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                // object
                var dataDailyActive = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"stageId", dataDailyActiveStageId}, {"score", dataDailyActiveScore}};
                // object
                var data = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"daily", dataDaily}, {"dailyActive", dataDailyActive}};
                return data;
            }
            case 12303:
            {
                // 结果
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = encoding.GetString(reader.ReadBytes(dataLength));
                return data;
            }
            case 12304:
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