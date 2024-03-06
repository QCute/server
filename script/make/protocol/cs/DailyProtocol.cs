public static class DailyProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, System.Object data) 
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
                    var type = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // 今日数量
                    var todayNumber = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // object
                    var count = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"type", type}, {"today_number", todayNumber}};
                    // add
                    data.Add(count);
                }
                return data;
            }
            case 12302:
            {
                // 
                // 
                // 日常ID
                var dailyDailyId = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                // 是否领取奖励
                var dailyIsAward = reader.ReadByte();
                // object
                var daily = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"daily_id", dailyDailyId}, {"is_award", dailyIsAward}};
                // 
                // 奖励阶段ID
                var dailyActiveStageId = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                // 活跃度
                var dailyActiveScore = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                // object
                var dailyActive = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"stage_id", dailyActiveStageId}, {"score", dailyActiveScore}};
                // object
                var data = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"daily", daily}, {"daily_active", dailyActive}};
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