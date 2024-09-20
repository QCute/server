public static class DailyProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, System.Collections.Generic.Dictionary<System.String, System.Object> data) 
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
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int32)(System.UInt32)data["dailyId"]));
                return;
            }
            case 12304:
            {
                // 阶段ID
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int32)(System.UInt32)data["stageId"]));
                return;
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }

    public static System.Collections.Generic.Dictionary<System.String, System.Object> Decode(System.Text.Encoding encoding, System.IO.BinaryReader reader, System.UInt16 protocol) 
    {
        switch (protocol) 
        {
            case 12301:
            {
                // 统计列表
                var listLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var list = new System.Collections.Generic.List<System.Object>(listLength);
                while (listLength-- > 0)
                {
                    // Count
                    // 统计类型
                    var type = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // 今日数量
                    var todayNumber = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // object
                    var count = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"type", type}, {"todayNumber", todayNumber}};
                    // add
                    list.Add(count);
                }
                return new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"list", list}};
            }
            case 12302:
            {
                // 日常列表
                var listLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var list = new System.Collections.Generic.List<System.Object>(listLength);
                while (listLength-- > 0)
                {
                    // Daily
                    // 日常ID
                    var dailyId = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // 是否领取奖励
                    var isAward = reader.ReadByte();
                    // object
                    var daily = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"dailyId", dailyId}, {"isAward", isAward}};
                    // add
                    list.Add(daily);
                }
                // DailyActive
                // 奖励阶段ID
                var stageId = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                // 活跃度
                var score = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                // object
                var dailyActive = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"stageId", stageId}, {"score", score}};
                return new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"list", list}, {"dailyActive", dailyActive}};
            }
            case 12303:
            {
                // 结果
                var resultLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var result = encoding.GetString(reader.ReadBytes(resultLength));
                return new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"result", result}};
            }
            case 12304:
            {
                // 结果
                var resultLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var result = encoding.GetString(reader.ReadBytes(resultLength));
                return new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"result", result}};
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }
}