public static class TaskProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, System.Object data) 
    {
        switch (protocol) 
        {
            case 11201:
            {
                return;
            }
            case 11202:
            {
                // 任务ID
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int32)(System.UInt32)data));
                return;
            }
            case 11203:
            {
                // 任务ID
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
            case 11201:
            {
                // 任务列表
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<System.Object>(dataLength);
                while (dataLength-- > 0)
                {
                    // 
                    // 任务ID
                    var taskId = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // 当前数量
                    var number = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // 是否领取奖励
                    var isAward = reader.ReadByte();
                    // object
                    var task = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"taskId", taskId}, {"number", number}, {"isAward", isAward}};
                    // add
                    data.Add(task);
                }
                return data;
            }
            case 11202:
            {
                // 
                // 结果
                var resultLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var result = encoding.GetString(reader.ReadBytes(resultLength));
                // 
                // 任务ID
                var taskTaskId = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                // 当前数量
                var taskNumber = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                // 是否领取奖励
                var taskIsAward = reader.ReadByte();
                // object
                var task = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"taskId", taskTaskId}, {"number", taskNumber}, {"isAward", taskIsAward}};
                // object
                var data = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"result", result}, {"task", task}};
                return data;
            }
            case 11203:
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