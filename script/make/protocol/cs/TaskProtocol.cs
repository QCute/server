public static class TaskProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, dynamic data) 
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
                    var dataDataTaskId = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // 当前数量
                    var dataDataNumber = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // 是否领取奖励
                    var dataDataIsAward = reader.ReadByte();
                    // object
                    var dataData = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"taskId", dataDataTaskId}, {"number", dataDataNumber}, {"isAward", dataDataIsAward}};
                    // add
                    data.Add(dataData);
                }
                return data;
            }
            case 11202:
            {
                // 
                // 结果
                var dataResultLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var dataResult = encoding.GetString(reader.ReadBytes(dataResultLength));
                // 
                // 任务ID
                var dataTaskTaskId = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                // 当前数量
                var dataTaskNumber = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                // 是否领取奖励
                var dataTaskIsAward = reader.ReadByte();
                // object
                var dataTask = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"taskId", dataTaskTaskId}, {"number", dataTaskNumber}, {"isAward", dataTaskIsAward}};
                // object
                var data = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"result", dataResult}, {"task", dataTask}};
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