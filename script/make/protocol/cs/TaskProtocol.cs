public class TaskQueryRequest
{
    public System.UInt16 protocol = 11201;
    public Empty data;
}

public class TaskQueryResponse
{
    public System.UInt16 protocol = 11201;
    public System.Collections.Generic.List<(
        System.UInt32 taskId,                                                   // 任务ID
        System.UInt16 number,                                                   // 当前数量
        System.Byte isAward                                                     // 是否领取奖励
    )> data;
}

public class TaskAcceptRequest
{
    public System.UInt16 protocol = 11202;
    public System.UInt32 data;
}

public class TaskAcceptResponse
{
    public System.UInt16 protocol = 11202;
    public (
        System.String result,                                                   // 结果
        (
            System.UInt32 taskId,                                               // 任务ID
            System.UInt16 number,                                               // 当前数量
            System.Byte isAward                                                 // 是否领取奖励
        ) task                                                                  // 
    ) data;
}

public class TaskSubmitRequest
{
    public System.UInt16 protocol = 11203;
    public System.UInt32 data;
}

public class TaskSubmitResponse
{
    public System.UInt16 protocol = 11203;
    public System.String data;
}

public static class TaskProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, System.Object dataRaw) 
    {
        switch (protocol) 
        {
            case 11201:
            {
                #pragma warning disable
                var data = (Empty)dataRaw;
                #pragma warning restore

                return;
            }
            case 11202:
            {
                var data = (System.UInt32)dataRaw;
                // 任务ID
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int32)(System.UInt32)data));
                return;
            }
            case 11203:
            {
                var data = (System.UInt32)dataRaw;
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
                var data = new System.Collections.Generic.List<(System.UInt32 taskId, System.UInt16 number, System.Byte isAward)>(dataLength);
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
                    var dataData = (taskId: dataDataTaskId, number: dataDataNumber, isAward: dataDataIsAward);
                    // add
                    data.Add(dataData);
                }
                return (protocol: 11201, data: data);
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
                var dataTask = (taskId: dataTaskTaskId, number: dataTaskNumber, isAward: dataTaskIsAward);
                // object
                var data = (result: dataResult, task: dataTask);
                return (protocol: 11202, data: data);
            }
            case 11203:
            {
                // 结果
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = encoding.GetString(reader.ReadBytes(dataLength));
                return (protocol: 11203, data: data);
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }
}