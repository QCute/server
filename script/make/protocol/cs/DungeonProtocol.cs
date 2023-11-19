public class DungeonQueryRequest
{
    public System.UInt16 protocol = 17001;
    public Empty data;
}

public class DungeonQueryResponse
{
    public System.UInt16 protocol = 17001;
    public System.Collections.Generic.List<(
        System.UInt32 dungeonId,                                                // 副本Id
        System.UInt16 todayNumber,                                              // 今天次数
        System.UInt16 totalNumber                                               // 总次数
    )> data;
}

public class DungeonEnterRequest
{
    public System.UInt16 protocol = 17002;
    public System.UInt32 data;
}

public class DungeonEnterResponse
{
    public System.UInt16 protocol = 17002;
    public System.String data;
}



public class DungeonStartResponse
{
    public System.UInt16 protocol = 17003;
    public System.String data;
}



public class DungeonOverResponse
{
    public System.UInt16 protocol = 17004;
    public System.String data;
}

public class DungeonInspireRequest
{
    public System.UInt16 protocol = 17005;
    public Empty data;
}

public class DungeonInspireResponse
{
    public System.UInt16 protocol = 17005;
    public System.String data;
}

public static class DungeonProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, System.Object dataRaw) 
    {
        switch (protocol) 
        {
            case 17001:
            {
                #pragma warning disable
                var data = (Empty)dataRaw;
                #pragma warning restore

                return;
            }
            case 17002:
            {
                var data = (System.UInt32)dataRaw;
                // 副本Id
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int32)(System.UInt32)data));
                return;
            }
            case 17005:
            {
                #pragma warning disable
                var data = (Empty)dataRaw;
                #pragma warning restore

                return;
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }

    public static System.Object Decode(System.Text.Encoding encoding, System.IO.BinaryReader reader, System.UInt16 protocol) 
    {
        switch (protocol) 
        {
            case 17001:
            {
                // 
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<(System.UInt32 dungeonId, System.UInt16 todayNumber, System.UInt16 totalNumber)>(dataLength);
                while (dataLength-- > 0)
                {
                    // 
                    // 副本Id
                    var dataDataDungeonId = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // 今天次数
                    var dataDataTodayNumber = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // 总次数
                    var dataDataTotalNumber = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // object
                    var dataData = (dungeonId: dataDataDungeonId, todayNumber: dataDataTodayNumber, totalNumber: dataDataTotalNumber);
                    // add
                    data.Add(dataData);
                }
                return (protocol: 17001, data: data);
            }
            case 17002:
            {
                // 结果
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = encoding.GetString(reader.ReadBytes(dataLength));
                return (protocol: 17002, data: data);
            }
            case 17003:
            {
                // 结果
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = encoding.GetString(reader.ReadBytes(dataLength));
                return (protocol: 17003, data: data);
            }
            case 17004:
            {
                // 结果
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = encoding.GetString(reader.ReadBytes(dataLength));
                return (protocol: 17004, data: data);
            }
            case 17005:
            {
                // 结果
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = encoding.GetString(reader.ReadBytes(dataLength));
                return (protocol: 17005, data: data);
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }
}