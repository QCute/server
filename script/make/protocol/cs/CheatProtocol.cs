public class CheatQueryRequest
{
    public System.UInt16 protocol = 60001;
    public Empty data;
}

public class CheatQueryResponse
{
    public System.UInt16 protocol = 60001;
    public System.Collections.Generic.List<(
        System.String description,                                              // 描述
        System.String command                                                   // 命令
    )> data;
}

public class CheatCheatRequest
{
    public System.UInt16 protocol = 60002;
    public System.String data;
}

public class CheatCheatResponse
{
    public System.UInt16 protocol = 60002;
    public System.String data;
}

public static class CheatProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, System.Object dataRaw) 
    {
        switch (protocol) 
        {
            case 60001:
            {
                #pragma warning disable
                var data = (Empty)dataRaw;
                #pragma warning restore

                return;
            }
            case 60002:
            {
                var data = (System.String)dataRaw;
                // 命令
                var dataBytes = encoding.GetBytes((System.String)data);
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)dataBytes.Length));
                writer.Write(dataBytes);
                return;
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }

    public static System.Object Decode(System.Text.Encoding encoding, System.IO.BinaryReader reader, System.UInt16 protocol) 
    {
        switch (protocol) 
        {
            case 60001:
            {
                // 命令列表
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<(System.String description, System.String command)>(dataLength);
                while (dataLength-- > 0)
                {
                    // 
                    // 描述
                    var dataDataDescriptionLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var dataDataDescription = encoding.GetString(reader.ReadBytes(dataDataDescriptionLength));
                    // 命令
                    var dataDataCommandLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var dataDataCommand = encoding.GetString(reader.ReadBytes(dataDataCommandLength));
                    // object
                    var dataData = (description: dataDataDescription, command: dataDataCommand);
                    // add
                    data.Add(dataData);
                }
                return (protocol: 60001, data: data);
            }
            case 60002:
            {
                // 结果
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = encoding.GetString(reader.ReadBytes(dataLength));
                return (protocol: 60002, data: data);
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }
}