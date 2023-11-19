public static class CheatProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, dynamic data) 
    {
        switch (protocol) 
        {
            case 60001:
            {

                return;
            }
            case 60002:
            {
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
                var data = new System.Collections.Generic.List<System.Object>(dataLength);
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
                    var dataData = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"description", dataDataDescription}, {"command", dataDataCommand}};
                    // add
                    data.Add(dataData);
                }
                return data;
            }
            case 60002:
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