public static class CheatProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, System.Collections.Generic.Dictionary<System.String, System.Object> data) 
    {
        switch (protocol) 
        {
            case 60002:
            {
                // 命令
                var commandBytes = encoding.GetBytes((System.String)data["command"]);
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)commandBytes.Length));
                writer.Write(commandBytes);
                return;
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }

    public static System.Collections.Generic.Dictionary<System.String, System.Object> Decode(System.Text.Encoding encoding, System.IO.BinaryReader reader, System.UInt16 protocol) 
    {
        switch (protocol) 
        {
            case 60001:
            {
                // 秘籍列表
                var cheatListLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var cheatList = new System.Collections.ArrayList(cheatListLength);
                while (cheatListLength-- > 0)
                {
                    // 描述
                    var descriptionLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var description = encoding.GetString(reader.ReadBytes(descriptionLength));
                    // 命令
                    var commandLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var command = encoding.GetString(reader.ReadBytes(commandLength));
                    // add
                    cheatList.Add(new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"description", description}, {"command", command}});
                }
                return new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"cheatList", cheatList}};
            }
            case 60002:
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