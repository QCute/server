public static class ChatProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, System.Collections.Generic.Dictionary<System.String, System.Object> data) 
    {
        switch (protocol) 
        {
            case 11602:
            {
                // 页
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)(System.UInt16)data["page"]));
                return;
            }
            case 11603:
            {
                // 类型
                writer.Write((System.Byte)data["type"]);
                // 消息
                var messageBytes = encoding.GetBytes((System.String)data["message"]);
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)messageBytes.Length));
                writer.Write(messageBytes);
                return;
            }
            case 11604:
            {
                // 页
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)(System.UInt16)data["page"]));
                return;
            }
            case 11605:
            {
                // 类型
                writer.Write((System.Byte)data["type"]);
                // 消息
                var messageBytes = encoding.GetBytes((System.String)data["message"]);
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)messageBytes.Length));
                writer.Write(messageBytes);
                return;
            }
            case 11606:
            {
                // 页
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)(System.UInt16)data["page"]));
                return;
            }
            case 11607:
            {
                // 角色ID
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)data["roleId"]));
                // 类型
                writer.Write((System.Byte)data["type"]);
                // 消息
                var messageBytes = encoding.GetBytes((System.String)data["message"]);
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)messageBytes.Length));
                writer.Write(messageBytes);
                return;
            }
            case 11608:
            {
                // 角色ID
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)data["roleId"]));
                // 页
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)(System.UInt16)data["page"]));
                return;
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }

    public static System.Collections.Generic.Dictionary<System.String, System.Object> Decode(System.Text.Encoding encoding, System.IO.BinaryReader reader, System.UInt16 protocol) 
    {
        switch (protocol) 
        {
            case 11602:
            {
                // 
                var listLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var list = new System.Collections.Generic.List<System.Object>(listLength);
                while (listLength-- > 0)
                {
                    // SystemChat
                    // ID
                    var id = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 角色ID
                    var roleId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 角色名字
                    var roleNameLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var roleName = encoding.GetString(reader.ReadBytes(roleNameLength));
                    // 类型
                    var type = reader.ReadByte();
                    // 消息内容
                    var messageLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var message = encoding.GetString(reader.ReadBytes(messageLength));
                    // object
                    var systemChat = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"id", id}, {"roleId", roleId}, {"roleName", roleName}, {"type", type}, {"message", message}};
                    // add
                    list.Add(systemChat);
                }
                return new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"list", list}};
            }
            case 11603:
            {
                // 结果
                var resultLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var result = encoding.GetString(reader.ReadBytes(resultLength));
                // WorldChat
                // ID
                var id = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // 角色ID
                var roleId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // 角色名字
                var roleNameLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var roleName = encoding.GetString(reader.ReadBytes(roleNameLength));
                // 类型
                var type = reader.ReadByte();
                // 消息内容
                var messageLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var message = encoding.GetString(reader.ReadBytes(messageLength));
                // object
                var worldChat = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"id", id}, {"roleId", roleId}, {"roleName", roleName}, {"type", type}, {"message", message}};
                return new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"result", result}, {"worldChat", worldChat}};
            }
            case 11604:
            {
                // 
                var listLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var list = new System.Collections.Generic.List<System.Object>(listLength);
                while (listLength-- > 0)
                {
                    // WorldChat
                    // ID
                    var id = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 角色ID
                    var roleId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 角色名字
                    var roleNameLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var roleName = encoding.GetString(reader.ReadBytes(roleNameLength));
                    // 类型
                    var type = reader.ReadByte();
                    // 消息内容
                    var messageLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var message = encoding.GetString(reader.ReadBytes(messageLength));
                    // object
                    var worldChat = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"id", id}, {"roleId", roleId}, {"roleName", roleName}, {"type", type}, {"message", message}};
                    // add
                    list.Add(worldChat);
                }
                return new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"list", list}};
            }
            case 11605:
            {
                // 结果
                var resultLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var result = encoding.GetString(reader.ReadBytes(resultLength));
                // GuildChat
                // ID
                var id = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // 角色ID
                var roleId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // 角色名字
                var roleNameLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var roleName = encoding.GetString(reader.ReadBytes(roleNameLength));
                // 类型
                var type = reader.ReadByte();
                // 消息内容
                var messageLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var message = encoding.GetString(reader.ReadBytes(messageLength));
                // object
                var guildChat = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"id", id}, {"roleId", roleId}, {"roleName", roleName}, {"type", type}, {"message", message}};
                return new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"result", result}, {"guildChat", guildChat}};
            }
            case 11606:
            {
                // 
                var listLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var list = new System.Collections.Generic.List<System.Object>(listLength);
                while (listLength-- > 0)
                {
                    // GuildChat
                    // ID
                    var id = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 角色ID
                    var roleId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 角色名字
                    var roleNameLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var roleName = encoding.GetString(reader.ReadBytes(roleNameLength));
                    // 类型
                    var type = reader.ReadByte();
                    // 消息内容
                    var messageLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var message = encoding.GetString(reader.ReadBytes(messageLength));
                    // object
                    var guildChat = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"id", id}, {"roleId", roleId}, {"roleName", roleName}, {"type", type}, {"message", message}};
                    // add
                    list.Add(guildChat);
                }
                return new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"list", list}};
            }
            case 11607:
            {
                // 结果
                var resultLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var result = encoding.GetString(reader.ReadBytes(resultLength));
                // PrivateChat
                // 发送者角色ID
                var senderId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // 接收者角色ID
                var receiverId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // 类型
                var type = reader.ReadByte();
                // 消息内容
                var messageLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var message = encoding.GetString(reader.ReadBytes(messageLength));
                // object
                var privateChat = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"senderId", senderId}, {"receiverId", receiverId}, {"type", type}, {"message", message}};
                return new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"result", result}, {"privateChat", privateChat}};
            }
            case 11608:
            {
                // 
                var listLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var list = new System.Collections.Generic.List<System.Object>(listLength);
                while (listLength-- > 0)
                {
                    // PrivateChat
                    // 发送者角色ID
                    var senderId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 接收者角色ID
                    var receiverId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 类型
                    var type = reader.ReadByte();
                    // 消息内容
                    var messageLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var message = encoding.GetString(reader.ReadBytes(messageLength));
                    // object
                    var privateChat = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"senderId", senderId}, {"receiverId", receiverId}, {"type", type}, {"message", message}};
                    // add
                    list.Add(privateChat);
                }
                return new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"list", list}};
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }
}