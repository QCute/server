public static class ChatProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, System.Object data) 
    {
        switch (protocol) 
        {
            case 11602:
            {
                // 页
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)(System.UInt16)data));
                return;
            }
            case 11603:
            {
                // convert
                var dataCast = (System.Collections.Generic.Dictionary<System.String, System.Object>)data;
                // 类型
                writer.Write((System.Byte)dataCast["type"]);
                // 消息
                var messageBytes = encoding.GetBytes((System.String)dataCast["message"]);
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)messageBytes.Length));
                writer.Write(messageBytes);
                return;
            }
            case 11604:
            {
                // 页
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)(System.UInt16)data));
                return;
            }
            case 11605:
            {
                // convert
                var dataCast = (System.Collections.Generic.Dictionary<System.String, System.Object>)data;
                // 类型
                writer.Write((System.Byte)dataCast["type"]);
                // 消息
                var messageBytes = encoding.GetBytes((System.String)dataCast["message"]);
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)messageBytes.Length));
                writer.Write(messageBytes);
                return;
            }
            case 11606:
            {
                // 页
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)(System.UInt16)data));
                return;
            }
            case 11607:
            {
                // convert
                var dataCast = (System.Collections.Generic.Dictionary<System.String, System.Object>)data;
                // 角色ID
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)dataCast["roleId"]));
                // 类型
                writer.Write((System.Byte)dataCast["type"]);
                // 消息
                var messageBytes = encoding.GetBytes((System.String)dataCast["message"]);
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)messageBytes.Length));
                writer.Write(messageBytes);
                return;
            }
            case 11608:
            {
                // convert
                var dataCast = (System.Collections.Generic.Dictionary<System.String, System.Object>)data;
                // 角色ID
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)dataCast["roleId"]));
                // 页
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)(System.UInt16)dataCast["page"]));
                return;
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }

    public static System.Object Decode(System.Text.Encoding encoding, System.IO.BinaryReader reader, System.UInt16 protocol) 
    {
        switch (protocol) 
        {
            case 11602:
            {
                // 
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<System.Object>(dataLength);
                while (dataLength-- > 0)
                {
                    // 
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
                    data.Add(systemChat);
                }
                return data;
            }
            case 11603:
            {
                // 
                // 结果
                var resultLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var result = encoding.GetString(reader.ReadBytes(resultLength));
                // 
                // ID
                var worldChatId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // 角色ID
                var worldChatRoleId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // 角色名字
                var worldChatRoleNameLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var worldChatRoleName = encoding.GetString(reader.ReadBytes(worldChatRoleNameLength));
                // 类型
                var worldChatType = reader.ReadByte();
                // 消息内容
                var worldChatMessageLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var worldChatMessage = encoding.GetString(reader.ReadBytes(worldChatMessageLength));
                // object
                var worldChat = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"id", worldChatId}, {"roleId", worldChatRoleId}, {"roleName", worldChatRoleName}, {"type", worldChatType}, {"message", worldChatMessage}};
                // object
                var data = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"result", result}, {"worldChat", worldChat}};
                return data;
            }
            case 11604:
            {
                // 
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<System.Object>(dataLength);
                while (dataLength-- > 0)
                {
                    // 
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
                    data.Add(worldChat);
                }
                return data;
            }
            case 11605:
            {
                // 
                // 结果
                var resultLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var result = encoding.GetString(reader.ReadBytes(resultLength));
                // 
                // ID
                var guildChatId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // 角色ID
                var guildChatRoleId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // 角色名字
                var guildChatRoleNameLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var guildChatRoleName = encoding.GetString(reader.ReadBytes(guildChatRoleNameLength));
                // 类型
                var guildChatType = reader.ReadByte();
                // 消息内容
                var guildChatMessageLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var guildChatMessage = encoding.GetString(reader.ReadBytes(guildChatMessageLength));
                // object
                var guildChat = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"id", guildChatId}, {"roleId", guildChatRoleId}, {"roleName", guildChatRoleName}, {"type", guildChatType}, {"message", guildChatMessage}};
                // object
                var data = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"result", result}, {"guildChat", guildChat}};
                return data;
            }
            case 11606:
            {
                // 
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<System.Object>(dataLength);
                while (dataLength-- > 0)
                {
                    // 
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
                    data.Add(guildChat);
                }
                return data;
            }
            case 11607:
            {
                // 
                // 结果
                var resultLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var result = encoding.GetString(reader.ReadBytes(resultLength));
                // 
                // 发送者角色ID
                var privateChatSenderId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // 接收者角色ID
                var privateChatReceiverId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // 类型
                var privateChatType = reader.ReadByte();
                // 消息内容
                var privateChatMessageLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var privateChatMessage = encoding.GetString(reader.ReadBytes(privateChatMessageLength));
                // object
                var privateChat = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"senderId", privateChatSenderId}, {"receiverId", privateChatReceiverId}, {"type", privateChatType}, {"message", privateChatMessage}};
                // object
                var data = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"result", result}, {"privateChat", privateChat}};
                return data;
            }
            case 11608:
            {
                // 
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<System.Object>(dataLength);
                while (dataLength-- > 0)
                {
                    // 
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
                    data.Add(privateChat);
                }
                return data;
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }
}