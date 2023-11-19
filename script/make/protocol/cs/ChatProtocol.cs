public static class ChatProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, dynamic data) 
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

                // 类型
                writer.Write((System.Byte)data["type"]);
                // 消息
                var dataMessageBytes = encoding.GetBytes((System.String)data["message"]);
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)dataMessageBytes.Length));
                writer.Write(dataMessageBytes);
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

                // 类型
                writer.Write((System.Byte)data["type"]);
                // 消息
                var dataMessageBytes = encoding.GetBytes((System.String)data["message"]);
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)dataMessageBytes.Length));
                writer.Write(dataMessageBytes);
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

                // 角色ID
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)data["roleId"]));
                // 类型
                writer.Write((System.Byte)data["type"]);
                // 消息
                var dataMessageBytes = encoding.GetBytes((System.String)data["message"]);
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)dataMessageBytes.Length));
                writer.Write(dataMessageBytes);
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
                    var dataDataId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 角色ID
                    var dataDataRoleId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 角色名字
                    var dataDataRoleNameLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var dataDataRoleName = encoding.GetString(reader.ReadBytes(dataDataRoleNameLength));
                    // 类型
                    var dataDataType = reader.ReadByte();
                    // 消息内容
                    var dataDataMessageLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var dataDataMessage = encoding.GetString(reader.ReadBytes(dataDataMessageLength));
                    // object
                    var dataData = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"id", dataDataId}, {"roleId", dataDataRoleId}, {"roleName", dataDataRoleName}, {"type", dataDataType}, {"message", dataDataMessage}};
                    // add
                    data.Add(dataData);
                }
                return data;
            }
            case 11603:
            {
                // 
                // 结果
                var dataResultLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var dataResult = encoding.GetString(reader.ReadBytes(dataResultLength));
                // 
                // ID
                var dataWorldChatId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // 角色ID
                var dataWorldChatRoleId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // 角色名字
                var dataWorldChatRoleNameLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var dataWorldChatRoleName = encoding.GetString(reader.ReadBytes(dataWorldChatRoleNameLength));
                // 类型
                var dataWorldChatType = reader.ReadByte();
                // 消息内容
                var dataWorldChatMessageLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var dataWorldChatMessage = encoding.GetString(reader.ReadBytes(dataWorldChatMessageLength));
                // object
                var dataWorldChat = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"id", dataWorldChatId}, {"roleId", dataWorldChatRoleId}, {"roleName", dataWorldChatRoleName}, {"type", dataWorldChatType}, {"message", dataWorldChatMessage}};
                // object
                var data = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"result", dataResult}, {"worldChat", dataWorldChat}};
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
                    var dataDataId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 角色ID
                    var dataDataRoleId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 角色名字
                    var dataDataRoleNameLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var dataDataRoleName = encoding.GetString(reader.ReadBytes(dataDataRoleNameLength));
                    // 类型
                    var dataDataType = reader.ReadByte();
                    // 消息内容
                    var dataDataMessageLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var dataDataMessage = encoding.GetString(reader.ReadBytes(dataDataMessageLength));
                    // object
                    var dataData = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"id", dataDataId}, {"roleId", dataDataRoleId}, {"roleName", dataDataRoleName}, {"type", dataDataType}, {"message", dataDataMessage}};
                    // add
                    data.Add(dataData);
                }
                return data;
            }
            case 11605:
            {
                // 
                // 结果
                var dataResultLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var dataResult = encoding.GetString(reader.ReadBytes(dataResultLength));
                // 
                // ID
                var dataGuildChatId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // 角色ID
                var dataGuildChatRoleId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // 角色名字
                var dataGuildChatRoleNameLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var dataGuildChatRoleName = encoding.GetString(reader.ReadBytes(dataGuildChatRoleNameLength));
                // 类型
                var dataGuildChatType = reader.ReadByte();
                // 消息内容
                var dataGuildChatMessageLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var dataGuildChatMessage = encoding.GetString(reader.ReadBytes(dataGuildChatMessageLength));
                // object
                var dataGuildChat = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"id", dataGuildChatId}, {"roleId", dataGuildChatRoleId}, {"roleName", dataGuildChatRoleName}, {"type", dataGuildChatType}, {"message", dataGuildChatMessage}};
                // object
                var data = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"result", dataResult}, {"guildChat", dataGuildChat}};
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
                    var dataDataId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 角色ID
                    var dataDataRoleId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 角色名字
                    var dataDataRoleNameLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var dataDataRoleName = encoding.GetString(reader.ReadBytes(dataDataRoleNameLength));
                    // 类型
                    var dataDataType = reader.ReadByte();
                    // 消息内容
                    var dataDataMessageLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var dataDataMessage = encoding.GetString(reader.ReadBytes(dataDataMessageLength));
                    // object
                    var dataData = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"id", dataDataId}, {"roleId", dataDataRoleId}, {"roleName", dataDataRoleName}, {"type", dataDataType}, {"message", dataDataMessage}};
                    // add
                    data.Add(dataData);
                }
                return data;
            }
            case 11607:
            {
                // 
                // 结果
                var dataResultLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var dataResult = encoding.GetString(reader.ReadBytes(dataResultLength));
                // 
                // 发送者角色ID
                var dataPrivateChatSenderId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // 接收者角色ID
                var dataPrivateChatReceiverId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // 类型
                var dataPrivateChatType = reader.ReadByte();
                // 消息内容
                var dataPrivateChatMessageLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var dataPrivateChatMessage = encoding.GetString(reader.ReadBytes(dataPrivateChatMessageLength));
                // object
                var dataPrivateChat = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"senderId", dataPrivateChatSenderId}, {"receiverId", dataPrivateChatReceiverId}, {"type", dataPrivateChatType}, {"message", dataPrivateChatMessage}};
                // object
                var data = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"result", dataResult}, {"privateChat", dataPrivateChat}};
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
                    var dataDataSenderId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 接收者角色ID
                    var dataDataReceiverId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 类型
                    var dataDataType = reader.ReadByte();
                    // 消息内容
                    var dataDataMessageLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var dataDataMessage = encoding.GetString(reader.ReadBytes(dataDataMessageLength));
                    // object
                    var dataData = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"senderId", dataDataSenderId}, {"receiverId", dataDataReceiverId}, {"type", dataDataType}, {"message", dataDataMessage}};
                    // add
                    data.Add(dataData);
                }
                return data;
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }
}