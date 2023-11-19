public class ChatGetSystemListRequest
{
    public System.UInt16 protocol = 11602;
    public System.UInt16 data;
}

public class ChatGetSystemListResponse
{
    public System.UInt16 protocol = 11602;
    public System.Collections.Generic.List<(
        System.UInt64 id,                                                       // ID
        System.UInt64 roleId,                                                   // 角色ID
        System.String roleName,                                                 // 角色名字
        System.Byte type,                                                       // 类型
        System.String message                                                   // 消息内容
    )> data;
}

public class ChatWorldRequest
{
    public System.UInt16 protocol = 11603;
    public (
        System.Byte type,                                                       // 类型
        System.String message                                                   // 消息
    ) data;
}

public class ChatWorldResponse
{
    public System.UInt16 protocol = 11603;
    public (
        System.String result,                                                   // 结果
        (
            System.UInt64 id,                                                   // ID
            System.UInt64 roleId,                                               // 角色ID
            System.String roleName,                                             // 角色名字
            System.Byte type,                                                   // 类型
            System.String message                                               // 消息内容
        ) worldChat                                                             // 
    ) data;
}

public class ChatGetWorldListRequest
{
    public System.UInt16 protocol = 11604;
    public System.UInt16 data;
}

public class ChatGetWorldListResponse
{
    public System.UInt16 protocol = 11604;
    public System.Collections.Generic.List<(
        System.UInt64 id,                                                       // ID
        System.UInt64 roleId,                                                   // 角色ID
        System.String roleName,                                                 // 角色名字
        System.Byte type,                                                       // 类型
        System.String message                                                   // 消息内容
    )> data;
}

public class ChatGuildRequest
{
    public System.UInt16 protocol = 11605;
    public (
        System.Byte type,                                                       // 类型
        System.String message                                                   // 消息
    ) data;
}

public class ChatGuildResponse
{
    public System.UInt16 protocol = 11605;
    public (
        System.String result,                                                   // 结果
        (
            System.UInt64 id,                                                   // ID
            System.UInt64 roleId,                                               // 角色ID
            System.String roleName,                                             // 角色名字
            System.Byte type,                                                   // 类型
            System.String message                                               // 消息内容
        ) guildChat                                                             // 
    ) data;
}

public class ChatGetGuildListRequest
{
    public System.UInt16 protocol = 11606;
    public System.UInt16 data;
}

public class ChatGetGuildListResponse
{
    public System.UInt16 protocol = 11606;
    public System.Collections.Generic.List<(
        System.UInt64 id,                                                       // ID
        System.UInt64 roleId,                                                   // 角色ID
        System.String roleName,                                                 // 角色名字
        System.Byte type,                                                       // 类型
        System.String message                                                   // 消息内容
    )> data;
}

public class ChatPrivateRequest
{
    public System.UInt16 protocol = 11607;
    public (
        System.UInt64 roleId,                                                   // 角色ID
        System.Byte type,                                                       // 类型
        System.String message                                                   // 消息
    ) data;
}

public class ChatPrivateResponse
{
    public System.UInt16 protocol = 11607;
    public (
        System.String result,                                                   // 结果
        (
            System.UInt64 senderId,                                             // 发送者角色ID
            System.UInt64 receiverId,                                           // 接收者角色ID
            System.Byte type,                                                   // 类型
            System.String message                                               // 消息内容
        ) privateChat                                                           // 
    ) data;
}

public class ChatGetPrivateListRequest
{
    public System.UInt16 protocol = 11608;
    public (
        System.UInt64 roleId,                                                   // 角色ID
        System.UInt16 page                                                      // 页
    ) data;
}

public class ChatGetPrivateListResponse
{
    public System.UInt16 protocol = 11608;
    public System.Collections.Generic.List<(
        System.UInt64 senderId,                                                 // 发送者角色ID
        System.UInt64 receiverId,                                               // 接收者角色ID
        System.Byte type,                                                       // 类型
        System.String message                                                   // 消息内容
    )> data;
}

public static class ChatProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, System.Object dataRaw) 
    {
        switch (protocol) 
        {
            case 11602:
            {
                var data = (System.UInt16)dataRaw;
                // 页
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)(System.UInt16)data));
                return;
            }
            case 11603:
            {
                var data = ((System.Byte type, System.String message))dataRaw;

                // 类型
                writer.Write((System.Byte)data.type);
                // 消息
                var dataMessageBytes = encoding.GetBytes((System.String)data.message);
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)dataMessageBytes.Length));
                writer.Write(dataMessageBytes);
                return;
            }
            case 11604:
            {
                var data = (System.UInt16)dataRaw;
                // 页
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)(System.UInt16)data));
                return;
            }
            case 11605:
            {
                var data = ((System.Byte type, System.String message))dataRaw;

                // 类型
                writer.Write((System.Byte)data.type);
                // 消息
                var dataMessageBytes = encoding.GetBytes((System.String)data.message);
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)dataMessageBytes.Length));
                writer.Write(dataMessageBytes);
                return;
            }
            case 11606:
            {
                var data = (System.UInt16)dataRaw;
                // 页
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)(System.UInt16)data));
                return;
            }
            case 11607:
            {
                var data = ((System.UInt64 roleId, System.Byte type, System.String message))dataRaw;

                // 角色ID
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)data.roleId));
                // 类型
                writer.Write((System.Byte)data.type);
                // 消息
                var dataMessageBytes = encoding.GetBytes((System.String)data.message);
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)dataMessageBytes.Length));
                writer.Write(dataMessageBytes);
                return;
            }
            case 11608:
            {
                var data = ((System.UInt64 roleId, System.UInt16 page))dataRaw;

                // 角色ID
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)data.roleId));
                // 页
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)(System.UInt16)data.page));
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
                var data = new System.Collections.Generic.List<(System.UInt64 id, System.UInt64 roleId, System.String roleName, System.Byte type, System.String message)>(dataLength);
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
                    var dataData = (id: dataDataId, roleId: dataDataRoleId, roleName: dataDataRoleName, type: dataDataType, message: dataDataMessage);
                    // add
                    data.Add(dataData);
                }
                return (protocol: 11602, data: data);
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
                var dataWorldChat = (id: dataWorldChatId, roleId: dataWorldChatRoleId, roleName: dataWorldChatRoleName, type: dataWorldChatType, message: dataWorldChatMessage);
                // object
                var data = (result: dataResult, worldChat: dataWorldChat);
                return (protocol: 11603, data: data);
            }
            case 11604:
            {
                // 
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<(System.UInt64 id, System.UInt64 roleId, System.String roleName, System.Byte type, System.String message)>(dataLength);
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
                    var dataData = (id: dataDataId, roleId: dataDataRoleId, roleName: dataDataRoleName, type: dataDataType, message: dataDataMessage);
                    // add
                    data.Add(dataData);
                }
                return (protocol: 11604, data: data);
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
                var dataGuildChat = (id: dataGuildChatId, roleId: dataGuildChatRoleId, roleName: dataGuildChatRoleName, type: dataGuildChatType, message: dataGuildChatMessage);
                // object
                var data = (result: dataResult, guildChat: dataGuildChat);
                return (protocol: 11605, data: data);
            }
            case 11606:
            {
                // 
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<(System.UInt64 id, System.UInt64 roleId, System.String roleName, System.Byte type, System.String message)>(dataLength);
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
                    var dataData = (id: dataDataId, roleId: dataDataRoleId, roleName: dataDataRoleName, type: dataDataType, message: dataDataMessage);
                    // add
                    data.Add(dataData);
                }
                return (protocol: 11606, data: data);
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
                var dataPrivateChat = (senderId: dataPrivateChatSenderId, receiverId: dataPrivateChatReceiverId, type: dataPrivateChatType, message: dataPrivateChatMessage);
                // object
                var data = (result: dataResult, privateChat: dataPrivateChat);
                return (protocol: 11607, data: data);
            }
            case 11608:
            {
                // 
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<(System.UInt64 senderId, System.UInt64 receiverId, System.Byte type, System.String message)>(dataLength);
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
                    var dataData = (senderId: dataDataSenderId, receiverId: dataDataReceiverId, type: dataDataType, message: dataDataMessage);
                    // add
                    data.Add(dataData);
                }
                return (protocol: 11608, data: data);
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }
}