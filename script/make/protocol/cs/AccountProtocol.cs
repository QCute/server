public class AccountHeartbeatRequest
{
    public System.UInt16 protocol = 10000;
    public Empty data;
}

public class AccountHeartbeatResponse
{
    public System.UInt16 protocol = 10000;
    public System.String data;
}

public class AccountQueryRequest
{
    public System.UInt16 protocol = 10001;
    public (
        System.UInt16 serverId,                                                 // 服务器ID
        System.String accountName                                               // 账户名
    ) data;
}

public class AccountQueryResponse
{
    public System.UInt16 protocol = 10001;
    public (
        System.String result,                                                   // 结果
        System.Collections.Generic.List<(
            System.UInt64 roleId,                                               // 角色ID
            System.String roleName                                              // 角色名
        )> list                                                                 // 角色名列表
    ) data;
}

public class AccountCreateRequest
{
    public System.UInt16 protocol = 10002;
    public (
        System.String roleName,                                                 // 角色名
        System.UInt16 serverId,                                                 // 服务器ID
        System.String accountName,                                              // 账户名
        System.Byte sex,                                                        // 性别
        System.Byte classes,                                                    // 职业
        System.String channel,                                                  // 渠道
        System.String deviceId,                                                 // 设备
        System.String mac,                                                      // mac地址
        System.String deviceType                                                // 设备类型
    ) data;
}

public class AccountCreateResponse
{
    public System.UInt16 protocol = 10002;
    public (
        System.String result,                                                   // 结果
        System.UInt64 roleId,                                                   // 角色ID
        System.String roleName                                                  // 角色名
    ) data;
}

public class AccountLoginRequest
{
    public System.UInt16 protocol = 10003;
    public (
        System.UInt64 roleId,                                                   // 角色ID
        System.String roleName,                                                 // 角色名
        System.UInt16 serverId,                                                 // 服务器ID
        System.String accountName                                               // 账户名
    ) data;
}

public class AccountLoginResponse
{
    public System.UInt16 protocol = 10003;
    public System.String data;
}

public class AccountLogoutRequest
{
    public System.UInt16 protocol = 10004;
    public Empty data;
}

public class AccountLogoutResponse
{
    public System.UInt16 protocol = 10004;
    public System.String data;
}

public static class AccountProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, System.Object dataRaw) 
    {
        switch (protocol) 
        {
            case 10000:
            {
                #pragma warning disable
                var data = (Empty)dataRaw;
                #pragma warning restore

                return;
            }
            case 10001:
            {
                var data = ((System.UInt16 serverId, System.String accountName))dataRaw;

                // 服务器ID
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)(System.UInt16)data.serverId));
                // 账户名
                var dataAccountNameBytes = encoding.GetBytes((System.String)data.accountName);
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)dataAccountNameBytes.Length));
                writer.Write(dataAccountNameBytes);
                return;
            }
            case 10002:
            {
                var data = ((System.String roleName, System.UInt16 serverId, System.String accountName, System.Byte sex, System.Byte classes, System.String channel, System.String deviceId, System.String mac, System.String deviceType))dataRaw;

                // 角色名
                var dataRoleNameBytes = encoding.GetBytes((System.String)data.roleName);
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)dataRoleNameBytes.Length));
                writer.Write(dataRoleNameBytes);
                // 服务器ID
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)(System.UInt16)data.serverId));
                // 账户名
                var dataAccountNameBytes = encoding.GetBytes((System.String)data.accountName);
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)dataAccountNameBytes.Length));
                writer.Write(dataAccountNameBytes);
                // 性别
                writer.Write((System.Byte)data.sex);
                // 职业
                writer.Write((System.Byte)data.classes);
                // 渠道
                var dataChannelBytes = encoding.GetBytes((System.String)data.channel);
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)dataChannelBytes.Length));
                writer.Write(dataChannelBytes);
                // 设备
                var dataDeviceIdBytes = encoding.GetBytes((System.String)data.deviceId);
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)dataDeviceIdBytes.Length));
                writer.Write(dataDeviceIdBytes);
                // mac地址
                var dataMacBytes = encoding.GetBytes((System.String)data.mac);
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)dataMacBytes.Length));
                writer.Write(dataMacBytes);
                // 设备类型
                var dataDeviceTypeBytes = encoding.GetBytes((System.String)data.deviceType);
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)dataDeviceTypeBytes.Length));
                writer.Write(dataDeviceTypeBytes);
                return;
            }
            case 10003:
            {
                var data = ((System.UInt64 roleId, System.String roleName, System.UInt16 serverId, System.String accountName))dataRaw;

                // 角色ID
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)data.roleId));
                // 角色名
                var dataRoleNameBytes = encoding.GetBytes((System.String)data.roleName);
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)dataRoleNameBytes.Length));
                writer.Write(dataRoleNameBytes);
                // 服务器ID
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)(System.UInt16)data.serverId));
                // 账户名
                var dataAccountNameBytes = encoding.GetBytes((System.String)data.accountName);
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)dataAccountNameBytes.Length));
                writer.Write(dataAccountNameBytes);
                return;
            }
            case 10004:
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
            case 10000:
            {
                // 结果
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = encoding.GetString(reader.ReadBytes(dataLength));
                return (protocol: 10000, data: data);
            }
            case 10001:
            {
                // 
                // 结果
                var dataResultLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var dataResult = encoding.GetString(reader.ReadBytes(dataResultLength));
                // 角色名列表
                var dataListLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var dataList = new System.Collections.Generic.List<(System.UInt64 roleId, System.String roleName)>(dataListLength);
                while (dataListLength-- > 0)
                {
                    // 
                    // 角色ID
                    var dataListDataRoleId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 角色名
                    var dataListDataRoleNameLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var dataListDataRoleName = encoding.GetString(reader.ReadBytes(dataListDataRoleNameLength));
                    // object
                    var dataListData = (roleId: dataListDataRoleId, roleName: dataListDataRoleName);
                    // add
                    dataList.Add(dataListData);
                }
                // object
                var data = (result: dataResult, list: dataList);
                return (protocol: 10001, data: data);
            }
            case 10002:
            {
                // 
                // 结果
                var dataResultLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var dataResult = encoding.GetString(reader.ReadBytes(dataResultLength));
                // 角色ID
                var dataRoleId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // 角色名
                var dataRoleNameLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var dataRoleName = encoding.GetString(reader.ReadBytes(dataRoleNameLength));
                // object
                var data = (result: dataResult, roleId: dataRoleId, roleName: dataRoleName);
                return (protocol: 10002, data: data);
            }
            case 10003:
            {
                // 结果
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = encoding.GetString(reader.ReadBytes(dataLength));
                return (protocol: 10003, data: data);
            }
            case 10004:
            {
                // 结果
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = encoding.GetString(reader.ReadBytes(dataLength));
                return (protocol: 10004, data: data);
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }
}