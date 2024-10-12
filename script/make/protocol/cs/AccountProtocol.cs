public static class AccountProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, System.Collections.Generic.Dictionary<System.String, System.Object> data) 
    {
        switch (protocol) 
        {
            case 10000:
            {
                return;
            }
            case 10001:
            {
                // 服务器ID
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)(System.UInt16)((System.Collections.Generic.Dictionary<System.String, System.Object>)data["data"])["serverId"]));
                // 账户名
                var accountNameBytes = encoding.GetBytes((System.String)((System.Collections.Generic.Dictionary<System.String, System.Object>)data["data"])["accountName"]);
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)accountNameBytes.Length));
                writer.Write(accountNameBytes);
                return;
            }
            case 10002:
            {
                // 角色名
                var roleNameBytes = encoding.GetBytes((System.String)((System.Collections.Generic.Dictionary<System.String, System.Object>)data["data"])["roleName"]);
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)roleNameBytes.Length));
                writer.Write(roleNameBytes);
                // 服务器ID
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)(System.UInt16)((System.Collections.Generic.Dictionary<System.String, System.Object>)data["data"])["serverId"]));
                // 账户名
                var accountNameBytes = encoding.GetBytes((System.String)((System.Collections.Generic.Dictionary<System.String, System.Object>)data["data"])["accountName"]);
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)accountNameBytes.Length));
                writer.Write(accountNameBytes);
                // 性别
                writer.Write((System.Byte)((System.Collections.Generic.Dictionary<System.String, System.Object>)data["data"])["sex"]);
                // 职业
                writer.Write((System.Byte)((System.Collections.Generic.Dictionary<System.String, System.Object>)data["data"])["classes"]);
                // 渠道
                var channelBytes = encoding.GetBytes((System.String)((System.Collections.Generic.Dictionary<System.String, System.Object>)data["data"])["channel"]);
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)channelBytes.Length));
                writer.Write(channelBytes);
                // 设备
                var deviceIdBytes = encoding.GetBytes((System.String)((System.Collections.Generic.Dictionary<System.String, System.Object>)data["data"])["deviceId"]);
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)deviceIdBytes.Length));
                writer.Write(deviceIdBytes);
                // mac地址
                var macBytes = encoding.GetBytes((System.String)((System.Collections.Generic.Dictionary<System.String, System.Object>)data["data"])["mac"]);
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)macBytes.Length));
                writer.Write(macBytes);
                // 设备类型
                var deviceTypeBytes = encoding.GetBytes((System.String)((System.Collections.Generic.Dictionary<System.String, System.Object>)data["data"])["deviceType"]);
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)deviceTypeBytes.Length));
                writer.Write(deviceTypeBytes);
                return;
            }
            case 10003:
            {
                // 角色ID
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)((System.Collections.Generic.Dictionary<System.String, System.Object>)data["data"])["roleId"]));
                // 角色名
                var roleNameBytes = encoding.GetBytes((System.String)((System.Collections.Generic.Dictionary<System.String, System.Object>)data["data"])["roleName"]);
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)roleNameBytes.Length));
                writer.Write(roleNameBytes);
                // 服务器ID
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)(System.UInt16)((System.Collections.Generic.Dictionary<System.String, System.Object>)data["data"])["serverId"]));
                // 账户名
                var accountNameBytes = encoding.GetBytes((System.String)((System.Collections.Generic.Dictionary<System.String, System.Object>)data["data"])["accountName"]);
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)accountNameBytes.Length));
                writer.Write(accountNameBytes);
                return;
            }
            case 10004:
            {
                return;
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }

    public static System.Collections.Generic.Dictionary<System.String, System.Object> Decode(System.Text.Encoding encoding, System.IO.BinaryReader reader, System.UInt16 protocol) 
    {
        switch (protocol) 
        {
            case 10000:
            {
                // 结果
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = encoding.GetString(reader.ReadBytes(dataLength));
                return data;
            }
            case 10001:
            {
                // 
                // 结果
                var resultLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var result = encoding.GetString(reader.ReadBytes(resultLength));
                // 角色名列表
                var listLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var list = new System.Collections.Generic.List<System.Object>(listLength);
                while (listLength-- > 0)
                {
                    // 
                    // 角色ID
                    var roleId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 角色名
                    var roleNameLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var roleName = encoding.GetString(reader.ReadBytes(roleNameLength));
                    // object
                    var listItem = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"roleId", roleId}, {"roleName", roleName}};
                    // add
                    list.Add(listItem);
                }
                // object
                var data = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"result", result}, {"list", list}};
                return data;
            }
            case 10002:
            {
                // 
                // 结果
                var resultLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var result = encoding.GetString(reader.ReadBytes(resultLength));
                // 角色ID
                var roleId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // 角色名
                var roleNameLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var roleName = encoding.GetString(reader.ReadBytes(roleNameLength));
                // object
                var data = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"result", result}, {"roleId", roleId}, {"roleName", roleName}};
                return data;
            }
            case 10003:
            {
                // 结果
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = encoding.GetString(reader.ReadBytes(dataLength));
                return data;
            }
            case 10004:
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