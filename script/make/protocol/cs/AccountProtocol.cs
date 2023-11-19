public static class AccountProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, dynamic data) 
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
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)(System.UInt16)data["serverId"]));
                // 账户名
                var dataAccountNameBytes = encoding.GetBytes((System.String)data["accountName"]);
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)dataAccountNameBytes.Length));
                writer.Write(dataAccountNameBytes);
                return;
            }
            case 10002:
            {

                // 角色名
                var dataRoleNameBytes = encoding.GetBytes((System.String)data["roleName"]);
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)dataRoleNameBytes.Length));
                writer.Write(dataRoleNameBytes);
                // 服务器ID
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)(System.UInt16)data["serverId"]));
                // 账户名
                var dataAccountNameBytes = encoding.GetBytes((System.String)data["accountName"]);
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)dataAccountNameBytes.Length));
                writer.Write(dataAccountNameBytes);
                // 性别
                writer.Write((System.Byte)data["sex"]);
                // 职业
                writer.Write((System.Byte)data["classes"]);
                // 渠道
                var dataChannelBytes = encoding.GetBytes((System.String)data["channel"]);
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)dataChannelBytes.Length));
                writer.Write(dataChannelBytes);
                // 设备
                var dataDeviceIdBytes = encoding.GetBytes((System.String)data["deviceId"]);
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)dataDeviceIdBytes.Length));
                writer.Write(dataDeviceIdBytes);
                // mac地址
                var dataMacBytes = encoding.GetBytes((System.String)data["mac"]);
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)dataMacBytes.Length));
                writer.Write(dataMacBytes);
                // 设备类型
                var dataDeviceTypeBytes = encoding.GetBytes((System.String)data["deviceType"]);
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)dataDeviceTypeBytes.Length));
                writer.Write(dataDeviceTypeBytes);
                return;
            }
            case 10003:
            {

                // 角色ID
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)data["roleId"]));
                // 角色名
                var dataRoleNameBytes = encoding.GetBytes((System.String)data["roleName"]);
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)dataRoleNameBytes.Length));
                writer.Write(dataRoleNameBytes);
                // 服务器ID
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)(System.UInt16)data["serverId"]));
                // 账户名
                var dataAccountNameBytes = encoding.GetBytes((System.String)data["accountName"]);
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)dataAccountNameBytes.Length));
                writer.Write(dataAccountNameBytes);
                return;
            }
            case 10004:
            {

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
                return data;
            }
            case 10001:
            {
                // 
                // 结果
                var dataResultLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var dataResult = encoding.GetString(reader.ReadBytes(dataResultLength));
                // 角色名列表
                var dataListLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var dataList = new System.Collections.Generic.List<System.Object>(dataListLength);
                while (dataListLength-- > 0)
                {
                    // 
                    // 角色ID
                    var dataListDataRoleId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 角色名
                    var dataListDataRoleNameLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var dataListDataRoleName = encoding.GetString(reader.ReadBytes(dataListDataRoleNameLength));
                    // object
                    var dataListData = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"roleId", dataListDataRoleId}, {"roleName", dataListDataRoleName}};
                    // add
                    dataList.Add(dataListData);
                }
                // object
                var data = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"result", dataResult}, {"list", dataList}};
                return data;
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
                var data = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"result", dataResult}, {"roleId", dataRoleId}, {"roleName", dataRoleName}};
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