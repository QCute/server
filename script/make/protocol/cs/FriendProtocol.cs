public static class FriendProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, dynamic data) 
    {
        switch (protocol) 
        {
            case 11501:
            {

                return;
            }
            case 11502:
            {
                // 好友角色ID
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)data));
                return;
            }
            case 11503:
            {
                // 好友角色ID
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)data));
                return;
            }
            case 11504:
            {
                // 好友角色ID
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)data));
                return;
            }
            case 11505:
            {
                // 好友角色ID
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)data));
                return;
            }
            case 11506:
            {
                // 好友角色ID
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)data));
                return;
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }

    public static System.Object Decode(System.Text.Encoding encoding, System.IO.BinaryReader reader, System.UInt16 protocol) 
    {
        switch (protocol) 
        {
            case 11501:
            {
                // 
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<System.Object>(dataLength);
                while (dataLength-- > 0)
                {
                    // 
                    // 好友角色ID
                    var dataDataFriendRoleId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 好友名字
                    var dataDataFriendNameLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var dataDataFriendName = encoding.GetString(reader.ReadBytes(dataDataFriendNameLength));
                    // 关系状态(申请:1/好友:2/黑名单:3)
                    var dataDataRelation = reader.ReadByte();
                    // 添加/修改状态时间
                    var dataDataTime = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // object
                    var dataData = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"friendRoleId", dataDataFriendRoleId}, {"friendName", dataDataFriendName}, {"relation", dataDataRelation}, {"time", dataDataTime}};
                    // add
                    data.Add(dataData);
                }
                return data;
            }
            case 11502:
            {
                // 结果
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = encoding.GetString(reader.ReadBytes(dataLength));
                return data;
            }
            case 11503:
            {
                // 结果
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = encoding.GetString(reader.ReadBytes(dataLength));
                return data;
            }
            case 11504:
            {
                // 
                // 结果
                var dataResultLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var dataResult = encoding.GetString(reader.ReadBytes(dataResultLength));
                // 好友角色ID
                var dataFriendRoleId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // object
                var data = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"result", dataResult}, {"friendRoleId", dataFriendRoleId}};
                return data;
            }
            case 11505:
            {
                // 
                // 结果
                var dataResultLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var dataResult = encoding.GetString(reader.ReadBytes(dataResultLength));
                // 好友角色ID
                var dataFriendRoleId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // object
                var data = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"result", dataResult}, {"friendRoleId", dataFriendRoleId}};
                return data;
            }
            case 11506:
            {
                // 
                // 结果
                var dataResultLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var dataResult = encoding.GetString(reader.ReadBytes(dataResultLength));
                // 好友角色ID
                var dataFriendRoleId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // object
                var data = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"result", dataResult}, {"friendRoleId", dataFriendRoleId}};
                return data;
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }
}