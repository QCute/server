public class FriendQueryRequest
{
    public System.UInt16 protocol = 11501;
    public Empty data;
}

public class FriendQueryResponse
{
    public System.UInt16 protocol = 11501;
    public System.Collections.Generic.List<(
        System.UInt64 friendRoleId,                                             // 好友角色ID
        System.String friendName,                                               // 好友名字
        System.Byte relation,                                                   // 关系状态(申请:1/好友:2/黑名单:3)
        System.UInt32 time                                                      // 添加/修改状态时间
    )> data;
}

public class FriendApplyRequest
{
    public System.UInt16 protocol = 11502;
    public System.UInt64 data;
}

public class FriendApplyResponse
{
    public System.UInt16 protocol = 11502;
    public System.String data;
}

public class FriendAgreeRequest
{
    public System.UInt16 protocol = 11503;
    public System.UInt64 data;
}

public class FriendAgreeResponse
{
    public System.UInt16 protocol = 11503;
    public System.String data;
}

public class FriendDeleteRequest
{
    public System.UInt16 protocol = 11504;
    public System.UInt64 data;
}

public class FriendDeleteResponse
{
    public System.UInt16 protocol = 11504;
    public (
        System.String result,                                                   // 结果
        System.UInt64 friendRoleId                                              // 好友角色ID
    ) data;
}

public class FriendBlockRequest
{
    public System.UInt16 protocol = 11505;
    public System.UInt64 data;
}

public class FriendBlockResponse
{
    public System.UInt16 protocol = 11505;
    public (
        System.String result,                                                   // 结果
        System.UInt64 friendRoleId                                              // 好友角色ID
    ) data;
}

public class FriendCancelBlockRequest
{
    public System.UInt16 protocol = 11506;
    public System.UInt64 data;
}

public class FriendCancelBlockResponse
{
    public System.UInt16 protocol = 11506;
    public (
        System.String result,                                                   // 结果
        System.UInt64 friendRoleId                                              // 好友角色ID
    ) data;
}

public static class FriendProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, System.Object dataRaw) 
    {
        switch (protocol) 
        {
            case 11501:
            {
                #pragma warning disable
                var data = (Empty)dataRaw;
                #pragma warning restore

                return;
            }
            case 11502:
            {
                var data = (System.UInt64)dataRaw;
                // 好友角色ID
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)data));
                return;
            }
            case 11503:
            {
                var data = (System.UInt64)dataRaw;
                // 好友角色ID
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)data));
                return;
            }
            case 11504:
            {
                var data = (System.UInt64)dataRaw;
                // 好友角色ID
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)data));
                return;
            }
            case 11505:
            {
                var data = (System.UInt64)dataRaw;
                // 好友角色ID
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)data));
                return;
            }
            case 11506:
            {
                var data = (System.UInt64)dataRaw;
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
                var data = new System.Collections.Generic.List<(System.UInt64 friendRoleId, System.String friendName, System.Byte relation, System.UInt32 time)>(dataLength);
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
                    var dataData = (friendRoleId: dataDataFriendRoleId, friendName: dataDataFriendName, relation: dataDataRelation, time: dataDataTime);
                    // add
                    data.Add(dataData);
                }
                return (protocol: 11501, data: data);
            }
            case 11502:
            {
                // 结果
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = encoding.GetString(reader.ReadBytes(dataLength));
                return (protocol: 11502, data: data);
            }
            case 11503:
            {
                // 结果
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = encoding.GetString(reader.ReadBytes(dataLength));
                return (protocol: 11503, data: data);
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
                var data = (result: dataResult, friendRoleId: dataFriendRoleId);
                return (protocol: 11504, data: data);
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
                var data = (result: dataResult, friendRoleId: dataFriendRoleId);
                return (protocol: 11505, data: data);
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
                var data = (result: dataResult, friendRoleId: dataFriendRoleId);
                return (protocol: 11506, data: data);
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }
}