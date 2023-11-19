public class RoleQueryRequest
{
    public System.UInt16 protocol = 10101;
    public Empty data;
}

public class RoleQueryResponse
{
    public System.UInt16 protocol = 10101;
    public (
        System.UInt64 roleId,                                                   // 角色ID
        System.String roleName,                                                 // 角色名
        System.Byte sex,                                                        // 性别
        System.Byte classes,                                                    // 职业
        System.UInt64 level                                                     // 等级
    ) data;
}

public class RoleAssetQueryRequest
{
    public System.UInt16 protocol = 10102;
    public Empty data;
}

public class RoleAssetQueryResponse
{
    public System.UInt16 protocol = 10102;
    public (
        System.UInt64 gold,                                                     // 金币
        System.UInt32 silver,                                                   // 银币
        System.UInt64 copper,                                                   // 铜币
        System.UInt64 exp                                                       // 经验
    ) data;
}

public class RoleVipQueryRequest
{
    public System.UInt16 protocol = 10103;
    public Empty data;
}

public class RoleVipQueryResponse
{
    public System.UInt16 protocol = 10103;
    public (
        System.Byte vipLevel,                                                   // 等级
        System.UInt64 exp,                                                      // 经验
        System.UInt32 expireTime                                                // 过期时间
    ) data;
}

public static class RoleProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, System.Object dataRaw) 
    {
        switch (protocol) 
        {
            case 10101:
            {
                #pragma warning disable
                var data = (Empty)dataRaw;
                #pragma warning restore

                return;
            }
            case 10102:
            {
                #pragma warning disable
                var data = (Empty)dataRaw;
                #pragma warning restore

                return;
            }
            case 10103:
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
            case 10101:
            {
                // 
                // 角色ID
                var dataRoleId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // 角色名
                var dataRoleNameLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var dataRoleName = encoding.GetString(reader.ReadBytes(dataRoleNameLength));
                // 性别
                var dataSex = reader.ReadByte();
                // 职业
                var dataClasses = reader.ReadByte();
                // 等级
                var dataLevel = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // object
                var data = (roleId: dataRoleId, roleName: dataRoleName, sex: dataSex, classes: dataClasses, level: dataLevel);
                return (protocol: 10101, data: data);
            }
            case 10102:
            {
                // 
                // 金币
                var dataGold = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // 银币
                var dataSilver = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                // 铜币
                var dataCopper = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // 经验
                var dataExp = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // object
                var data = (gold: dataGold, silver: dataSilver, copper: dataCopper, exp: dataExp);
                return (protocol: 10102, data: data);
            }
            case 10103:
            {
                // 
                // 等级
                var dataVipLevel = reader.ReadByte();
                // 经验
                var dataExp = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // 过期时间
                var dataExpireTime = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                // object
                var data = (vipLevel: dataVipLevel, exp: dataExp, expireTime: dataExpireTime);
                return (protocol: 10103, data: data);
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }
}