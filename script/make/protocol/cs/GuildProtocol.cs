public class GuildQueryGuildRequest
{
    public System.UInt16 protocol = 30101;
    public Empty data;
}

public class GuildQueryGuildResponse
{
    public System.UInt16 protocol = 30101;
    public System.Collections.Generic.List<(
        System.UInt64 guildId,                                                  // 公会ID
        System.String guildName,                                                // 公会名字
        System.UInt32 createTime,                                               // 创建时间
        System.UInt64 leaderRoleId,                                             // 会长角色ID
        System.String leaderName                                                // 会长名字
    )> data;
}

public class GuildQueryRoleRequest
{
    public System.UInt16 protocol = 30102;
    public Empty data;
}

public class GuildQueryRoleResponse
{
    public System.UInt16 protocol = 30102;
    public System.Collections.Generic.List<(
        System.UInt64 roleId,                                                   // 成员ID
        System.Byte job,                                                        // 职位
        System.UInt32 joinTime,                                                 // 加入时间
        System.String roleName,                                                 // 成员名字
        System.Byte sex,                                                        // 性别
        System.Byte classes,                                                    // 职业
        System.Byte vipLevel                                                    // Vip等级
    )> data;
}

public class GuildQueryApplyRequest
{
    public System.UInt16 protocol = 30103;
    public Empty data;
}

public class GuildQueryApplyResponse
{
    public System.UInt16 protocol = 30103;
    public System.Collections.Generic.List<(
        System.UInt64 roleId,                                                   // 申请ID
        System.UInt32 applyTime,                                                // 申请时间
        System.String roleName,                                                 // 申请名字
        System.Byte sex,                                                        // 性别
        System.Byte classes,                                                    // 职业
        System.Byte vipLevel                                                    // Vip等级
    )> data;
}

public class GuildQuerySelfGuildRequest
{
    public System.UInt16 protocol = 30104;
    public Empty data;
}

public class GuildQuerySelfGuildResponse
{
    public System.UInt16 protocol = 30104;
    public (
        System.UInt64 guildId,                                                  // 公会ID
        System.String guildName,                                                // 公会名字
        System.UInt32 exp,                                                      // 经验
        System.UInt32 wealth,                                                   // 财富
        System.Byte level,                                                      // 等级
        System.UInt32 createTime,                                               // 创建时间
        System.String notice,                                                   // 公告
        System.UInt64 leaderRoleId,                                             // 会长角色ID
        System.String leaderName                                                // 会长名字
    ) data;
}

public class GuildQuerySelfRoleRequest
{
    public System.UInt16 protocol = 30105;
    public Empty data;
}

public class GuildQuerySelfRoleResponse
{
    public System.UInt16 protocol = 30105;
    public (
        System.UInt64 roleId,                                                   // 成员ID
        System.Byte job,                                                        // 职位
        System.UInt32 joinTime,                                                 // 加入时间
        System.String roleName,                                                 // 成员名字
        System.Byte sex,                                                        // 性别
        System.Byte classes,                                                    // 职业
        System.Byte vipLevel                                                    // Vip等级
    ) data;
}

public class GuildQuerySelfApplyRequest
{
    public System.UInt16 protocol = 30106;
    public Empty data;
}

public class GuildQuerySelfApplyResponse
{
    public System.UInt16 protocol = 30106;
    public System.Collections.Generic.List<(
        System.UInt64 guildId,                                                  // 公会ID
        System.UInt32 applyTime,                                                // 申请时间
        System.String guildName                                                 // 公会名字
    )> data;
}

public class GuildCreateRequest
{
    public System.UInt16 protocol = 30107;
    public (
        System.Byte type,                                                       // 类型
        System.String guildName                                                 // 公会名
    ) data;
}

public class GuildCreateResponse
{
    public System.UInt16 protocol = 30107;
    public System.String data;
}

public class GuildApplyRequest
{
    public System.UInt16 protocol = 30108;
    public System.UInt64 data;
}

public class GuildApplyResponse
{
    public System.UInt16 protocol = 30108;
    public System.String data;
}

public class GuildCancelApplyRequest
{
    public System.UInt16 protocol = 30109;
    public System.UInt64 data;
}

public class GuildCancelApplyResponse
{
    public System.UInt16 protocol = 30109;
    public System.String data;
}

public class GuildCancelAllApplyRequest
{
    public System.UInt16 protocol = 30110;
    public Empty data;
}

public class GuildCancelAllApplyResponse
{
    public System.UInt16 protocol = 30110;
    public System.String data;
}

public class GuildApproveApplyRequest
{
    public System.UInt16 protocol = 30111;
    public System.UInt64 data;
}

public class GuildApproveApplyResponse
{
    public System.UInt16 protocol = 30111;
    public System.String data;
}

public class GuildApproveAllApplyRequest
{
    public System.UInt16 protocol = 30112;
    public Empty data;
}

public class GuildApproveAllApplyResponse
{
    public System.UInt16 protocol = 30112;
    public System.String data;
}

public class GuildRejectApplyRequest
{
    public System.UInt16 protocol = 30113;
    public System.UInt64 data;
}

public class GuildRejectApplyResponse
{
    public System.UInt16 protocol = 30113;
    public System.String data;
}

public class GuildRejectAllApplyRequest
{
    public System.UInt16 protocol = 30114;
    public Empty data;
}

public class GuildRejectAllApplyResponse
{
    public System.UInt16 protocol = 30114;
    public System.String data;
}

public class GuildLeaveRequest
{
    public System.UInt16 protocol = 30115;
    public Empty data;
}

public class GuildLeaveResponse
{
    public System.UInt16 protocol = 30115;
    public System.String data;
}

public class GuildDismissRequest
{
    public System.UInt16 protocol = 30116;
    public Empty data;
}

public class GuildDismissResponse
{
    public System.UInt16 protocol = 30116;
    public System.String data;
}

public class GuildKickRequest
{
    public System.UInt16 protocol = 30117;
    public System.UInt64 data;
}

public class GuildKickResponse
{
    public System.UInt16 protocol = 30117;
    public System.String data;
}

public class GuildUpdateJobRequest
{
    public System.UInt16 protocol = 30118;
    public (
        System.UInt64 roleId,                                                   // 角色ID
        System.Byte job                                                         // 位置
    ) data;
}

public class GuildUpdateJobResponse
{
    public System.UInt16 protocol = 30118;
    public System.String data;
}

public class GuildUpgradeLevelRequest
{
    public System.UInt16 protocol = 30119;
    public Empty data;
}

public class GuildUpgradeLevelResponse
{
    public System.UInt16 protocol = 30119;
    public System.String data;
}

public class GuildChangeNoticeRequest
{
    public System.UInt16 protocol = 30120;
    public System.String data;
}

public class GuildChangeNoticeResponse
{
    public System.UInt16 protocol = 30120;
    public System.String data;
}

public static class GuildProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, System.Object dataRaw) 
    {
        switch (protocol) 
        {
            case 30101:
            {
                #pragma warning disable
                var data = (Empty)dataRaw;
                #pragma warning restore

                return;
            }
            case 30102:
            {
                #pragma warning disable
                var data = (Empty)dataRaw;
                #pragma warning restore

                return;
            }
            case 30103:
            {
                #pragma warning disable
                var data = (Empty)dataRaw;
                #pragma warning restore

                return;
            }
            case 30104:
            {
                #pragma warning disable
                var data = (Empty)dataRaw;
                #pragma warning restore

                return;
            }
            case 30105:
            {
                #pragma warning disable
                var data = (Empty)dataRaw;
                #pragma warning restore

                return;
            }
            case 30106:
            {
                #pragma warning disable
                var data = (Empty)dataRaw;
                #pragma warning restore

                return;
            }
            case 30107:
            {
                var data = ((System.Byte type, System.String guildName))dataRaw;

                // 类型
                writer.Write((System.Byte)data.type);
                // 公会名
                var dataGuildNameBytes = encoding.GetBytes((System.String)data.guildName);
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)dataGuildNameBytes.Length));
                writer.Write(dataGuildNameBytes);
                return;
            }
            case 30108:
            {
                var data = (System.UInt64)dataRaw;
                // 公会ID
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)data));
                return;
            }
            case 30109:
            {
                var data = (System.UInt64)dataRaw;
                // 公会ID
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)data));
                return;
            }
            case 30110:
            {
                #pragma warning disable
                var data = (Empty)dataRaw;
                #pragma warning restore

                return;
            }
            case 30111:
            {
                var data = (System.UInt64)dataRaw;
                // 角色ID
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)data));
                return;
            }
            case 30112:
            {
                #pragma warning disable
                var data = (Empty)dataRaw;
                #pragma warning restore

                return;
            }
            case 30113:
            {
                var data = (System.UInt64)dataRaw;
                // 角色ID
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)data));
                return;
            }
            case 30114:
            {
                #pragma warning disable
                var data = (Empty)dataRaw;
                #pragma warning restore

                return;
            }
            case 30115:
            {
                #pragma warning disable
                var data = (Empty)dataRaw;
                #pragma warning restore

                return;
            }
            case 30116:
            {
                #pragma warning disable
                var data = (Empty)dataRaw;
                #pragma warning restore

                return;
            }
            case 30117:
            {
                var data = (System.UInt64)dataRaw;
                // 角色ID
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)data));
                return;
            }
            case 30118:
            {
                var data = ((System.UInt64 roleId, System.Byte job))dataRaw;

                // 角色ID
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)data.roleId));
                // 位置
                writer.Write((System.Byte)data.job);
                return;
            }
            case 30119:
            {
                #pragma warning disable
                var data = (Empty)dataRaw;
                #pragma warning restore

                return;
            }
            case 30120:
            {
                var data = (System.String)dataRaw;
                // 公告
                var dataBytes = encoding.GetBytes((System.String)data);
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)dataBytes.Length));
                writer.Write(dataBytes);
                return;
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }

    public static System.Object Decode(System.Text.Encoding encoding, System.IO.BinaryReader reader, System.UInt16 protocol) 
    {
        switch (protocol) 
        {
            case 30101:
            {
                // 
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<(System.UInt64 guildId, System.String guildName, System.UInt32 createTime, System.UInt64 leaderRoleId, System.String leaderName)>(dataLength);
                while (dataLength-- > 0)
                {
                    // 
                    // 公会ID
                    var dataDataGuildId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 公会名字
                    var dataDataGuildNameLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var dataDataGuildName = encoding.GetString(reader.ReadBytes(dataDataGuildNameLength));
                    // 创建时间
                    var dataDataCreateTime = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // 会长角色ID
                    var dataDataLeaderRoleId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 会长名字
                    var dataDataLeaderNameLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var dataDataLeaderName = encoding.GetString(reader.ReadBytes(dataDataLeaderNameLength));
                    // object
                    var dataData = (guildId: dataDataGuildId, guildName: dataDataGuildName, createTime: dataDataCreateTime, leaderRoleId: dataDataLeaderRoleId, leaderName: dataDataLeaderName);
                    // add
                    data.Add(dataData);
                }
                return (protocol: 30101, data: data);
            }
            case 30102:
            {
                // 
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<(System.UInt64 roleId, System.Byte job, System.UInt32 joinTime, System.String roleName, System.Byte sex, System.Byte classes, System.Byte vipLevel)>(dataLength);
                while (dataLength-- > 0)
                {
                    // 
                    // 成员ID
                    var dataDataRoleId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 职位
                    var dataDataJob = reader.ReadByte();
                    // 加入时间
                    var dataDataJoinTime = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // 成员名字
                    var dataDataRoleNameLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var dataDataRoleName = encoding.GetString(reader.ReadBytes(dataDataRoleNameLength));
                    // 性别
                    var dataDataSex = reader.ReadByte();
                    // 职业
                    var dataDataClasses = reader.ReadByte();
                    // Vip等级
                    var dataDataVipLevel = reader.ReadByte();
                    // object
                    var dataData = (roleId: dataDataRoleId, job: dataDataJob, joinTime: dataDataJoinTime, roleName: dataDataRoleName, sex: dataDataSex, classes: dataDataClasses, vipLevel: dataDataVipLevel);
                    // add
                    data.Add(dataData);
                }
                return (protocol: 30102, data: data);
            }
            case 30103:
            {
                // 
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<(System.UInt64 roleId, System.UInt32 applyTime, System.String roleName, System.Byte sex, System.Byte classes, System.Byte vipLevel)>(dataLength);
                while (dataLength-- > 0)
                {
                    // 
                    // 申请ID
                    var dataDataRoleId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 申请时间
                    var dataDataApplyTime = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // 申请名字
                    var dataDataRoleNameLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var dataDataRoleName = encoding.GetString(reader.ReadBytes(dataDataRoleNameLength));
                    // 性别
                    var dataDataSex = reader.ReadByte();
                    // 职业
                    var dataDataClasses = reader.ReadByte();
                    // Vip等级
                    var dataDataVipLevel = reader.ReadByte();
                    // object
                    var dataData = (roleId: dataDataRoleId, applyTime: dataDataApplyTime, roleName: dataDataRoleName, sex: dataDataSex, classes: dataDataClasses, vipLevel: dataDataVipLevel);
                    // add
                    data.Add(dataData);
                }
                return (protocol: 30103, data: data);
            }
            case 30104:
            {
                // 
                // 公会ID
                var dataGuildId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // 公会名字
                var dataGuildNameLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var dataGuildName = encoding.GetString(reader.ReadBytes(dataGuildNameLength));
                // 经验
                var dataExp = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                // 财富
                var dataWealth = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                // 等级
                var dataLevel = reader.ReadByte();
                // 创建时间
                var dataCreateTime = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                // 公告
                var dataNoticeLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var dataNotice = encoding.GetString(reader.ReadBytes(dataNoticeLength));
                // 会长角色ID
                var dataLeaderRoleId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // 会长名字
                var dataLeaderNameLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var dataLeaderName = encoding.GetString(reader.ReadBytes(dataLeaderNameLength));
                // object
                var data = (guildId: dataGuildId, guildName: dataGuildName, exp: dataExp, wealth: dataWealth, level: dataLevel, createTime: dataCreateTime, notice: dataNotice, leaderRoleId: dataLeaderRoleId, leaderName: dataLeaderName);
                return (protocol: 30104, data: data);
            }
            case 30105:
            {
                // 
                // 成员ID
                var dataRoleId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // 职位
                var dataJob = reader.ReadByte();
                // 加入时间
                var dataJoinTime = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                // 成员名字
                var dataRoleNameLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var dataRoleName = encoding.GetString(reader.ReadBytes(dataRoleNameLength));
                // 性别
                var dataSex = reader.ReadByte();
                // 职业
                var dataClasses = reader.ReadByte();
                // Vip等级
                var dataVipLevel = reader.ReadByte();
                // object
                var data = (roleId: dataRoleId, job: dataJob, joinTime: dataJoinTime, roleName: dataRoleName, sex: dataSex, classes: dataClasses, vipLevel: dataVipLevel);
                return (protocol: 30105, data: data);
            }
            case 30106:
            {
                // 
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<(System.UInt64 guildId, System.UInt32 applyTime, System.String guildName)>(dataLength);
                while (dataLength-- > 0)
                {
                    // 
                    // 公会ID
                    var dataDataGuildId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 申请时间
                    var dataDataApplyTime = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // 公会名字
                    var dataDataGuildNameLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var dataDataGuildName = encoding.GetString(reader.ReadBytes(dataDataGuildNameLength));
                    // object
                    var dataData = (guildId: dataDataGuildId, applyTime: dataDataApplyTime, guildName: dataDataGuildName);
                    // add
                    data.Add(dataData);
                }
                return (protocol: 30106, data: data);
            }
            case 30107:
            {
                // 结果
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = encoding.GetString(reader.ReadBytes(dataLength));
                return (protocol: 30107, data: data);
            }
            case 30108:
            {
                // 结果
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = encoding.GetString(reader.ReadBytes(dataLength));
                return (protocol: 30108, data: data);
            }
            case 30109:
            {
                // 结果
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = encoding.GetString(reader.ReadBytes(dataLength));
                return (protocol: 30109, data: data);
            }
            case 30110:
            {
                // 结果
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = encoding.GetString(reader.ReadBytes(dataLength));
                return (protocol: 30110, data: data);
            }
            case 30111:
            {
                // 结果
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = encoding.GetString(reader.ReadBytes(dataLength));
                return (protocol: 30111, data: data);
            }
            case 30112:
            {
                // 结果
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = encoding.GetString(reader.ReadBytes(dataLength));
                return (protocol: 30112, data: data);
            }
            case 30113:
            {
                // 结果
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = encoding.GetString(reader.ReadBytes(dataLength));
                return (protocol: 30113, data: data);
            }
            case 30114:
            {
                // 结果
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = encoding.GetString(reader.ReadBytes(dataLength));
                return (protocol: 30114, data: data);
            }
            case 30115:
            {
                // 结果
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = encoding.GetString(reader.ReadBytes(dataLength));
                return (protocol: 30115, data: data);
            }
            case 30116:
            {
                // 结果
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = encoding.GetString(reader.ReadBytes(dataLength));
                return (protocol: 30116, data: data);
            }
            case 30117:
            {
                // 结果
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = encoding.GetString(reader.ReadBytes(dataLength));
                return (protocol: 30117, data: data);
            }
            case 30118:
            {
                // 结果
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = encoding.GetString(reader.ReadBytes(dataLength));
                return (protocol: 30118, data: data);
            }
            case 30119:
            {
                // 结果
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = encoding.GetString(reader.ReadBytes(dataLength));
                return (protocol: 30119, data: data);
            }
            case 30120:
            {
                // 结果
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = encoding.GetString(reader.ReadBytes(dataLength));
                return (protocol: 30120, data: data);
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }
}