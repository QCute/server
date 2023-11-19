public static class GuildProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, dynamic data) 
    {
        switch (protocol) 
        {
            case 30101:
            {

                return;
            }
            case 30102:
            {

                return;
            }
            case 30103:
            {

                return;
            }
            case 30104:
            {

                return;
            }
            case 30105:
            {

                return;
            }
            case 30106:
            {

                return;
            }
            case 30107:
            {

                // 类型
                writer.Write((System.Byte)data["type"]);
                // 公会名
                var dataGuildNameBytes = encoding.GetBytes((System.String)data["guildName"]);
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)dataGuildNameBytes.Length));
                writer.Write(dataGuildNameBytes);
                return;
            }
            case 30108:
            {
                // 公会ID
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)data));
                return;
            }
            case 30109:
            {
                // 公会ID
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)data));
                return;
            }
            case 30110:
            {

                return;
            }
            case 30111:
            {
                // 角色ID
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)data));
                return;
            }
            case 30112:
            {

                return;
            }
            case 30113:
            {
                // 角色ID
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)data));
                return;
            }
            case 30114:
            {

                return;
            }
            case 30115:
            {

                return;
            }
            case 30116:
            {

                return;
            }
            case 30117:
            {
                // 角色ID
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)data));
                return;
            }
            case 30118:
            {

                // 角色ID
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)data["roleId"]));
                // 位置
                writer.Write((System.Byte)data["job"]);
                return;
            }
            case 30119:
            {

                return;
            }
            case 30120:
            {
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
                var data = new System.Collections.Generic.List<System.Object>(dataLength);
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
                    var dataData = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"guildId", dataDataGuildId}, {"guildName", dataDataGuildName}, {"createTime", dataDataCreateTime}, {"leaderRoleId", dataDataLeaderRoleId}, {"leaderName", dataDataLeaderName}};
                    // add
                    data.Add(dataData);
                }
                return data;
            }
            case 30102:
            {
                // 
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<System.Object>(dataLength);
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
                    var dataData = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"roleId", dataDataRoleId}, {"job", dataDataJob}, {"joinTime", dataDataJoinTime}, {"roleName", dataDataRoleName}, {"sex", dataDataSex}, {"classes", dataDataClasses}, {"vipLevel", dataDataVipLevel}};
                    // add
                    data.Add(dataData);
                }
                return data;
            }
            case 30103:
            {
                // 
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<System.Object>(dataLength);
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
                    var dataData = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"roleId", dataDataRoleId}, {"applyTime", dataDataApplyTime}, {"roleName", dataDataRoleName}, {"sex", dataDataSex}, {"classes", dataDataClasses}, {"vipLevel", dataDataVipLevel}};
                    // add
                    data.Add(dataData);
                }
                return data;
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
                var data = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"guildId", dataGuildId}, {"guildName", dataGuildName}, {"exp", dataExp}, {"wealth", dataWealth}, {"level", dataLevel}, {"createTime", dataCreateTime}, {"notice", dataNotice}, {"leaderRoleId", dataLeaderRoleId}, {"leaderName", dataLeaderName}};
                return data;
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
                var data = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"roleId", dataRoleId}, {"job", dataJob}, {"joinTime", dataJoinTime}, {"roleName", dataRoleName}, {"sex", dataSex}, {"classes", dataClasses}, {"vipLevel", dataVipLevel}};
                return data;
            }
            case 30106:
            {
                // 
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<System.Object>(dataLength);
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
                    var dataData = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"guildId", dataDataGuildId}, {"applyTime", dataDataApplyTime}, {"guildName", dataDataGuildName}};
                    // add
                    data.Add(dataData);
                }
                return data;
            }
            case 30107:
            {
                // 结果
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = encoding.GetString(reader.ReadBytes(dataLength));
                return data;
            }
            case 30108:
            {
                // 结果
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = encoding.GetString(reader.ReadBytes(dataLength));
                return data;
            }
            case 30109:
            {
                // 结果
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = encoding.GetString(reader.ReadBytes(dataLength));
                return data;
            }
            case 30110:
            {
                // 结果
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = encoding.GetString(reader.ReadBytes(dataLength));
                return data;
            }
            case 30111:
            {
                // 结果
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = encoding.GetString(reader.ReadBytes(dataLength));
                return data;
            }
            case 30112:
            {
                // 结果
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = encoding.GetString(reader.ReadBytes(dataLength));
                return data;
            }
            case 30113:
            {
                // 结果
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = encoding.GetString(reader.ReadBytes(dataLength));
                return data;
            }
            case 30114:
            {
                // 结果
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = encoding.GetString(reader.ReadBytes(dataLength));
                return data;
            }
            case 30115:
            {
                // 结果
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = encoding.GetString(reader.ReadBytes(dataLength));
                return data;
            }
            case 30116:
            {
                // 结果
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = encoding.GetString(reader.ReadBytes(dataLength));
                return data;
            }
            case 30117:
            {
                // 结果
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = encoding.GetString(reader.ReadBytes(dataLength));
                return data;
            }
            case 30118:
            {
                // 结果
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = encoding.GetString(reader.ReadBytes(dataLength));
                return data;
            }
            case 30119:
            {
                // 结果
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = encoding.GetString(reader.ReadBytes(dataLength));
                return data;
            }
            case 30120:
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