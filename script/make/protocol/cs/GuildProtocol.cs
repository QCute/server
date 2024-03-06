public static class GuildProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, System.Object data) 
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
                // convert
                var dataCast = (System.Collections.Generic.Dictionary<System.String, System.Object>)data;
                // 类型
                writer.Write((System.Byte)dataCast["type"]);
                // 公会名
                var guildNameBytes = encoding.GetBytes((System.String)dataCast["guildName"]);
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)guildNameBytes.Length));
                writer.Write(guildNameBytes);
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
                // convert
                var dataCast = (System.Collections.Generic.Dictionary<System.String, System.Object>)data;
                // 角色ID
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)dataCast["roleId"]));
                // 位置
                writer.Write((System.Byte)dataCast["job"]);
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
                    var guildId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 公会名字
                    var guildNameLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var guildName = encoding.GetString(reader.ReadBytes(guildNameLength));
                    // 创建时间
                    var createTime = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // 会长角色ID
                    var leaderRoleId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 会长名字
                    var leaderNameLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var leaderName = encoding.GetString(reader.ReadBytes(leaderNameLength));
                    // object
                    var guild = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"guild_id", guildId}, {"guild_name", guildName}, {"create_time", createTime}, {"leader_role_id", leaderRoleId}, {"leader_name", leaderName}};
                    // add
                    data.Add(guild);
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
                    var roleId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 职位
                    var job = reader.ReadByte();
                    // 加入时间
                    var joinTime = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // 成员名字
                    var roleNameLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var roleName = encoding.GetString(reader.ReadBytes(roleNameLength));
                    // 性别
                    var sex = reader.ReadByte();
                    // 职业
                    var classes = reader.ReadByte();
                    // Vip等级
                    var vipLevel = reader.ReadByte();
                    // object
                    var guildRole = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"role_id", roleId}, {"job", job}, {"join_time", joinTime}, {"role_name", roleName}, {"sex", sex}, {"classes", classes}, {"vip_level", vipLevel}};
                    // add
                    data.Add(guildRole);
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
                    var roleId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 申请时间
                    var applyTime = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // 申请名字
                    var roleNameLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var roleName = encoding.GetString(reader.ReadBytes(roleNameLength));
                    // 性别
                    var sex = reader.ReadByte();
                    // 职业
                    var classes = reader.ReadByte();
                    // Vip等级
                    var vipLevel = reader.ReadByte();
                    // object
                    var guildApply = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"role_id", roleId}, {"apply_time", applyTime}, {"role_name", roleName}, {"sex", sex}, {"classes", classes}, {"vip_level", vipLevel}};
                    // add
                    data.Add(guildApply);
                }
                return data;
            }
            case 30104:
            {
                // 
                // 公会ID
                var guildId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // 公会名字
                var guildNameLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var guildName = encoding.GetString(reader.ReadBytes(guildNameLength));
                // 经验
                var exp = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                // 财富
                var wealth = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                // 等级
                var level = reader.ReadByte();
                // 创建时间
                var createTime = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                // 公告
                var noticeLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var notice = encoding.GetString(reader.ReadBytes(noticeLength));
                // 会长角色ID
                var leaderRoleId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // 会长名字
                var leaderNameLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var leaderName = encoding.GetString(reader.ReadBytes(leaderNameLength));
                // object
                var guild = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"guild_id", guildId}, {"guild_name", guildName}, {"exp", exp}, {"wealth", wealth}, {"level", level}, {"create_time", createTime}, {"notice", notice}, {"leader_role_id", leaderRoleId}, {"leader_name", leaderName}};
                return guild;
            }
            case 30105:
            {
                // 
                // 成员ID
                var roleId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // 职位
                var job = reader.ReadByte();
                // 加入时间
                var joinTime = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                // 成员名字
                var roleNameLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var roleName = encoding.GetString(reader.ReadBytes(roleNameLength));
                // 性别
                var sex = reader.ReadByte();
                // 职业
                var classes = reader.ReadByte();
                // Vip等级
                var vipLevel = reader.ReadByte();
                // object
                var guildRole = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"role_id", roleId}, {"job", job}, {"join_time", joinTime}, {"role_name", roleName}, {"sex", sex}, {"classes", classes}, {"vip_level", vipLevel}};
                return guildRole;
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
                    var guildId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 申请时间
                    var applyTime = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // 公会名字
                    var guildNameLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var guildName = encoding.GetString(reader.ReadBytes(guildNameLength));
                    // object
                    var guildApply = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"guild_id", guildId}, {"apply_time", applyTime}, {"guild_name", guildName}};
                    // add
                    data.Add(guildApply);
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