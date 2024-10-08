public static class GuildProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, System.Collections.Generic.Dictionary<System.String, System.Object> data) 
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
                var guildNameBytes = encoding.GetBytes((System.String)data["guildName"]);
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)guildNameBytes.Length));
                writer.Write(guildNameBytes);
                return;
            }
            case 30108:
            {
                // 公会ID
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)data["guildId"]));
                return;
            }
            case 30109:
            {
                // 公会ID
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)data["guildId"]));
                return;
            }
            case 30110:
            {
                return;
            }
            case 30111:
            {
                // 角色ID
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)data["roleId"]));
                return;
            }
            case 30112:
            {
                return;
            }
            case 30113:
            {
                // 角色ID
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)data["roleId"]));
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
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)data["roleId"]));
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
                var noticeBytes = encoding.GetBytes((System.String)data["notice"]);
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)noticeBytes.Length));
                writer.Write(noticeBytes);
                return;
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }

    public static System.Collections.Generic.Dictionary<System.String, System.Object> Decode(System.Text.Encoding encoding, System.IO.BinaryReader reader, System.UInt16 protocol) 
    {
        switch (protocol) 
        {
            case 30101:
            {
                // 公会列表
                var listLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var list = new System.Collections.Generic.List<System.Object>(listLength);
                while (listLength-- > 0)
                {
                    // Guild
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
                    var guild = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"guildId", guildId}, {"guildName", guildName}, {"createTime", createTime}, {"leaderRoleId", leaderRoleId}, {"leaderName", leaderName}};
                    // add
                    list.Add(guild);
                }
                return new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"list", list}};
            }
            case 30102:
            {
                // 成员列表
                var listLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var list = new System.Collections.Generic.List<System.Object>(listLength);
                while (listLength-- > 0)
                {
                    // GuildRole
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
                    var guildRole = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"roleId", roleId}, {"job", job}, {"joinTime", joinTime}, {"roleName", roleName}, {"sex", sex}, {"classes", classes}, {"vipLevel", vipLevel}};
                    // add
                    list.Add(guildRole);
                }
                return new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"list", list}};
            }
            case 30103:
            {
                // 申请列表
                var listLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var list = new System.Collections.Generic.List<System.Object>(listLength);
                while (listLength-- > 0)
                {
                    // GuildApply
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
                    var guildApply = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"roleId", roleId}, {"applyTime", applyTime}, {"roleName", roleName}, {"sex", sex}, {"classes", classes}, {"vipLevel", vipLevel}};
                    // add
                    list.Add(guildApply);
                }
                return new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"list", list}};
            }
            case 30104:
            {
                // Guild
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
                var guild = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"guildId", guildId}, {"guildName", guildName}, {"exp", exp}, {"wealth", wealth}, {"level", level}, {"createTime", createTime}, {"notice", notice}, {"leaderRoleId", leaderRoleId}, {"leaderName", leaderName}};
                return new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"guild", guild}};
            }
            case 30105:
            {
                // GuildRole
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
                var guildRole = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"roleId", roleId}, {"job", job}, {"joinTime", joinTime}, {"roleName", roleName}, {"sex", sex}, {"classes", classes}, {"vipLevel", vipLevel}};
                return new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"guildRole", guildRole}};
            }
            case 30106:
            {
                // 
                var listLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var list = new System.Collections.Generic.List<System.Object>(listLength);
                while (listLength-- > 0)
                {
                    // GuildApply
                    // 公会ID
                    var guildId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 申请时间
                    var applyTime = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // 公会名字
                    var guildNameLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var guildName = encoding.GetString(reader.ReadBytes(guildNameLength));
                    // object
                    var guildApply = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"guildId", guildId}, {"applyTime", applyTime}, {"guildName", guildName}};
                    // add
                    list.Add(guildApply);
                }
                return new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"list", list}};
            }
            case 30107:
            {
                // 结果
                var resultLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var result = encoding.GetString(reader.ReadBytes(resultLength));
                return new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"result", result}};
            }
            case 30108:
            {
                // 结果
                var resultLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var result = encoding.GetString(reader.ReadBytes(resultLength));
                return new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"result", result}};
            }
            case 30109:
            {
                // 结果
                var resultLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var result = encoding.GetString(reader.ReadBytes(resultLength));
                return new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"result", result}};
            }
            case 30110:
            {
                // 结果
                var resultLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var result = encoding.GetString(reader.ReadBytes(resultLength));
                return new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"result", result}};
            }
            case 30111:
            {
                // 结果
                var resultLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var result = encoding.GetString(reader.ReadBytes(resultLength));
                return new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"result", result}};
            }
            case 30112:
            {
                // 结果
                var resultLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var result = encoding.GetString(reader.ReadBytes(resultLength));
                return new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"result", result}};
            }
            case 30113:
            {
                // 结果
                var resultLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var result = encoding.GetString(reader.ReadBytes(resultLength));
                return new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"result", result}};
            }
            case 30114:
            {
                // 结果
                var resultLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var result = encoding.GetString(reader.ReadBytes(resultLength));
                return new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"result", result}};
            }
            case 30115:
            {
                // 结果
                var resultLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var result = encoding.GetString(reader.ReadBytes(resultLength));
                return new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"result", result}};
            }
            case 30116:
            {
                // 结果
                var resultLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var result = encoding.GetString(reader.ReadBytes(resultLength));
                return new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"result", result}};
            }
            case 30117:
            {
                // 结果
                var resultLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var result = encoding.GetString(reader.ReadBytes(resultLength));
                return new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"result", result}};
            }
            case 30118:
            {
                // 结果
                var resultLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var result = encoding.GetString(reader.ReadBytes(resultLength));
                return new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"result", result}};
            }
            case 30119:
            {
                // 结果
                var resultLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var result = encoding.GetString(reader.ReadBytes(resultLength));
                return new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"result", result}};
            }
            case 30120:
            {
                // 结果
                var resultLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var result = encoding.GetString(reader.ReadBytes(resultLength));
                return new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"result", result}};
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }
}