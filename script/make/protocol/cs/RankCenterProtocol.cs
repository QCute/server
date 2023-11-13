public static class RankCenterProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, System.Object data) 
    {
        switch (protocol) 
        {
            case 19101:
            {
                return;
            }
            case 19102:
            {
                return;
            }
            case 19103:
            {
                return;
            }
            case 19104:
            {
                return;
            }
            case 19105:
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
            case 19101:
            {
                // 
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<System.Object>(dataLength);
                while (dataLength-- > 0)
                {
                    // 
                    // 类型
                    var type = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // 排名
                    var order = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 键
                    var key = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 值
                    var value = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 时间
                    var time = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // 名字
                    var nameLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var name = encoding.GetString(reader.ReadBytes(nameLength));
                    // 服务器ID
                    var serverId = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // object
                    var rank = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"type", type}, {"order", order}, {"key", key}, {"value", value}, {"time", time}, {"name", name}, {"serverId", serverId}};
                    // add
                    data.Add(rank);
                }
                return data;
            }
            case 19102:
            {
                // 
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<System.Object>(dataLength);
                while (dataLength-- > 0)
                {
                    // 
                    // 类型
                    var type = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // 排名
                    var order = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 键
                    var key = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 值
                    var value = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 时间
                    var time = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // 名字
                    var nameLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var name = encoding.GetString(reader.ReadBytes(nameLength));
                    // 服务器ID
                    var serverId = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // 
                    // 等级
                    var otherLevel = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // 职业
                    var otherClasses = reader.ReadByte();
                    // object
                    var other = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"level", otherLevel}, {"classes", otherClasses}};
                    // object
                    var rank = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"type", type}, {"order", order}, {"key", key}, {"value", value}, {"time", time}, {"name", name}, {"serverId", serverId}, {"other", other}};
                    // add
                    data.Add(rank);
                }
                return data;
            }
            case 19103:
            {
                // 
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<System.Object>(dataLength);
                while (dataLength-- > 0)
                {
                    // 
                    // 类型
                    var type = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // 排名
                    var order = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 键
                    var key = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 值
                    var value = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 时间
                    var time = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // 名字
                    var nameLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var name = encoding.GetString(reader.ReadBytes(nameLength));
                    // 服务器ID
                    var serverId = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // 
                    // 等级
                    var otherLevel = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // 职业
                    var otherClasses = reader.ReadByte();
                    // 性别
                    var otherSex = reader.ReadByte();
                    // object
                    var other = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"level", otherLevel}, {"classes", otherClasses}, {"sex", otherSex}};
                    // object
                    var rank = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"type", type}, {"order", order}, {"key", key}, {"value", value}, {"time", time}, {"name", name}, {"serverId", serverId}, {"other", other}};
                    // add
                    data.Add(rank);
                }
                return data;
            }
            case 19104:
            {
                // 
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<System.Object>(dataLength);
                while (dataLength-- > 0)
                {
                    // 
                    // 类型
                    var type = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // 排名
                    var order = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 键
                    var key = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 值
                    var value = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 时间
                    var time = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // 名字
                    var nameLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var name = encoding.GetString(reader.ReadBytes(nameLength));
                    // 服务器ID
                    var serverId = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // 
                    // 等级
                    var otherLevel = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // 职业
                    var otherClasses = reader.ReadByte();
                    // 性别
                    var otherSex = reader.ReadByte();
                    // VIP等级
                    var otherVipLevel = reader.ReadByte();
                    // object
                    var other = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"level", otherLevel}, {"classes", otherClasses}, {"sex", otherSex}, {"vipLevel", otherVipLevel}};
                    // object
                    var rank = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"type", type}, {"order", order}, {"key", key}, {"value", value}, {"time", time}, {"name", name}, {"serverId", serverId}, {"other", other}};
                    // add
                    data.Add(rank);
                }
                return data;
            }
            case 19105:
            {
                // 
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<System.Object>(dataLength);
                while (dataLength-- > 0)
                {
                    // 
                    // 类型
                    var type = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // 排名
                    var order = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 键
                    var key = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 值
                    var value = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 时间
                    var time = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // 名字
                    var nameLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var name = encoding.GetString(reader.ReadBytes(nameLength));
                    // 服务器ID
                    var serverId = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // 
                    // 等级
                    var otherLevel = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // 职业
                    var otherClasses = reader.ReadByte();
                    // 性别
                    var otherSex = reader.ReadByte();
                    // VIP等级
                    var otherVipLevel = reader.ReadByte();
                    // 头像
                    var otherAvatar = reader.ReadByte();
                    // object
                    var other = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"level", otherLevel}, {"classes", otherClasses}, {"sex", otherSex}, {"vipLevel", otherVipLevel}, {"avatar", otherAvatar}};
                    // object
                    var rank = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"type", type}, {"order", order}, {"key", key}, {"value", value}, {"time", time}, {"name", name}, {"serverId", serverId}, {"other", other}};
                    // add
                    data.Add(rank);
                }
                return data;
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }
}