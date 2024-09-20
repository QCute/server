public static class RankWorldProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, System.Collections.Generic.Dictionary<System.String, System.Object> data) 
    {
        switch (protocol) 
        {
            case 19201:
            {
                return;
            }
            case 19202:
            {
                return;
            }
            case 19203:
            {
                return;
            }
            case 19204:
            {
                return;
            }
            case 19205:
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
            case 19201:
            {
                // 排行榜
                var listLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var list = new System.Collections.Generic.List<System.Object>(listLength);
                while (listLength-- > 0)
                {
                    // Rank
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
                    list.Add(rank);
                }
                return new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"list", list}};
            }
            case 19202:
            {
                // 排行榜
                var listLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var list = new System.Collections.Generic.List<System.Object>(listLength);
                while (listLength-- > 0)
                {
                    // Rank
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
                    var level = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // 职业
                    var classes = reader.ReadByte();
                    // object
                    var other = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"level", level}, {"classes", classes}};
                    // object
                    var rank = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"type", type}, {"order", order}, {"key", key}, {"value", value}, {"time", time}, {"name", name}, {"serverId", serverId}, {"other", other}};
                    // add
                    list.Add(rank);
                }
                return new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"list", list}};
            }
            case 19203:
            {
                // 排行榜
                var listLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var list = new System.Collections.Generic.List<System.Object>(listLength);
                while (listLength-- > 0)
                {
                    // Rank
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
                    var level = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // 职业
                    var classes = reader.ReadByte();
                    // 性别
                    var sex = reader.ReadByte();
                    // object
                    var other = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"level", level}, {"classes", classes}, {"sex", sex}};
                    // object
                    var rank = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"type", type}, {"order", order}, {"key", key}, {"value", value}, {"time", time}, {"name", name}, {"serverId", serverId}, {"other", other}};
                    // add
                    list.Add(rank);
                }
                return new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"list", list}};
            }
            case 19204:
            {
                // 排行榜
                var listLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var list = new System.Collections.Generic.List<System.Object>(listLength);
                while (listLength-- > 0)
                {
                    // Rank
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
                    var level = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // 职业
                    var classes = reader.ReadByte();
                    // 性别
                    var sex = reader.ReadByte();
                    // VIP等级
                    var vipLevel = reader.ReadByte();
                    // object
                    var other = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"level", level}, {"classes", classes}, {"sex", sex}, {"vipLevel", vipLevel}};
                    // object
                    var rank = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"type", type}, {"order", order}, {"key", key}, {"value", value}, {"time", time}, {"name", name}, {"serverId", serverId}, {"other", other}};
                    // add
                    list.Add(rank);
                }
                return new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"list", list}};
            }
            case 19205:
            {
                // 排行榜
                var listLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var list = new System.Collections.Generic.List<System.Object>(listLength);
                while (listLength-- > 0)
                {
                    // Rank
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
                    var level = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // 职业
                    var classes = reader.ReadByte();
                    // 性别
                    var sex = reader.ReadByte();
                    // VIP等级
                    var vipLevel = reader.ReadByte();
                    // 头像
                    var avatar = reader.ReadByte();
                    // object
                    var other = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"level", level}, {"classes", classes}, {"sex", sex}, {"vipLevel", vipLevel}, {"avatar", avatar}};
                    // object
                    var rank = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"type", type}, {"order", order}, {"key", key}, {"value", value}, {"time", time}, {"name", name}, {"serverId", serverId}, {"other", other}};
                    // add
                    list.Add(rank);
                }
                return new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"list", list}};
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }
}