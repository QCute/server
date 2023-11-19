public static class RankProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, dynamic data) 
    {
        switch (protocol) 
        {
            case 19001:
            {

                return;
            }
            case 19002:
            {

                return;
            }
            case 19003:
            {

                return;
            }
            case 19004:
            {

                return;
            }
            case 19005:
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
            case 19001:
            {
                // 
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<System.Object>(dataLength);
                while (dataLength-- > 0)
                {
                    // 
                    // 类型
                    var dataDataType = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // 排名
                    var dataDataOrder = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 键
                    var dataDataKey = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 值
                    var dataDataValue = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 时间
                    var dataDataTime = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // 名字
                    var dataDataNameLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var dataDataName = encoding.GetString(reader.ReadBytes(dataDataNameLength));
                    // 服务器ID
                    var dataDataServerId = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // object
                    var dataData = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"type", dataDataType}, {"order", dataDataOrder}, {"key", dataDataKey}, {"value", dataDataValue}, {"time", dataDataTime}, {"name", dataDataName}, {"serverId", dataDataServerId}};
                    // add
                    data.Add(dataData);
                }
                return data;
            }
            case 19002:
            {
                // 
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<System.Object>(dataLength);
                while (dataLength-- > 0)
                {
                    // 
                    // 类型
                    var dataDataType = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // 排名
                    var dataDataOrder = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 键
                    var dataDataKey = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 值
                    var dataDataValue = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 时间
                    var dataDataTime = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // 名字
                    var dataDataNameLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var dataDataName = encoding.GetString(reader.ReadBytes(dataDataNameLength));
                    // 服务器ID
                    var dataDataServerId = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // 
                    // 等级
                    var dataDataOtherLevel = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // 职业
                    var dataDataOtherClasses = reader.ReadByte();
                    // object
                    var dataDataOther = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"level", dataDataOtherLevel}, {"classes", dataDataOtherClasses}};
                    // object
                    var dataData = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"type", dataDataType}, {"order", dataDataOrder}, {"key", dataDataKey}, {"value", dataDataValue}, {"time", dataDataTime}, {"name", dataDataName}, {"serverId", dataDataServerId}, {"other", dataDataOther}};
                    // add
                    data.Add(dataData);
                }
                return data;
            }
            case 19003:
            {
                // 
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<System.Object>(dataLength);
                while (dataLength-- > 0)
                {
                    // 
                    // 类型
                    var dataDataType = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // 排名
                    var dataDataOrder = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 键
                    var dataDataKey = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 值
                    var dataDataValue = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 时间
                    var dataDataTime = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // 名字
                    var dataDataNameLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var dataDataName = encoding.GetString(reader.ReadBytes(dataDataNameLength));
                    // 服务器ID
                    var dataDataServerId = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // 
                    // 等级
                    var dataDataOtherLevel = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // 职业
                    var dataDataOtherClasses = reader.ReadByte();
                    // 性别
                    var dataDataOtherSex = reader.ReadByte();
                    // object
                    var dataDataOther = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"level", dataDataOtherLevel}, {"classes", dataDataOtherClasses}, {"sex", dataDataOtherSex}};
                    // object
                    var dataData = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"type", dataDataType}, {"order", dataDataOrder}, {"key", dataDataKey}, {"value", dataDataValue}, {"time", dataDataTime}, {"name", dataDataName}, {"serverId", dataDataServerId}, {"other", dataDataOther}};
                    // add
                    data.Add(dataData);
                }
                return data;
            }
            case 19004:
            {
                // 
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<System.Object>(dataLength);
                while (dataLength-- > 0)
                {
                    // 
                    // 类型
                    var dataDataType = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // 排名
                    var dataDataOrder = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 键
                    var dataDataKey = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 值
                    var dataDataValue = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 时间
                    var dataDataTime = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // 名字
                    var dataDataNameLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var dataDataName = encoding.GetString(reader.ReadBytes(dataDataNameLength));
                    // 服务器ID
                    var dataDataServerId = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // 
                    // 等级
                    var dataDataOtherLevel = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // 职业
                    var dataDataOtherClasses = reader.ReadByte();
                    // 性别
                    var dataDataOtherSex = reader.ReadByte();
                    // VIP等级
                    var dataDataOtherVipLevel = reader.ReadByte();
                    // object
                    var dataDataOther = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"level", dataDataOtherLevel}, {"classes", dataDataOtherClasses}, {"sex", dataDataOtherSex}, {"vipLevel", dataDataOtherVipLevel}};
                    // object
                    var dataData = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"type", dataDataType}, {"order", dataDataOrder}, {"key", dataDataKey}, {"value", dataDataValue}, {"time", dataDataTime}, {"name", dataDataName}, {"serverId", dataDataServerId}, {"other", dataDataOther}};
                    // add
                    data.Add(dataData);
                }
                return data;
            }
            case 19005:
            {
                // 
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<System.Object>(dataLength);
                while (dataLength-- > 0)
                {
                    // 
                    // 类型
                    var dataDataType = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // 排名
                    var dataDataOrder = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 键
                    var dataDataKey = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 值
                    var dataDataValue = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 时间
                    var dataDataTime = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // 名字
                    var dataDataNameLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var dataDataName = encoding.GetString(reader.ReadBytes(dataDataNameLength));
                    // 服务器ID
                    var dataDataServerId = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // 
                    // 等级
                    var dataDataOtherLevel = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // 职业
                    var dataDataOtherClasses = reader.ReadByte();
                    // 性别
                    var dataDataOtherSex = reader.ReadByte();
                    // VIP等级
                    var dataDataOtherVipLevel = reader.ReadByte();
                    // 头像
                    var dataDataOtherAvatar = reader.ReadByte();
                    // object
                    var dataDataOther = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"level", dataDataOtherLevel}, {"classes", dataDataOtherClasses}, {"sex", dataDataOtherSex}, {"vipLevel", dataDataOtherVipLevel}, {"avatar", dataDataOtherAvatar}};
                    // object
                    var dataData = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"type", dataDataType}, {"order", dataDataOrder}, {"key", dataDataKey}, {"value", dataDataValue}, {"time", dataDataTime}, {"name", dataDataName}, {"serverId", dataDataServerId}, {"other", dataDataOther}};
                    // add
                    data.Add(dataData);
                }
                return data;
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }
}