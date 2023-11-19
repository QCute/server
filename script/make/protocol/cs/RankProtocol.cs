public class RankLevelRequest
{
    public System.UInt16 protocol = 19001;
    public Empty data;
}

public class RankLevelResponse
{
    public System.UInt16 protocol = 19001;
    public System.Collections.Generic.List<(
        System.UInt16 type,                                                     // 类型
        System.UInt64 order,                                                    // 排名
        System.UInt64 key,                                                      // 键
        System.UInt64 value,                                                    // 值
        System.UInt32 time,                                                     // 时间
        System.String name,                                                     // 名字
        System.UInt16 serverId                                                  // 服务器ID
    )> data;
}

public class RankFightRequest
{
    public System.UInt16 protocol = 19002;
    public Empty data;
}

public class RankFightResponse
{
    public System.UInt16 protocol = 19002;
    public System.Collections.Generic.List<(
        System.UInt16 type,                                                     // 类型
        System.UInt64 order,                                                    // 排名
        System.UInt64 key,                                                      // 键
        System.UInt64 value,                                                    // 值
        System.UInt32 time,                                                     // 时间
        System.String name,                                                     // 名字
        System.UInt16 serverId,                                                 // 服务器ID
        (
            System.UInt16 level,                                                // 等级
            System.Byte classes                                                 // 职业
        ) other                                                                 // 
    )> data;
}

public class RankAchievementRequest
{
    public System.UInt16 protocol = 19003;
    public Empty data;
}

public class RankAchievementResponse
{
    public System.UInt16 protocol = 19003;
    public System.Collections.Generic.List<(
        System.UInt16 type,                                                     // 类型
        System.UInt64 order,                                                    // 排名
        System.UInt64 key,                                                      // 键
        System.UInt64 value,                                                    // 值
        System.UInt32 time,                                                     // 时间
        System.String name,                                                     // 名字
        System.UInt16 serverId,                                                 // 服务器ID
        (
            System.UInt16 level,                                                // 等级
            System.Byte classes,                                                // 职业
            System.Byte sex                                                     // 性别
        ) other                                                                 // 
    )> data;
}

public class RankWealthRequest
{
    public System.UInt16 protocol = 19004;
    public Empty data;
}

public class RankWealthResponse
{
    public System.UInt16 protocol = 19004;
    public System.Collections.Generic.List<(
        System.UInt16 type,                                                     // 类型
        System.UInt64 order,                                                    // 排名
        System.UInt64 key,                                                      // 键
        System.UInt64 value,                                                    // 值
        System.UInt32 time,                                                     // 时间
        System.String name,                                                     // 名字
        System.UInt16 serverId,                                                 // 服务器ID
        (
            System.UInt16 level,                                                // 等级
            System.Byte classes,                                                // 职业
            System.Byte sex,                                                    // 性别
            System.Byte vipLevel                                                // VIP等级
        ) other                                                                 // 
    )> data;
}

public class RankClassesRequest
{
    public System.UInt16 protocol = 19005;
    public Empty data;
}

public class RankClassesResponse
{
    public System.UInt16 protocol = 19005;
    public System.Collections.Generic.List<(
        System.UInt16 type,                                                     // 类型
        System.UInt64 order,                                                    // 排名
        System.UInt64 key,                                                      // 键
        System.UInt64 value,                                                    // 值
        System.UInt32 time,                                                     // 时间
        System.String name,                                                     // 名字
        System.UInt16 serverId,                                                 // 服务器ID
        (
            System.UInt16 level,                                                // 等级
            System.Byte classes,                                                // 职业
            System.Byte sex,                                                    // 性别
            System.Byte vipLevel,                                               // VIP等级
            System.Byte avatar                                                  // 头像
        ) other                                                                 // 
    )> data;
}

public static class RankProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, System.Object dataRaw) 
    {
        switch (protocol) 
        {
            case 19001:
            {
                #pragma warning disable
                var data = (Empty)dataRaw;
                #pragma warning restore

                return;
            }
            case 19002:
            {
                #pragma warning disable
                var data = (Empty)dataRaw;
                #pragma warning restore

                return;
            }
            case 19003:
            {
                #pragma warning disable
                var data = (Empty)dataRaw;
                #pragma warning restore

                return;
            }
            case 19004:
            {
                #pragma warning disable
                var data = (Empty)dataRaw;
                #pragma warning restore

                return;
            }
            case 19005:
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
            case 19001:
            {
                // 
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<(System.UInt16 type, System.UInt64 order, System.UInt64 key, System.UInt64 value, System.UInt32 time, System.String name, System.UInt16 serverId)>(dataLength);
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
                    var dataData = (type: dataDataType, order: dataDataOrder, key: dataDataKey, value: dataDataValue, time: dataDataTime, name: dataDataName, serverId: dataDataServerId);
                    // add
                    data.Add(dataData);
                }
                return (protocol: 19001, data: data);
            }
            case 19002:
            {
                // 
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<(System.UInt16 type, System.UInt64 order, System.UInt64 key, System.UInt64 value, System.UInt32 time, System.String name, System.UInt16 serverId, (System.UInt16 level, System.Byte classes) other)>(dataLength);
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
                    var dataDataOther = (level: dataDataOtherLevel, classes: dataDataOtherClasses);
                    // object
                    var dataData = (type: dataDataType, order: dataDataOrder, key: dataDataKey, value: dataDataValue, time: dataDataTime, name: dataDataName, serverId: dataDataServerId, other: dataDataOther);
                    // add
                    data.Add(dataData);
                }
                return (protocol: 19002, data: data);
            }
            case 19003:
            {
                // 
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<(System.UInt16 type, System.UInt64 order, System.UInt64 key, System.UInt64 value, System.UInt32 time, System.String name, System.UInt16 serverId, (System.UInt16 level, System.Byte classes, System.Byte sex) other)>(dataLength);
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
                    var dataDataOther = (level: dataDataOtherLevel, classes: dataDataOtherClasses, sex: dataDataOtherSex);
                    // object
                    var dataData = (type: dataDataType, order: dataDataOrder, key: dataDataKey, value: dataDataValue, time: dataDataTime, name: dataDataName, serverId: dataDataServerId, other: dataDataOther);
                    // add
                    data.Add(dataData);
                }
                return (protocol: 19003, data: data);
            }
            case 19004:
            {
                // 
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<(System.UInt16 type, System.UInt64 order, System.UInt64 key, System.UInt64 value, System.UInt32 time, System.String name, System.UInt16 serverId, (System.UInt16 level, System.Byte classes, System.Byte sex, System.Byte vipLevel) other)>(dataLength);
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
                    var dataDataOther = (level: dataDataOtherLevel, classes: dataDataOtherClasses, sex: dataDataOtherSex, vipLevel: dataDataOtherVipLevel);
                    // object
                    var dataData = (type: dataDataType, order: dataDataOrder, key: dataDataKey, value: dataDataValue, time: dataDataTime, name: dataDataName, serverId: dataDataServerId, other: dataDataOther);
                    // add
                    data.Add(dataData);
                }
                return (protocol: 19004, data: data);
            }
            case 19005:
            {
                // 
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<(System.UInt16 type, System.UInt64 order, System.UInt64 key, System.UInt64 value, System.UInt32 time, System.String name, System.UInt16 serverId, (System.UInt16 level, System.Byte classes, System.Byte sex, System.Byte vipLevel, System.Byte avatar) other)>(dataLength);
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
                    var dataDataOther = (level: dataDataOtherLevel, classes: dataDataOtherClasses, sex: dataDataOtherSex, vipLevel: dataDataOtherVipLevel, avatar: dataDataOtherAvatar);
                    // object
                    var dataData = (type: dataDataType, order: dataDataOrder, key: dataDataKey, value: dataDataValue, time: dataDataTime, name: dataDataName, serverId: dataDataServerId, other: dataDataOther);
                    // add
                    data.Add(dataData);
                }
                return (protocol: 19005, data: data);
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }
}