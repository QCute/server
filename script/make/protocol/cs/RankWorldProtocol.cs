public class RankWorldLevelRequest
{
    public System.UInt16 protocol = 19201;
    public Empty data;
}

public class RankWorldLevelResponse
{
    public System.UInt16 protocol = 19201;
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

public class RankWorldFightRequest
{
    public System.UInt16 protocol = 19202;
    public Empty data;
}

public class RankWorldFightResponse
{
    public System.UInt16 protocol = 19202;
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

public class RankWorldAchievementRequest
{
    public System.UInt16 protocol = 19203;
    public Empty data;
}

public class RankWorldAchievementResponse
{
    public System.UInt16 protocol = 19203;
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

public class RankWorldWealthRequest
{
    public System.UInt16 protocol = 19204;
    public Empty data;
}

public class RankWorldWealthResponse
{
    public System.UInt16 protocol = 19204;
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

public class RankWorldClassesRequest
{
    public System.UInt16 protocol = 19205;
    public Empty data;
}

public class RankWorldClassesResponse
{
    public System.UInt16 protocol = 19205;
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

public static class RankWorldProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, System.Object dataRaw) 
    {
        switch (protocol) 
        {
            case 19201:
            {
                #pragma warning disable
                var data = (Empty)dataRaw;
                #pragma warning restore

                return;
            }
            case 19202:
            {
                #pragma warning disable
                var data = (Empty)dataRaw;
                #pragma warning restore

                return;
            }
            case 19203:
            {
                #pragma warning disable
                var data = (Empty)dataRaw;
                #pragma warning restore

                return;
            }
            case 19204:
            {
                #pragma warning disable
                var data = (Empty)dataRaw;
                #pragma warning restore

                return;
            }
            case 19205:
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
            case 19201:
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
                return (protocol: 19201, data: data);
            }
            case 19202:
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
                return (protocol: 19202, data: data);
            }
            case 19203:
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
                return (protocol: 19203, data: data);
            }
            case 19204:
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
                return (protocol: 19204, data: data);
            }
            case 19205:
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
                return (protocol: 19205, data: data);
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }
}