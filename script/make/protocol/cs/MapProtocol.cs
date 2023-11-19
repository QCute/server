public class MapQueryRequest
{
    public System.UInt16 protocol = 20001;
    public Empty data;
}

public class MapQueryResponse
{
    public System.UInt16 protocol = 20001;
    public (
        System.UInt64 mapNo,                                                    // 地图编号
        System.UInt32 mapId,                                                    // 地图ID
        System.Collections.Generic.List<(
            System.UInt64 id,                                                   // ID
            System.Byte type,                                                   // 类型
            (
                System.UInt64 fc,                                               // 战力
                System.UInt64 hp,                                               // 血量
                System.UInt64 health                                            // 健康
            ) attribute,                                                        // 属性
            System.Collections.Generic.List<(
                System.UInt32 skillId,                                          // 技能ID
                System.UInt32 time,                                             // 时间
                System.UInt32 number                                            // 数量
            )> skill,                                                           // 技能列表
            System.Collections.Generic.List<(
                System.UInt32 buffId,                                           // BuffID
                System.UInt32 expireTime,                                       // 过期时间
                System.UInt32 overlap                                           // 数量
            )> buff,                                                            // Buff列表
            System.UInt16 x,                                                    // X坐标
            System.UInt16 y                                                     // Y坐标
        )> fighter                                                              // 
    ) data;
}



public class MapFighterResponse
{
    public System.UInt16 protocol = 20011;
    public System.Collections.Generic.List<(
        System.UInt64 id,                                                       // ID
        System.Byte type,                                                       // 类型
        (
            System.UInt64 fc,                                                   // 战力
            System.UInt64 hp,                                                   // 血量
            System.UInt64 health                                                // 健康
        ) attribute,                                                            // 属性
        System.Collections.Generic.List<(
            System.UInt32 skillId,                                              // 技能ID
            System.UInt32 time,                                                 // 时间
            System.UInt32 number                                                // 数量
        )> skill,                                                               // 技能列表
        System.Collections.Generic.List<(
            System.UInt32 buffId,                                               // BuffID
            System.UInt32 expireTime,                                           // 过期时间
            System.UInt32 overlap                                               // 数量
        )> buff,                                                                // Buff列表
        System.UInt16 x,                                                        // X坐标
        System.UInt16 y                                                         // Y坐标
    )> data;
}

public class MapFighterMoveRequest
{
    public System.UInt16 protocol = 20012;
    public (
        System.UInt16 x,                                                        // X坐标
        System.UInt16 y                                                         // Y坐标
    ) data;
}

public class MapFighterMoveResponse
{
    public System.UInt16 protocol = 20012;
    public (
        System.UInt64 id,                                                       // ID
        System.UInt16 x,                                                        // X坐标
        System.UInt16 y                                                         // Y坐标
    ) data;
}



public class MapFighterLeaveResponse
{
    public System.UInt16 protocol = 20013;
    public (
        System.UInt64 id,                                                       // 战斗对象ID
        Empty _                                                                 // 
    ) data;
}

public class MapAttackRequest
{
    public System.UInt16 protocol = 20014;
    public (
        System.UInt32 skillId,                                                  // 技能Id
        System.Collections.Generic.List<System.UInt64> targetList               // 战斗对象ID列表
    ) data;
}

public class MapAttackResponse
{
    public System.UInt16 protocol = 20014;
    public (
        System.UInt64 fighterId,                                                // 战斗对象Id
        System.UInt32 performSkillId,                                           // 技能Id
        System.Collections.Generic.List<(
            System.UInt64 id,                                                   // ID
            System.Byte type,                                                   // 类型
            (
                System.UInt64 fc,                                               // 战力
                System.UInt64 hp,                                               // 血量
                System.UInt64 health                                            // 健康
            ) attribute,                                                        // 属性
            System.Collections.Generic.List<(
                System.UInt32 skillId,                                          // 技能ID
                System.UInt32 time,                                             // 时间
                System.UInt32 number                                            // 数量
            )> skill,                                                           // 技能列表
            System.Collections.Generic.List<(
                System.UInt32 buffId,                                           // BuffID
                System.UInt32 expireTime,                                       // 过期时间
                System.UInt32 overlap                                           // 数量
            )> buff,                                                            // Buff列表
            System.UInt16 x,                                                    // X坐标
            System.UInt16 y                                                     // Y坐标
        )> fighterList                                                          // 
    ) data;
}

public static class MapProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, System.Object dataRaw) 
    {
        switch (protocol) 
        {
            case 20001:
            {
                #pragma warning disable
                var data = (Empty)dataRaw;
                #pragma warning restore

                return;
            }
            case 20012:
            {
                var data = ((System.UInt16 x, System.UInt16 y))dataRaw;

                // X坐标
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)(System.UInt16)data.x));
                // Y坐标
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)(System.UInt16)data.y));
                return;
            }
            case 20014:
            {
                var data = ((System.UInt32 skillId, System.Collections.Generic.List<System.UInt64> targetList))dataRaw;

                // 技能Id
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int32)(System.UInt32)data.skillId));
                var dataTargetList = data.targetList;
                // 战斗对象ID列表
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)dataTargetList.Count));
                foreach(var dataTargetListData in dataTargetList)
                {
                    // 战斗对象ID
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)dataTargetListData));
                }
                return;
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }

    public static System.Object Decode(System.Text.Encoding encoding, System.IO.BinaryReader reader, System.UInt16 protocol) 
    {
        switch (protocol) 
        {
            case 20001:
            {
                // 
                // 地图编号
                var dataMapNo = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // 地图ID
                var dataMapId = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                // 
                var dataFighterLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var dataFighter = new System.Collections.Generic.List<(System.UInt64 id, System.Byte type, (System.UInt64 fc, System.UInt64 hp, System.UInt64 health) attribute, System.Collections.Generic.List<(System.UInt32 skillId, System.UInt32 time, System.UInt32 number)> skill, System.Collections.Generic.List<(System.UInt32 buffId, System.UInt32 expireTime, System.UInt32 overlap)> buff, System.UInt16 x, System.UInt16 y)>(dataFighterLength);
                while (dataFighterLength-- > 0)
                {
                    // 
                    // ID
                    var dataFighterDataId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 类型
                    var dataFighterDataType = reader.ReadByte();
                    // 属性
                    // 战力
                    var dataFighterDataAttributeFc = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 血量
                    var dataFighterDataAttributeHp = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 健康
                    var dataFighterDataAttributeHealth = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // object
                    var dataFighterDataAttribute = (fc: dataFighterDataAttributeFc, hp: dataFighterDataAttributeHp, health: dataFighterDataAttributeHealth);
                    // 技能列表
                    var dataFighterDataSkillLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var dataFighterDataSkill = new System.Collections.Generic.List<(System.UInt32 skillId, System.UInt32 time, System.UInt32 number)>(dataFighterDataSkillLength);
                    while (dataFighterDataSkillLength-- > 0)
                    {
                        // 
                        // 技能ID
                        var dataFighterDataSkillDataSkillId = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                        // 时间
                        var dataFighterDataSkillDataTime = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                        // 数量
                        var dataFighterDataSkillDataNumber = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                        // object
                        var dataFighterDataSkillData = (skillId: dataFighterDataSkillDataSkillId, time: dataFighterDataSkillDataTime, number: dataFighterDataSkillDataNumber);
                        // add
                        dataFighterDataSkill.Add(dataFighterDataSkillData);
                    }
                    // Buff列表
                    var dataFighterDataBuffLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var dataFighterDataBuff = new System.Collections.Generic.List<(System.UInt32 buffId, System.UInt32 expireTime, System.UInt32 overlap)>(dataFighterDataBuffLength);
                    while (dataFighterDataBuffLength-- > 0)
                    {
                        // 
                        // BuffID
                        var dataFighterDataBuffDataBuffId = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                        // 过期时间
                        var dataFighterDataBuffDataExpireTime = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                        // 数量
                        var dataFighterDataBuffDataOverlap = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                        // object
                        var dataFighterDataBuffData = (buffId: dataFighterDataBuffDataBuffId, expireTime: dataFighterDataBuffDataExpireTime, overlap: dataFighterDataBuffDataOverlap);
                        // add
                        dataFighterDataBuff.Add(dataFighterDataBuffData);
                    }
                    // X坐标
                    var dataFighterDataX = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // Y坐标
                    var dataFighterDataY = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // object
                    var dataFighterData = (id: dataFighterDataId, type: dataFighterDataType, attribute: dataFighterDataAttribute, skill: dataFighterDataSkill, buff: dataFighterDataBuff, x: dataFighterDataX, y: dataFighterDataY);
                    // add
                    dataFighter.Add(dataFighterData);
                }
                // object
                var data = (mapNo: dataMapNo, mapId: dataMapId, fighter: dataFighter);
                return (protocol: 20001, data: data);
            }
            case 20011:
            {
                // 
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<(System.UInt64 id, System.Byte type, (System.UInt64 fc, System.UInt64 hp, System.UInt64 health) attribute, System.Collections.Generic.List<(System.UInt32 skillId, System.UInt32 time, System.UInt32 number)> skill, System.Collections.Generic.List<(System.UInt32 buffId, System.UInt32 expireTime, System.UInt32 overlap)> buff, System.UInt16 x, System.UInt16 y)>(dataLength);
                while (dataLength-- > 0)
                {
                    // 
                    // ID
                    var dataDataId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 类型
                    var dataDataType = reader.ReadByte();
                    // 属性
                    // 战力
                    var dataDataAttributeFc = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 血量
                    var dataDataAttributeHp = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 健康
                    var dataDataAttributeHealth = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // object
                    var dataDataAttribute = (fc: dataDataAttributeFc, hp: dataDataAttributeHp, health: dataDataAttributeHealth);
                    // 技能列表
                    var dataDataSkillLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var dataDataSkill = new System.Collections.Generic.List<(System.UInt32 skillId, System.UInt32 time, System.UInt32 number)>(dataDataSkillLength);
                    while (dataDataSkillLength-- > 0)
                    {
                        // 
                        // 技能ID
                        var dataDataSkillDataSkillId = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                        // 时间
                        var dataDataSkillDataTime = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                        // 数量
                        var dataDataSkillDataNumber = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                        // object
                        var dataDataSkillData = (skillId: dataDataSkillDataSkillId, time: dataDataSkillDataTime, number: dataDataSkillDataNumber);
                        // add
                        dataDataSkill.Add(dataDataSkillData);
                    }
                    // Buff列表
                    var dataDataBuffLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var dataDataBuff = new System.Collections.Generic.List<(System.UInt32 buffId, System.UInt32 expireTime, System.UInt32 overlap)>(dataDataBuffLength);
                    while (dataDataBuffLength-- > 0)
                    {
                        // 
                        // BuffID
                        var dataDataBuffDataBuffId = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                        // 过期时间
                        var dataDataBuffDataExpireTime = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                        // 数量
                        var dataDataBuffDataOverlap = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                        // object
                        var dataDataBuffData = (buffId: dataDataBuffDataBuffId, expireTime: dataDataBuffDataExpireTime, overlap: dataDataBuffDataOverlap);
                        // add
                        dataDataBuff.Add(dataDataBuffData);
                    }
                    // X坐标
                    var dataDataX = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // Y坐标
                    var dataDataY = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // object
                    var dataData = (id: dataDataId, type: dataDataType, attribute: dataDataAttribute, skill: dataDataSkill, buff: dataDataBuff, x: dataDataX, y: dataDataY);
                    // add
                    data.Add(dataData);
                }
                return (protocol: 20011, data: data);
            }
            case 20012:
            {
                // 
                // ID
                var dataId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // X坐标
                var dataX = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                // Y坐标
                var dataY = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                // object
                var data = (id: dataId, x: dataX, y: dataY);
                return (protocol: 20012, data: data);
            }
            case 20013:
            {
                // 
                // 战斗对象ID
                var dataId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // object
                var data = (id: dataId, _: new Empty());
                return (protocol: 20013, data: data);
            }
            case 20014:
            {
                // 
                // 战斗对象Id
                var dataFighterId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // 技能Id
                var dataPerformSkillId = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                // 
                var dataFighterListLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var dataFighterList = new System.Collections.Generic.List<(System.UInt64 id, System.Byte type, (System.UInt64 fc, System.UInt64 hp, System.UInt64 health) attribute, System.Collections.Generic.List<(System.UInt32 skillId, System.UInt32 time, System.UInt32 number)> skill, System.Collections.Generic.List<(System.UInt32 buffId, System.UInt32 expireTime, System.UInt32 overlap)> buff, System.UInt16 x, System.UInt16 y)>(dataFighterListLength);
                while (dataFighterListLength-- > 0)
                {
                    // 
                    // ID
                    var dataFighterListDataId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 类型
                    var dataFighterListDataType = reader.ReadByte();
                    // 属性
                    // 战力
                    var dataFighterListDataAttributeFc = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 血量
                    var dataFighterListDataAttributeHp = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 健康
                    var dataFighterListDataAttributeHealth = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // object
                    var dataFighterListDataAttribute = (fc: dataFighterListDataAttributeFc, hp: dataFighterListDataAttributeHp, health: dataFighterListDataAttributeHealth);
                    // 技能列表
                    var dataFighterListDataSkillLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var dataFighterListDataSkill = new System.Collections.Generic.List<(System.UInt32 skillId, System.UInt32 time, System.UInt32 number)>(dataFighterListDataSkillLength);
                    while (dataFighterListDataSkillLength-- > 0)
                    {
                        // 
                        // 技能ID
                        var dataFighterListDataSkillDataSkillId = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                        // 时间
                        var dataFighterListDataSkillDataTime = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                        // 数量
                        var dataFighterListDataSkillDataNumber = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                        // object
                        var dataFighterListDataSkillData = (skillId: dataFighterListDataSkillDataSkillId, time: dataFighterListDataSkillDataTime, number: dataFighterListDataSkillDataNumber);
                        // add
                        dataFighterListDataSkill.Add(dataFighterListDataSkillData);
                    }
                    // Buff列表
                    var dataFighterListDataBuffLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var dataFighterListDataBuff = new System.Collections.Generic.List<(System.UInt32 buffId, System.UInt32 expireTime, System.UInt32 overlap)>(dataFighterListDataBuffLength);
                    while (dataFighterListDataBuffLength-- > 0)
                    {
                        // 
                        // BuffID
                        var dataFighterListDataBuffDataBuffId = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                        // 过期时间
                        var dataFighterListDataBuffDataExpireTime = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                        // 数量
                        var dataFighterListDataBuffDataOverlap = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                        // object
                        var dataFighterListDataBuffData = (buffId: dataFighterListDataBuffDataBuffId, expireTime: dataFighterListDataBuffDataExpireTime, overlap: dataFighterListDataBuffDataOverlap);
                        // add
                        dataFighterListDataBuff.Add(dataFighterListDataBuffData);
                    }
                    // X坐标
                    var dataFighterListDataX = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // Y坐标
                    var dataFighterListDataY = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // object
                    var dataFighterListData = (id: dataFighterListDataId, type: dataFighterListDataType, attribute: dataFighterListDataAttribute, skill: dataFighterListDataSkill, buff: dataFighterListDataBuff, x: dataFighterListDataX, y: dataFighterListDataY);
                    // add
                    dataFighterList.Add(dataFighterListData);
                }
                // object
                var data = (fighterId: dataFighterId, performSkillId: dataPerformSkillId, fighterList: dataFighterList);
                return (protocol: 20014, data: data);
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }
}