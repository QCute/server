public static class MapProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, dynamic data) 
    {
        switch (protocol) 
        {
            case 20001:
            {

                return;
            }
            case 20012:
            {

                // X坐标
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)(System.UInt16)data["x"]));
                // Y坐标
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)(System.UInt16)data["y"]));
                return;
            }
            case 20014:
            {

                // 技能Id
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int32)(System.UInt32)data["skillId"]));
                var dataTargetList = data["targetList"];
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
                var dataFighter = new System.Collections.Generic.List<System.Object>(dataFighterLength);
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
                    var dataFighterDataAttribute = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"fc", dataFighterDataAttributeFc}, {"hp", dataFighterDataAttributeHp}, {"health", dataFighterDataAttributeHealth}};
                    // 技能列表
                    var dataFighterDataSkillLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var dataFighterDataSkill = new System.Collections.Generic.List<System.Object>(dataFighterDataSkillLength);
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
                        var dataFighterDataSkillData = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"skillId", dataFighterDataSkillDataSkillId}, {"time", dataFighterDataSkillDataTime}, {"number", dataFighterDataSkillDataNumber}};
                        // add
                        dataFighterDataSkill.Add(dataFighterDataSkillData);
                    }
                    // Buff列表
                    var dataFighterDataBuffLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var dataFighterDataBuff = new System.Collections.Generic.List<System.Object>(dataFighterDataBuffLength);
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
                        var dataFighterDataBuffData = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"buffId", dataFighterDataBuffDataBuffId}, {"expireTime", dataFighterDataBuffDataExpireTime}, {"overlap", dataFighterDataBuffDataOverlap}};
                        // add
                        dataFighterDataBuff.Add(dataFighterDataBuffData);
                    }
                    // X坐标
                    var dataFighterDataX = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // Y坐标
                    var dataFighterDataY = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // object
                    var dataFighterData = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"id", dataFighterDataId}, {"type", dataFighterDataType}, {"attribute", dataFighterDataAttribute}, {"skill", dataFighterDataSkill}, {"buff", dataFighterDataBuff}, {"x", dataFighterDataX}, {"y", dataFighterDataY}};
                    // add
                    dataFighter.Add(dataFighterData);
                }
                // object
                var data = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"mapNo", dataMapNo}, {"mapId", dataMapId}, {"fighter", dataFighter}};
                return data;
            }
            case 20011:
            {
                // 
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<System.Object>(dataLength);
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
                    var dataDataAttribute = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"fc", dataDataAttributeFc}, {"hp", dataDataAttributeHp}, {"health", dataDataAttributeHealth}};
                    // 技能列表
                    var dataDataSkillLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var dataDataSkill = new System.Collections.Generic.List<System.Object>(dataDataSkillLength);
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
                        var dataDataSkillData = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"skillId", dataDataSkillDataSkillId}, {"time", dataDataSkillDataTime}, {"number", dataDataSkillDataNumber}};
                        // add
                        dataDataSkill.Add(dataDataSkillData);
                    }
                    // Buff列表
                    var dataDataBuffLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var dataDataBuff = new System.Collections.Generic.List<System.Object>(dataDataBuffLength);
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
                        var dataDataBuffData = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"buffId", dataDataBuffDataBuffId}, {"expireTime", dataDataBuffDataExpireTime}, {"overlap", dataDataBuffDataOverlap}};
                        // add
                        dataDataBuff.Add(dataDataBuffData);
                    }
                    // X坐标
                    var dataDataX = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // Y坐标
                    var dataDataY = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // object
                    var dataData = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"id", dataDataId}, {"type", dataDataType}, {"attribute", dataDataAttribute}, {"skill", dataDataSkill}, {"buff", dataDataBuff}, {"x", dataDataX}, {"y", dataDataY}};
                    // add
                    data.Add(dataData);
                }
                return data;
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
                var data = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"id", dataId}, {"x", dataX}, {"y", dataY}};
                return data;
            }
            case 20013:
            {
                // 
                // 战斗对象ID
                var dataId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // object
                var data = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"id", dataId}};
                return data;
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
                var dataFighterList = new System.Collections.Generic.List<System.Object>(dataFighterListLength);
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
                    var dataFighterListDataAttribute = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"fc", dataFighterListDataAttributeFc}, {"hp", dataFighterListDataAttributeHp}, {"health", dataFighterListDataAttributeHealth}};
                    // 技能列表
                    var dataFighterListDataSkillLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var dataFighterListDataSkill = new System.Collections.Generic.List<System.Object>(dataFighterListDataSkillLength);
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
                        var dataFighterListDataSkillData = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"skillId", dataFighterListDataSkillDataSkillId}, {"time", dataFighterListDataSkillDataTime}, {"number", dataFighterListDataSkillDataNumber}};
                        // add
                        dataFighterListDataSkill.Add(dataFighterListDataSkillData);
                    }
                    // Buff列表
                    var dataFighterListDataBuffLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var dataFighterListDataBuff = new System.Collections.Generic.List<System.Object>(dataFighterListDataBuffLength);
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
                        var dataFighterListDataBuffData = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"buffId", dataFighterListDataBuffDataBuffId}, {"expireTime", dataFighterListDataBuffDataExpireTime}, {"overlap", dataFighterListDataBuffDataOverlap}};
                        // add
                        dataFighterListDataBuff.Add(dataFighterListDataBuffData);
                    }
                    // X坐标
                    var dataFighterListDataX = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // Y坐标
                    var dataFighterListDataY = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // object
                    var dataFighterListData = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"id", dataFighterListDataId}, {"type", dataFighterListDataType}, {"attribute", dataFighterListDataAttribute}, {"skill", dataFighterListDataSkill}, {"buff", dataFighterListDataBuff}, {"x", dataFighterListDataX}, {"y", dataFighterListDataY}};
                    // add
                    dataFighterList.Add(dataFighterListData);
                }
                // object
                var data = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"fighterId", dataFighterId}, {"performSkillId", dataPerformSkillId}, {"fighterList", dataFighterList}};
                return data;
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }
}