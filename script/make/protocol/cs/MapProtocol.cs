public static class MapProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, System.Object data) 
    {
        switch (protocol) 
        {
            case 20001:
            {
                return;
            }
            case 20012:
            {
                // convert
                var dataCast = (System.Collections.Generic.Dictionary<System.String, System.Object>)data;
                // X坐标
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)(System.UInt16)dataCast["x"]));
                // Y坐标
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)(System.UInt16)dataCast["y"]));
                return;
            }
            case 20014:
            {
                // convert
                var dataCast = (System.Collections.Generic.Dictionary<System.String, System.Object>)data;
                // 技能Id
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int32)(System.UInt32)dataCast["skillId"]));
                // 战斗对象ID列表
                var targetList = (System.Collections.Generic.List<System.Object>)dataCast["targetList"];
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)targetList.Count));
                foreach(var targetListItemRaw in targetList)
                {
                    // 战斗对象ID
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)targetListItemRaw));
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
                var mapNo = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // 地图ID
                var mapId = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                // 
                var fighterLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var fighter = new System.Collections.Generic.List<System.Object>(fighterLength);
                while (fighterLength-- > 0)
                {
                    // 
                    // ID
                    var fighterId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 类型
                    var fighterType = reader.ReadByte();
                    // 属性
                    // 战力
                    var fighterAttributeFc = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 血量
                    var fighterAttributeHp = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 健康
                    var fighterAttributeHealth = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // object
                    var fighterAttribute = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"fc", fighterAttributeFc}, {"hp", fighterAttributeHp}, {"health", fighterAttributeHealth}};
                    // 技能列表
                    var fighterSkillLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var fighterSkill = new System.Collections.Generic.List<System.Object>(fighterSkillLength);
                    while (fighterSkillLength-- > 0)
                    {
                        // 
                        // 技能ID
                        var fighterSkillSkillId = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                        // 时间
                        var fighterSkillTime = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                        // 数量
                        var fighterSkillNumber = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                        // object
                        var fighterSkillBattleSkill = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"skillId", fighterSkillSkillId}, {"time", fighterSkillTime}, {"number", fighterSkillNumber}};
                        // add
                        fighterSkill.Add(fighterSkillBattleSkill);
                    }
                    // Buff列表
                    var fighterBuffLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var fighterBuff = new System.Collections.Generic.List<System.Object>(fighterBuffLength);
                    while (fighterBuffLength-- > 0)
                    {
                        // 
                        // BuffID
                        var fighterBuffBuffId = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                        // 过期时间
                        var fighterBuffExpireTime = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                        // 数量
                        var fighterBuffOverlap = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                        // object
                        var fighterBuffBattleBuff = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"buffId", fighterBuffBuffId}, {"expireTime", fighterBuffExpireTime}, {"overlap", fighterBuffOverlap}};
                        // add
                        fighterBuff.Add(fighterBuffBattleBuff);
                    }
                    // X坐标
                    var fighterX = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // Y坐标
                    var fighterY = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // object
                    var fighterFighter = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"id", fighterId}, {"type", fighterType}, {"attribute", fighterAttribute}, {"skill", fighterSkill}, {"buff", fighterBuff}, {"x", fighterX}, {"y", fighterY}};
                    // add
                    fighter.Add(fighterFighter);
                }
                // object
                var map = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"mapNo", mapNo}, {"mapId", mapId}, {"fighter", fighter}};
                return map;
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
                    var id = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 类型
                    var type = reader.ReadByte();
                    // 属性
                    // 战力
                    var attributeFc = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 血量
                    var attributeHp = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 健康
                    var attributeHealth = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // object
                    var attribute = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"fc", attributeFc}, {"hp", attributeHp}, {"health", attributeHealth}};
                    // 技能列表
                    var skillLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var skill = new System.Collections.Generic.List<System.Object>(skillLength);
                    while (skillLength-- > 0)
                    {
                        // 
                        // 技能ID
                        var skillSkillId = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                        // 时间
                        var skillTime = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                        // 数量
                        var skillNumber = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                        // object
                        var skillBattleSkill = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"skillId", skillSkillId}, {"time", skillTime}, {"number", skillNumber}};
                        // add
                        skill.Add(skillBattleSkill);
                    }
                    // Buff列表
                    var buffLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var buff = new System.Collections.Generic.List<System.Object>(buffLength);
                    while (buffLength-- > 0)
                    {
                        // 
                        // BuffID
                        var buffBuffId = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                        // 过期时间
                        var buffExpireTime = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                        // 数量
                        var buffOverlap = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                        // object
                        var buffBattleBuff = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"buffId", buffBuffId}, {"expireTime", buffExpireTime}, {"overlap", buffOverlap}};
                        // add
                        buff.Add(buffBattleBuff);
                    }
                    // X坐标
                    var x = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // Y坐标
                    var y = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // object
                    var fighter = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"id", id}, {"type", type}, {"attribute", attribute}, {"skill", skill}, {"buff", buff}, {"x", x}, {"y", y}};
                    // add
                    data.Add(fighter);
                }
                return data;
            }
            case 20012:
            {
                // 
                // ID
                var id = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // X坐标
                var x = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                // Y坐标
                var y = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                // object
                var fighter = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"id", id}, {"x", x}, {"y", y}};
                return fighter;
            }
            case 20013:
            {
                // 
                // 战斗对象ID
                var id = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // object
                var fighter = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"id", id}};
                return fighter;
            }
            case 20014:
            {
                // 
                // 战斗对象Id
                var fighterId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // 技能Id
                var performSkillId = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                // 
                var fighterListLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var fighterList = new System.Collections.Generic.List<System.Object>(fighterListLength);
                while (fighterListLength-- > 0)
                {
                    // 
                    // ID
                    var fighterListId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 类型
                    var fighterListType = reader.ReadByte();
                    // 属性
                    // 战力
                    var fighterListAttributeFc = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 血量
                    var fighterListAttributeHp = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 健康
                    var fighterListAttributeHealth = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // object
                    var fighterListAttribute = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"fc", fighterListAttributeFc}, {"hp", fighterListAttributeHp}, {"health", fighterListAttributeHealth}};
                    // 技能列表
                    var fighterListSkillLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var fighterListSkill = new System.Collections.Generic.List<System.Object>(fighterListSkillLength);
                    while (fighterListSkillLength-- > 0)
                    {
                        // 
                        // 技能ID
                        var fighterListSkillSkillId = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                        // 时间
                        var fighterListSkillTime = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                        // 数量
                        var fighterListSkillNumber = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                        // object
                        var fighterListSkillBattleSkill = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"skillId", fighterListSkillSkillId}, {"time", fighterListSkillTime}, {"number", fighterListSkillNumber}};
                        // add
                        fighterListSkill.Add(fighterListSkillBattleSkill);
                    }
                    // Buff列表
                    var fighterListBuffLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var fighterListBuff = new System.Collections.Generic.List<System.Object>(fighterListBuffLength);
                    while (fighterListBuffLength-- > 0)
                    {
                        // 
                        // BuffID
                        var fighterListBuffBuffId = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                        // 过期时间
                        var fighterListBuffExpireTime = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                        // 数量
                        var fighterListBuffOverlap = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                        // object
                        var fighterListBuffBattleBuff = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"buffId", fighterListBuffBuffId}, {"expireTime", fighterListBuffExpireTime}, {"overlap", fighterListBuffOverlap}};
                        // add
                        fighterListBuff.Add(fighterListBuffBattleBuff);
                    }
                    // X坐标
                    var fighterListX = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // Y坐标
                    var fighterListY = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // object
                    var fighterListFighter = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"id", fighterListId}, {"type", fighterListType}, {"attribute", fighterListAttribute}, {"skill", fighterListSkill}, {"buff", fighterListBuff}, {"x", fighterListX}, {"y", fighterListY}};
                    // add
                    fighterList.Add(fighterListFighter);
                }
                // object
                var data = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"fighterId", fighterId}, {"performSkillId", performSkillId}, {"fighterList", fighterList}};
                return data;
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }
}