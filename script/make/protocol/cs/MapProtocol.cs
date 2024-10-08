public static class MapProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, System.Collections.Generic.Dictionary<System.String, System.Object> data) 
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
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)(System.UInt16)((System.Collections.Generic.Dictionary<System.String, System.Object>)data["data"])["x"]));
                // Y坐标
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)(System.UInt16)((System.Collections.Generic.Dictionary<System.String, System.Object>)data["data"])["y"]));
                return;
            }
            case 20014:
            {
                // 技能Id
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int32)(System.UInt32)((System.Collections.Generic.Dictionary<System.String, System.Object>)data["data"])["skillId"]));
                // 战斗对象ID列表
                var targetListData = (System.Collections.Generic.List<System.Object>)((System.Collections.Generic.Dictionary<System.String, System.Object>)data["data"])["targetList"];
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)targetListData.Count));
                foreach(System.UInt64 targetListDataItem in targetListData)
                {
                    // 战斗对象ID
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)targetListDataItem));
                }
                return;
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }

    public static System.Collections.Generic.Dictionary<System.String, System.Object> Decode(System.Text.Encoding encoding, System.IO.BinaryReader reader, System.UInt16 protocol) 
    {
        switch (protocol) 
        {
            case 20001:
            {
                // 

                // object
                var data = new System.Collections.Generic.Dictionary<System.String, System.Object>() {};
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
                    var id = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 类型
                    var type = reader.ReadByte();
                    // 属性
                    // 战力
                    var fc = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 血量
                    var hp = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 健康
                    var health = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // object
                    var attribute = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"fc", fc}, {"hp", hp}, {"health", health}};
                    // 技能列表
                    var skillLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var skill = new System.Collections.Generic.List<System.Object>(skillLength);
                    while (skillLength-- > 0)
                    {
                        // 
                        // 技能ID
                        var skillId = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                        // 时间
                        var time = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                        // 数量
                        var number = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                        // object
                        var battleSkill = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"skillId", skillId}, {"time", time}, {"number", number}};
                        // add
                        skill.Add(battleSkill);
                    }
                    // Buff列表
                    var buffLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var buff = new System.Collections.Generic.List<System.Object>(buffLength);
                    while (buffLength-- > 0)
                    {
                        // 
                        // BuffID
                        var buffId = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                        // 过期时间
                        var expireTime = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                        // 数量
                        var overlap = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                        // object
                        var battleBuff = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"buffId", buffId}, {"expireTime", expireTime}, {"overlap", overlap}};
                        // add
                        buff.Add(battleBuff);
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
                    var id = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 类型
                    var type = reader.ReadByte();
                    // 属性
                    // 战力
                    var fc = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 血量
                    var hp = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 健康
                    var health = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // object
                    var attribute = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"fc", fc}, {"hp", hp}, {"health", health}};
                    // 技能列表
                    var skillLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var skill = new System.Collections.Generic.List<System.Object>(skillLength);
                    while (skillLength-- > 0)
                    {
                        // 
                        // 技能ID
                        var skillId = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                        // 时间
                        var time = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                        // 数量
                        var number = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                        // object
                        var battleSkill = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"skillId", skillId}, {"time", time}, {"number", number}};
                        // add
                        skill.Add(battleSkill);
                    }
                    // Buff列表
                    var buffLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var buff = new System.Collections.Generic.List<System.Object>(buffLength);
                    while (buffLength-- > 0)
                    {
                        // 
                        // BuffID
                        var buffId = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                        // 过期时间
                        var expireTime = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                        // 数量
                        var overlap = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                        // object
                        var battleBuff = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"buffId", buffId}, {"expireTime", expireTime}, {"overlap", overlap}};
                        // add
                        buff.Add(battleBuff);
                    }
                    // X坐标
                    var x = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // Y坐标
                    var y = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // object
                    var fighter = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"id", id}, {"type", type}, {"attribute", attribute}, {"skill", skill}, {"buff", buff}, {"x", x}, {"y", y}};
                    // add
                    fighterList.Add(fighter);
                }
                // object
                var data = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"fighterId", fighterId}, {"performSkillId", performSkillId}, {"fighterList", fighterList}};
                return data;
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }
}