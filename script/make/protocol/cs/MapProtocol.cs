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
            case 20011:
            {
                return;
            }
            case 20012:
            {
                // x坐标
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)(System.UInt16)data["x"]));
                // y坐标
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)(System.UInt16)data["y"]));
                return;
            }
            case 20014:
            {
                // 技能Id
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int32)(System.UInt32)data["skillId"]));
                // 对象列表
                var targetListData = (System.Collections.Generic.List<System.Object>)data["targetList"];
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)targetListData.Count));
                foreach(System.UInt64 targetListDataItem in targetListData)
                {
                    // ID
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
                return new System.Collections.Generic.Dictionary<System.String, System.Object>() {};
            }
            case 20011:
            {
                // 对象列表
                var listLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var list = new System.Collections.Generic.List<System.Object>(listLength);
                while (listLength-- > 0)
                {
                    // Fighter
                    // ID
                    var id = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 类型
                    var type = reader.ReadByte();
                    // 技能列表
                    var skillLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var skill = new System.Collections.Generic.List<System.Object>(skillLength);
                    while (skillLength-- > 0)
                    {
                        // BattleSkill
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
                        // BattleBuff
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
                    var fighter = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"id", id}, {"type", type}, {"skill", skill}, {"buff", buff}, {"x", x}, {"y", y}};
                    // add
                    list.Add(fighter);
                }
                return new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"list", list}};
            }
            case 20012:
            {
                // Fighter
                // ID
                var id = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // X坐标
                var x = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                // Y坐标
                var y = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                // object
                var fighter = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"id", id}, {"x", x}, {"y", y}};
                return new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"fighter", fighter}};
            }
            case 20013:
            {
                // Fighter
                // ID
                var id = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // object
                var fighter = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"id", id}};
                return new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"fighter", fighter}};
            }
            case 20014:
            {
                // 战斗对象Id
                var fighterId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // 技能Id
                var performSkillId = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                // 对象列表
                var listLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var list = new System.Collections.Generic.List<System.Object>(listLength);
                while (listLength-- > 0)
                {
                    // Fighter
                    // ID
                    var id = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 类型
                    var type = reader.ReadByte();
                    // 技能列表
                    var skillLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var skill = new System.Collections.Generic.List<System.Object>(skillLength);
                    while (skillLength-- > 0)
                    {
                        // BattleSkill
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
                        // BattleBuff
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
                    var fighter = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"id", id}, {"type", type}, {"skill", skill}, {"buff", buff}, {"x", x}, {"y", y}};
                    // add
                    list.Add(fighter);
                }
                return new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"fighterId", fighterId}, {"performSkillId", performSkillId}, {"list", list}};
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }
}