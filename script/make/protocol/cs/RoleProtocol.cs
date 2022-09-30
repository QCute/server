public static class RoleProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, System.Collections.Generic.Dictionary<System.String, System.Object> data) 
    {
        switch (protocol) 
        {

            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }

    public static System.Collections.Generic.Dictionary<System.String, System.Object> Decode(System.Text.Encoding encoding, System.IO.BinaryReader reader, System.UInt16 protocol) 
    {
        switch (protocol) 
        {
            case 10101:
            {
                // 角色ID
                var roleId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // 角色名
                var roleNameLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var roleName = encoding.GetString(reader.ReadBytes(roleNameLength));
                // 性别
                var sex = reader.ReadByte();
                // 职业
                var classes = reader.ReadByte();
                // 等级
                var level = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // 普通背包大小
                var itemSize = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                // 装备背包大小
                var bagSize = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                // 仓库背包大小
                var storeSize = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                return new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"roleId", roleId}, {"roleName", roleName}, {"sex", sex}, {"classes", classes}, {"level", level}, {"itemSize", itemSize}, {"bagSize", bagSize}, {"storeSize", storeSize}};
            }
            case 10102:
            {
                // 金币
                var gold = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // 银币
                var silver = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                // 铜币
                var copper = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // 经验
                var exp = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                return new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"gold", gold}, {"silver", silver}, {"copper", copper}, {"exp", exp}};
            }
            case 10103:
            {
                // 等级
                var vipLevel = reader.ReadByte();
                // 经验
                var exp = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // 过期时间
                var expireTime = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                return new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"vipLevel", vipLevel}, {"exp", exp}, {"expireTime", expireTime}};
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }
}