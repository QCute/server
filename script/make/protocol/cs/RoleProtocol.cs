public static class RoleProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, System.Object data) 
    {
        switch (protocol) 
        {
            case 10101:
            {
                return;
            }
            case 10102:
            {
                return;
            }
            case 10103:
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
            case 10101:
            {
                // 
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
                // object
                var role = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"roleId", roleId}, {"roleName", roleName}, {"sex", sex}, {"classes", classes}, {"level", level}};
                return role;
            }
            case 10102:
            {
                // 
                // 金币
                var gold = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // 银币
                var silver = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                // 铜币
                var copper = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // 经验
                var exp = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // object
                var asset = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"gold", gold}, {"silver", silver}, {"copper", copper}, {"exp", exp}};
                return asset;
            }
            case 10103:
            {
                // 
                // 等级
                var vipLevel = reader.ReadByte();
                // 经验
                var exp = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // 过期时间
                var expireTime = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                // object
                var vip = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"vipLevel", vipLevel}, {"exp", exp}, {"expireTime", expireTime}};
                return vip;
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }
}