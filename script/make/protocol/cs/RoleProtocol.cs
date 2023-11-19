public static class RoleProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, dynamic data) 
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
                var dataRoleId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // 角色名
                var dataRoleNameLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var dataRoleName = encoding.GetString(reader.ReadBytes(dataRoleNameLength));
                // 性别
                var dataSex = reader.ReadByte();
                // 职业
                var dataClasses = reader.ReadByte();
                // 等级
                var dataLevel = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // object
                var data = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"roleId", dataRoleId}, {"roleName", dataRoleName}, {"sex", dataSex}, {"classes", dataClasses}, {"level", dataLevel}};
                return data;
            }
            case 10102:
            {
                // 
                // 金币
                var dataGold = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // 银币
                var dataSilver = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                // 铜币
                var dataCopper = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // 经验
                var dataExp = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // object
                var data = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"gold", dataGold}, {"silver", dataSilver}, {"copper", dataCopper}, {"exp", dataExp}};
                return data;
            }
            case 10103:
            {
                // 
                // 等级
                var dataVipLevel = reader.ReadByte();
                // 经验
                var dataExp = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // 过期时间
                var dataExpireTime = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                // object
                var data = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"vipLevel", dataVipLevel}, {"exp", dataExp}, {"expireTime", dataExpireTime}};
                return data;
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }
}