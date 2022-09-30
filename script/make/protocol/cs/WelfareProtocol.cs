public static class WelfareProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, System.Collections.Generic.Dictionary<System.String, System.Object> data) 
    {
        switch (protocol) 
        {
            case 15002:
            {
                // 兑换码
                var keyBytes = encoding.GetBytes((System.String)data["key"]);
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)keyBytes.Length));
                writer.Write(keyBytes);
                return;
            }
            case 15003:
            {
                // 红包编号
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)data["luckyMoneyNo"]));
                return;
            }
            case 15004:
            {
                // 红包编号
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)data["luckyMoneyNo"]));
                return;
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }

    public static System.Collections.Generic.Dictionary<System.String, System.Object> Decode(System.Text.Encoding encoding, System.IO.BinaryReader reader, System.UInt16 protocol) 
    {
        switch (protocol) 
        {
            case 15001:
            {
                // 结果
                var resultLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var result = encoding.GetString(reader.ReadBytes(resultLength));
                return new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"result", result}};
            }
            case 15002:
            {
                // 结果
                var resultLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var result = encoding.GetString(reader.ReadBytes(resultLength));
                return new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"result", result}};
            }
            case 15003:
            {
                // 红包编号
                var luckyMoneyNo = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // 总金币
                var totalGold = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // 总数量
                var totalNumber = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                // 已经领取人数
                var receiveNumber = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                // 领取列表
                var receiveListLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var receiveList = new System.Collections.ArrayList(receiveListLength);
                while (receiveListLength-- > 0)
                {
                    // 服务器Id
                    var serverId = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // 角色Id
                    var roleId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 角色名
                    var roleNameLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var roleName = encoding.GetString(reader.ReadBytes(roleNameLength));
                    // 金币
                    var gold = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 领取时间
                    var receiveTime = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // add
                    receiveList.Add(new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"serverId", serverId}, {"roleId", roleId}, {"roleName", roleName}, {"gold", gold}, {"receiveTime", receiveTime}});
                }
                // 发送时间
                var sendTime = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                return new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"luckyMoneyNo", luckyMoneyNo}, {"totalGold", totalGold}, {"totalNumber", totalNumber}, {"receiveNumber", receiveNumber}, {"receiveList", receiveList}, {"sendTime", sendTime}};
            }
            case 15004:
            {
                // 结果
                var resultLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var result = encoding.GetString(reader.ReadBytes(resultLength));
                // 金币
                var gold = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                return new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"result", result}, {"gold", gold}};
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }
}