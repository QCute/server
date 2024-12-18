public static class WelfareProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, System.Object data) 
    {
        switch (protocol) 
        {
            case 15001:
            {
                return;
            }
            case 15002:
            {
                // 兑换码
                var dataBytes = encoding.GetBytes((System.String)data);
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)dataBytes.Length));
                writer.Write(dataBytes);
                return;
            }
            case 15003:
            {
                // 红包编号
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)data));
                return;
            }
            case 15004:
            {
                // 红包编号
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)data));
                return;
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }

    public static System.Object Decode(System.Text.Encoding encoding, System.IO.BinaryReader reader, System.UInt16 protocol) 
    {
        switch (protocol) 
        {
            case 15001:
            {
                // 结果
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = encoding.GetString(reader.ReadBytes(dataLength));
                return data;
            }
            case 15002:
            {
                // 结果
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = encoding.GetString(reader.ReadBytes(dataLength));
                return data;
            }
            case 15003:
            {
                // 
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
                var receiveList = new System.Collections.Generic.List<System.Object>(receiveListLength);
                while (receiveListLength-- > 0)
                {
                    // 
                    // 服务器Id
                    var receiveListServerId = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // 角色Id
                    var receiveListRoleId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 角色名
                    var receiveListRoleNameLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var receiveListRoleName = encoding.GetString(reader.ReadBytes(receiveListRoleNameLength));
                    // 金币
                    var receiveListGold = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 领取时间
                    var receiveListTime = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // object
                    var receiveListLuckyMoneyRole = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"serverId", receiveListServerId}, {"roleId", receiveListRoleId}, {"roleName", receiveListRoleName}, {"gold", receiveListGold}, {"time", receiveListTime}};
                    // add
                    receiveList.Add(receiveListLuckyMoneyRole);
                }
                // 发送时间
                var time = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                // object
                var luckyMoney = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"luckyMoneyNo", luckyMoneyNo}, {"totalGold", totalGold}, {"totalNumber", totalNumber}, {"receiveNumber", receiveNumber}, {"receiveList", receiveList}, {"time", time}};
                return luckyMoney;
            }
            case 15004:
            {
                // 
                // 结果
                var resultLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var result = encoding.GetString(reader.ReadBytes(resultLength));
                // 金币
                var gold = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // object
                var data = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"result", result}, {"gold", gold}};
                return data;
            }
            case 15005:
            {
                // 

                // object
                var data = new System.Collections.Generic.Dictionary<System.String, System.Object>() {};
                return data;
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }
}