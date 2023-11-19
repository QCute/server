public static class WelfareProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, dynamic data) 
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
                var dataLuckyMoneyNo = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // 总金币
                var dataTotalGold = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // 总数量
                var dataTotalNumber = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                // 已经领取人数
                var dataReceiveNumber = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                // 领取列表
                var dataReceiveListLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var dataReceiveList = new System.Collections.Generic.List<System.Object>(dataReceiveListLength);
                while (dataReceiveListLength-- > 0)
                {
                    // 
                    // 服务器Id
                    var dataReceiveListDataServerId = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // 角色Id
                    var dataReceiveListDataRoleId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 角色名
                    var dataReceiveListDataRoleNameLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var dataReceiveListDataRoleName = encoding.GetString(reader.ReadBytes(dataReceiveListDataRoleNameLength));
                    // 金币
                    var dataReceiveListDataGold = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 领取时间
                    var dataReceiveListDataTime = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // object
                    var dataReceiveListData = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"serverId", dataReceiveListDataServerId}, {"roleId", dataReceiveListDataRoleId}, {"roleName", dataReceiveListDataRoleName}, {"gold", dataReceiveListDataGold}, {"time", dataReceiveListDataTime}};
                    // add
                    dataReceiveList.Add(dataReceiveListData);
                }
                // 发送时间
                var dataTime = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                // object
                var data = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"luckyMoneyNo", dataLuckyMoneyNo}, {"totalGold", dataTotalGold}, {"totalNumber", dataTotalNumber}, {"receiveNumber", dataReceiveNumber}, {"receiveList", dataReceiveList}, {"time", dataTime}};
                return data;
            }
            case 15004:
            {
                // 
                // 结果
                var dataResultLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var dataResult = encoding.GetString(reader.ReadBytes(dataResultLength));
                // 金币
                var dataGold = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // object
                var data = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"result", dataResult}, {"gold", dataGold}};
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