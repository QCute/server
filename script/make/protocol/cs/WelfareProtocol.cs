public class WelfareSignRequest
{
    public System.UInt16 protocol = 15001;
    public Empty data;
}

public class WelfareSignResponse
{
    public System.UInt16 protocol = 15001;
    public System.String data;
}

public class WelfareAwardRequest
{
    public System.UInt16 protocol = 15002;
    public System.String data;
}

public class WelfareAwardResponse
{
    public System.UInt16 protocol = 15002;
    public System.String data;
}

public class WelfareQueryLuckyMoneyRequest
{
    public System.UInt16 protocol = 15003;
    public System.UInt64 data;
}

public class WelfareQueryLuckyMoneyResponse
{
    public System.UInt16 protocol = 15003;
    public (
        System.UInt64 luckyMoneyNo,                                             // 红包编号
        System.UInt64 totalGold,                                                // 总金币
        System.UInt32 totalNumber,                                              // 总数量
        System.UInt16 receiveNumber,                                            // 已经领取人数
        System.Collections.Generic.List<(
            System.UInt16 serverId,                                             // 服务器Id
            System.UInt64 roleId,                                               // 角色Id
            System.String roleName,                                             // 角色名
            System.UInt64 gold,                                                 // 金币
            System.UInt32 time                                                  // 领取时间
        )> receiveList,                                                         // 领取列表
        System.UInt32 time                                                      // 发送时间
    ) data;
}

public class WelfareReceiveLuckyMoneyRequest
{
    public System.UInt16 protocol = 15004;
    public System.UInt64 data;
}

public class WelfareReceiveLuckyMoneyResponse
{
    public System.UInt16 protocol = 15004;
    public (
        System.String result,                                                   // 结果
        System.UInt64 gold                                                      // 金币
    ) data;
}



public class WelfareLuckyMoneyComingResponse
{
    public System.UInt16 protocol = 15005;
    public Empty data;
}

public static class WelfareProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, System.Object dataRaw) 
    {
        switch (protocol) 
        {
            case 15001:
            {
                #pragma warning disable
                var data = (Empty)dataRaw;
                #pragma warning restore

                return;
            }
            case 15002:
            {
                var data = (System.String)dataRaw;
                // 兑换码
                var dataBytes = encoding.GetBytes((System.String)data);
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)dataBytes.Length));
                writer.Write(dataBytes);
                return;
            }
            case 15003:
            {
                var data = (System.UInt64)dataRaw;
                // 红包编号
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)data));
                return;
            }
            case 15004:
            {
                var data = (System.UInt64)dataRaw;
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
                return (protocol: 15001, data: data);
            }
            case 15002:
            {
                // 结果
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = encoding.GetString(reader.ReadBytes(dataLength));
                return (protocol: 15002, data: data);
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
                var dataReceiveList = new System.Collections.Generic.List<(System.UInt16 serverId, System.UInt64 roleId, System.String roleName, System.UInt64 gold, System.UInt32 time)>(dataReceiveListLength);
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
                    var dataReceiveListData = (serverId: dataReceiveListDataServerId, roleId: dataReceiveListDataRoleId, roleName: dataReceiveListDataRoleName, gold: dataReceiveListDataGold, time: dataReceiveListDataTime);
                    // add
                    dataReceiveList.Add(dataReceiveListData);
                }
                // 发送时间
                var dataTime = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                // object
                var data = (luckyMoneyNo: dataLuckyMoneyNo, totalGold: dataTotalGold, totalNumber: dataTotalNumber, receiveNumber: dataReceiveNumber, receiveList: dataReceiveList, time: dataTime);
                return (protocol: 15003, data: data);
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
                var data = (result: dataResult, gold: dataGold);
                return (protocol: 15004, data: data);
            }
            case 15005:
            {
                // 

                // object
                var data = new Empty();
                return (protocol: 15005, data: data);
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }
}