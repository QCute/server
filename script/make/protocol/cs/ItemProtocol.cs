public class ItemQueryItemRequest
{
    public System.UInt16 protocol = 11101;
    public Empty data;
}

public class ItemQueryItemResponse
{
    public System.UInt16 protocol = 11101;
    public System.Collections.Generic.List<(
        System.UInt64 itemNo,                                                   // 物品编号
        System.UInt32 itemId,                                                   // 物品ID
        System.Byte type,                                                       // 类型
        System.UInt16 number                                                    // 数量
    )> data;
}

public class ItemQueryBagRequest
{
    public System.UInt16 protocol = 11102;
    public Empty data;
}

public class ItemQueryBagResponse
{
    public System.UInt16 protocol = 11102;
    public System.Collections.Generic.List<(
        System.UInt64 itemNo,                                                   // 物品编号
        System.UInt32 itemId,                                                   // 物品ID
        System.Byte type,                                                       // 类型
        System.UInt16 number                                                    // 数量
    )> data;
}

public class ItemQueryStoreRequest
{
    public System.UInt16 protocol = 11103;
    public Empty data;
}

public class ItemQueryStoreResponse
{
    public System.UInt16 protocol = 11103;
    public System.Collections.Generic.List<(
        System.UInt64 itemNo,                                                   // 物品编号
        System.UInt32 itemId,                                                   // 物品ID
        System.Byte type,                                                       // 类型
        System.UInt16 number                                                    // 数量
    )> data;
}



public class ItemDeleteResponse
{
    public System.UInt16 protocol = 11104;
    public System.Collections.Generic.List<(
        System.UInt64 itemNo,                                                   // 物品编号
        System.UInt32 itemId,                                                   // 物品ID
        System.Byte type                                                        // 类型
    )> data;
}

public class ItemUseRequest
{
    public System.UInt16 protocol = 11106;
    public (
        System.UInt64 itemNo,                                                   // 物品编号
        System.UInt16 number,                                                   // 数量
        System.Byte type                                                        // 类型
    ) data;
}

public class ItemUseResponse
{
    public System.UInt16 protocol = 11106;
    public System.String data;
}

public static class ItemProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, System.Object dataRaw) 
    {
        switch (protocol) 
        {
            case 11101:
            {
                #pragma warning disable
                var data = (Empty)dataRaw;
                #pragma warning restore

                return;
            }
            case 11102:
            {
                #pragma warning disable
                var data = (Empty)dataRaw;
                #pragma warning restore

                return;
            }
            case 11103:
            {
                #pragma warning disable
                var data = (Empty)dataRaw;
                #pragma warning restore

                return;
            }
            case 11106:
            {
                var data = ((System.UInt64 itemNo, System.UInt16 number, System.Byte type))dataRaw;

                // 物品编号
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)data.itemNo));
                // 数量
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)(System.UInt16)data.number));
                // 类型
                writer.Write((System.Byte)data.type);
                return;
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }

    public static System.Object Decode(System.Text.Encoding encoding, System.IO.BinaryReader reader, System.UInt16 protocol) 
    {
        switch (protocol) 
        {
            case 11101:
            {
                // 
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<(System.UInt64 itemNo, System.UInt32 itemId, System.Byte type, System.UInt16 number)>(dataLength);
                while (dataLength-- > 0)
                {
                    // 
                    // 物品编号
                    var dataDataItemNo = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 物品ID
                    var dataDataItemId = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // 类型
                    var dataDataType = reader.ReadByte();
                    // 数量
                    var dataDataNumber = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // object
                    var dataData = (itemNo: dataDataItemNo, itemId: dataDataItemId, type: dataDataType, number: dataDataNumber);
                    // add
                    data.Add(dataData);
                }
                return (protocol: 11101, data: data);
            }
            case 11102:
            {
                // 
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<(System.UInt64 itemNo, System.UInt32 itemId, System.Byte type, System.UInt16 number)>(dataLength);
                while (dataLength-- > 0)
                {
                    // 
                    // 物品编号
                    var dataDataItemNo = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 物品ID
                    var dataDataItemId = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // 类型
                    var dataDataType = reader.ReadByte();
                    // 数量
                    var dataDataNumber = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // object
                    var dataData = (itemNo: dataDataItemNo, itemId: dataDataItemId, type: dataDataType, number: dataDataNumber);
                    // add
                    data.Add(dataData);
                }
                return (protocol: 11102, data: data);
            }
            case 11103:
            {
                // 
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<(System.UInt64 itemNo, System.UInt32 itemId, System.Byte type, System.UInt16 number)>(dataLength);
                while (dataLength-- > 0)
                {
                    // 
                    // 物品编号
                    var dataDataItemNo = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 物品ID
                    var dataDataItemId = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // 类型
                    var dataDataType = reader.ReadByte();
                    // 数量
                    var dataDataNumber = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // object
                    var dataData = (itemNo: dataDataItemNo, itemId: dataDataItemId, type: dataDataType, number: dataDataNumber);
                    // add
                    data.Add(dataData);
                }
                return (protocol: 11103, data: data);
            }
            case 11104:
            {
                // 
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<(System.UInt64 itemNo, System.UInt32 itemId, System.Byte type)>(dataLength);
                while (dataLength-- > 0)
                {
                    // 
                    // 物品编号
                    var dataDataItemNo = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 物品ID
                    var dataDataItemId = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // 类型
                    var dataDataType = reader.ReadByte();
                    // object
                    var dataData = (itemNo: dataDataItemNo, itemId: dataDataItemId, type: dataDataType);
                    // add
                    data.Add(dataData);
                }
                return (protocol: 11104, data: data);
            }
            case 11106:
            {
                // 结果
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = encoding.GetString(reader.ReadBytes(dataLength));
                return (protocol: 11106, data: data);
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }
}