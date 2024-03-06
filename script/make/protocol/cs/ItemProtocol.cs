public static class ItemProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, System.Object data) 
    {
        switch (protocol) 
        {
            case 11101:
            {
                return;
            }
            case 11102:
            {
                return;
            }
            case 11103:
            {
                return;
            }
            case 11106:
            {
                // convert
                var dataCast = (System.Collections.Generic.Dictionary<System.String, System.Object>)data;
                // 物品编号
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)dataCast["itemNo"]));
                // 数量
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)(System.UInt16)dataCast["number"]));
                // 类型
                writer.Write((System.Byte)dataCast["type"]);
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
                var data = new System.Collections.Generic.List<System.Object>(dataLength);
                while (dataLength-- > 0)
                {
                    // 
                    // 物品编号
                    var itemNo = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 物品ID
                    var itemId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 类型
                    var type = reader.ReadByte();
                    // 数量
                    var number = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // object
                    var item = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"item_no", itemNo}, {"item_id", itemId}, {"type", type}, {"number", number}};
                    // add
                    data.Add(item);
                }
                return data;
            }
            case 11102:
            {
                // 
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<System.Object>(dataLength);
                while (dataLength-- > 0)
                {
                    // 
                    // 物品编号
                    var itemNo = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 物品ID
                    var itemId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 类型
                    var type = reader.ReadByte();
                    // 数量
                    var number = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // object
                    var item = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"item_no", itemNo}, {"item_id", itemId}, {"type", type}, {"number", number}};
                    // add
                    data.Add(item);
                }
                return data;
            }
            case 11103:
            {
                // 
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<System.Object>(dataLength);
                while (dataLength-- > 0)
                {
                    // 
                    // 物品编号
                    var itemNo = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 物品ID
                    var itemId = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 类型
                    var type = reader.ReadByte();
                    // 数量
                    var number = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // object
                    var item = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"item_no", itemNo}, {"item_id", itemId}, {"type", type}, {"number", number}};
                    // add
                    data.Add(item);
                }
                return data;
            }
            case 11104:
            {
                // 
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<System.Object>(dataLength);
                while (dataLength-- > 0)
                {
                    // 
                    // 物品编号
                    var itemNo = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // 物品ID
                    var itemId = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // 类型
                    var type = reader.ReadByte();
                    // object
                    var item = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"item_no", itemNo}, {"item_id", itemId}, {"type", type}};
                    // add
                    data.Add(item);
                }
                return data;
            }
            case 11106:
            {
                // 结果
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = encoding.GetString(reader.ReadBytes(dataLength));
                return data;
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }
}