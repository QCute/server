public static class TestProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, System.Collections.Generic.Dictionary<System.String, System.Object> data) 
    {
        switch (protocol) 
        {
            case 65535:
            {
                // bin
                writer.Write((System.Byte[])((System.Collections.Generic.Dictionary<System.String, System.Object>)data[""])["bin"]);
                // bool
                writer.Write((System.Byte)((System.Boolean)((System.Collections.Generic.Dictionary<System.String, System.Object>)data[""])["bool"] ? 1 : 0));
                // u8
                writer.Write((System.Byte)((System.Collections.Generic.Dictionary<System.String, System.Object>)data[""])["u8"]);
                // u16
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)(System.UInt16)((System.Collections.Generic.Dictionary<System.String, System.Object>)data[""])["u16"]));
                // u32
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int32)(System.UInt32)((System.Collections.Generic.Dictionary<System.String, System.Object>)data[""])["u32"]));
                // u64
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)((System.Collections.Generic.Dictionary<System.String, System.Object>)data[""])["u64"]));
                // i8
                writer.Write((System.SByte)((System.Collections.Generic.Dictionary<System.String, System.Object>)data[""])["i8"]);
                // i16
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)((System.Collections.Generic.Dictionary<System.String, System.Object>)data[""])["i16"]));
                // i32
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int32)((System.Collections.Generic.Dictionary<System.String, System.Object>)data[""])["i32"]));
                // i16
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)((System.Collections.Generic.Dictionary<System.String, System.Object>)data[""])["i64"]));
                // f32
                var f32Bytes = System.BitConverter.GetBytes((System.Single)((System.Collections.Generic.Dictionary<System.String, System.Object>)data[""])["f32"]);
                if (System.BitConverter.IsLittleEndian) System.Array.Reverse(f32Bytes);
                writer.Write(f32Bytes);
                // f64
                var f64Bytes = System.BitConverter.GetBytes((System.Double)((System.Collections.Generic.Dictionary<System.String, System.Object>)data[""])["f64"]);
                if (System.BitConverter.IsLittleEndian) System.Array.Reverse(f64Bytes);
                writer.Write(f64Bytes);
                // str
                var strBytes = encoding.GetBytes((System.String)((System.Collections.Generic.Dictionary<System.String, System.Object>)data[""])["str"]);
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)strBytes.Length));
                writer.Write(strBytes);
                // bst
                var bstBytes = encoding.GetBytes((System.String)((System.Collections.Generic.Dictionary<System.String, System.Object>)data[""])["bst"]);
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)bstBytes.Length));
                writer.Write(bstBytes);
                // tuple bin
                writer.Write((System.Byte[])((System.Collections.Generic.Dictionary<System.String, System.Object>)((System.Collections.Generic.Dictionary<System.String, System.Object>)data[""])["tuple"])["bin"]);
                // tuple tuple u8
                writer.Write((System.Byte)((System.Collections.Generic.Dictionary<System.String, System.Object>)((System.Collections.Generic.Dictionary<System.String, System.Object>)((System.Collections.Generic.Dictionary<System.String, System.Object>)data[""])["tuple"])["sub"])["u8"]);
                // tuple tuple str
                var strBytes = encoding.GetBytes((System.String)((System.Collections.Generic.Dictionary<System.String, System.Object>)((System.Collections.Generic.Dictionary<System.String, System.Object>)((System.Collections.Generic.Dictionary<System.String, System.Object>)data[""])["tuple"])["sub"])["str"]);
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)strBytes.Length));
                writer.Write(strBytes);
                // tuple list
                var listData = (System.Collections.Generic.List<System.Object>)((System.Collections.Generic.Dictionary<System.String, System.Object>)((System.Collections.Generic.Dictionary<System.String, System.Object>)data[""])["tuple"])["list"];
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)listData.Count));
                foreach(System.Collections.Generic.Dictionary<System.String, System.Object> listDataItem in listData)
                {
                    // tuple list i16
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)((System.Collections.Generic.Dictionary<System.String, System.Object>)listDataItem)["i16"]));
                    // tuple list bst
                    var bstBytes = encoding.GetBytes((System.String)((System.Collections.Generic.Dictionary<System.String, System.Object>)listDataItem)["bst"]);
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)bstBytes.Length));
                    writer.Write(bstBytes);
                }
                // u8
                var singleData = (System.Collections.Generic.List<System.Object>)((System.Collections.Generic.Dictionary<System.String, System.Object>)((System.Collections.Generic.Dictionary<System.String, System.Object>)data[""])["tuple"])["single"];
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)singleData.Count));
                foreach(System.Byte singleDataItem in singleData)
                {
                    // 
                    writer.Write((System.Byte)singleDataItem);
                }
                // list
                var indexListData = (System.Collections.Generic.List<System.Object>)((System.Collections.Generic.Dictionary<System.String, System.Object>)data[""])["indexList"];
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)indexListData.Count));
                foreach(System.Collections.Generic.Dictionary<System.String, System.Object> indexListDataItem in indexListData)
                {
                    // tuple bin
                    writer.Write((System.Byte[])((System.Collections.Generic.Dictionary<System.String, System.Object>)indexListDataItem)["bin"]);
                    // tuple tuple u8
                    writer.Write((System.Byte)((System.Collections.Generic.Dictionary<System.String, System.Object>)((System.Collections.Generic.Dictionary<System.String, System.Object>)indexListDataItem)["sub"])["u8"]);
                    // tuple tuple str
                    var strBytes = encoding.GetBytes((System.String)((System.Collections.Generic.Dictionary<System.String, System.Object>)((System.Collections.Generic.Dictionary<System.String, System.Object>)indexListDataItem)["sub"])["str"]);
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)strBytes.Length));
                    writer.Write(strBytes);
                    // tuple list
                    var listData = (System.Collections.Generic.List<System.Object>)((System.Collections.Generic.Dictionary<System.String, System.Object>)indexListDataItem)["list"];
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)listData.Count));
                    foreach(System.Collections.Generic.Dictionary<System.String, System.Object> listDataItem in listData)
                    {
                        // tuple list i16
                        writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)((System.Collections.Generic.Dictionary<System.String, System.Object>)listDataItem)["i16"]));
                        // tuple list bst
                        var bstBytes = encoding.GetBytes((System.String)((System.Collections.Generic.Dictionary<System.String, System.Object>)listDataItem)["bst"]);
                        writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)bstBytes.Length));
                        writer.Write(bstBytes);
                    }
                    // u8
                    var singleData = (System.Collections.Generic.List<System.Object>)((System.Collections.Generic.Dictionary<System.String, System.Object>)indexListDataItem)["single"];
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)singleData.Count));
                    foreach(System.Byte singleDataItem in singleData)
                    {
                        // 
                        writer.Write((System.Byte)singleDataItem);
                    }
                }
                // 
                var keyListData = (System.Collections.Generic.Dictionary<System.Object, System.Collections.Generic.Dictionary<System.String, System.Object>>)((System.Collections.Generic.Dictionary<System.String, System.Object>)data[""])["keyList"];
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)keyListData.Count));
                foreach(System.Collections.Generic.KeyValuePair<System.Object, System.Collections.Generic.Dictionary<System.String, System.Object>> keyListDataItem in keyListData)
                {
                    // bin
                    writer.Write((System.Byte[])((System.Collections.Generic.Dictionary<System.String, System.Object>)keyListDataItem.Value)["bin"]);
                    // bool
                    writer.Write((System.Byte)((System.Boolean)((System.Collections.Generic.Dictionary<System.String, System.Object>)keyListDataItem.Value)["bool"] ? 1 : 0));
                    // u8
                    writer.Write((System.Byte)((System.Collections.Generic.Dictionary<System.String, System.Object>)keyListDataItem.Value)["u8"]);
                    // u16
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)(System.UInt16)((System.Collections.Generic.Dictionary<System.String, System.Object>)keyListDataItem.Value)["u16"]));
                    // u32
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int32)(System.UInt32)((System.Collections.Generic.Dictionary<System.String, System.Object>)keyListDataItem.Value)["u32"]));
                    // u64
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)((System.Collections.Generic.Dictionary<System.String, System.Object>)keyListDataItem.Value)["u64"]));
                    // i8
                    writer.Write((System.SByte)((System.Collections.Generic.Dictionary<System.String, System.Object>)keyListDataItem.Value)["i8"]);
                    // i16
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)((System.Collections.Generic.Dictionary<System.String, System.Object>)keyListDataItem.Value)["i16"]));
                    // i32
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int32)((System.Collections.Generic.Dictionary<System.String, System.Object>)keyListDataItem.Value)["i32"]));
                    // i64
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)((System.Collections.Generic.Dictionary<System.String, System.Object>)keyListDataItem.Value)["i64"]));
                    // f32
                    var f32Bytes = System.BitConverter.GetBytes((System.Single)((System.Collections.Generic.Dictionary<System.String, System.Object>)keyListDataItem.Value)["f32"]);
                    if (System.BitConverter.IsLittleEndian) System.Array.Reverse(f32Bytes);
                    writer.Write(f32Bytes);
                    // f64
                    var f64Bytes = System.BitConverter.GetBytes((System.Double)((System.Collections.Generic.Dictionary<System.String, System.Object>)keyListDataItem.Value)["f64"]);
                    if (System.BitConverter.IsLittleEndian) System.Array.Reverse(f64Bytes);
                    writer.Write(f64Bytes);
                    // str
                    var strBytes = encoding.GetBytes((System.String)((System.Collections.Generic.Dictionary<System.String, System.Object>)keyListDataItem.Value)["str"]);
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)strBytes.Length));
                    writer.Write(strBytes);
                    // bst
                    var bstBytes = encoding.GetBytes((System.String)((System.Collections.Generic.Dictionary<System.String, System.Object>)keyListDataItem.Value)["bst"]);
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)bstBytes.Length));
                    writer.Write(bstBytes);
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
            case 65535:
            {
                // 
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = encoding.GetString(reader.ReadBytes(dataLength));
                return data;
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }
}