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
                // bin
                var bin = reader.ReadBytes(6);
                // bool
                var bool = reader.ReadByte() != 0;
                // u8
                var u8 = reader.ReadByte();
                // u16
                var u16 = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                // u32
                var u32 = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                // u64
                var u64 = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // i8
                var i8 = reader.ReadSByte();
                // i16
                var i16 = System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                // i32
                var i32 = System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                // i16
                var i64 = System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // f32
                var f32 = System.BitConverter.ToSingle(System.BitConverter.GetBytes(System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32())), 0);
                // f64
                var f64 = System.BitConverter.ToDouble(System.BitConverter.GetBytes(System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64())), 0);
                // str
                var strLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var str = encoding.GetString(reader.ReadBytes(strLength));
                // bst
                var bstLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var bst = encoding.GetString(reader.ReadBytes(bstLength));
                // tuple
                // tuple bin
                var bin = reader.ReadBytes(6);
                // tuple tuple
                // tuple tuple u8
                var u8 = reader.ReadByte();
                // tuple tuple str
                var strLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var str = encoding.GetString(reader.ReadBytes(strLength));
                // object
                var sub = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"u8", u8}, {"str", str}};
                // tuple list
                var listLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var list = new System.Collections.Generic.List<System.Object>(listLength);
                while (listLength-- > 0)
                {
                    // 
                    // tuple list i16
                    var i16 = System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // tuple list bst
                    var bstLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var bst = encoding.GetString(reader.ReadBytes(bstLength));
                    // object
                    var listItem = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"i16", i16}, {"bst", bst}};
                    // add
                    list.Add(listItem);
                }
                // u8
                var singleLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var single = new System.Collections.Generic.List<System.Object>(singleLength);
                while (singleLength-- > 0)
                {
                    // 
                    var singleItem = reader.ReadByte();
                    // add
                    single.Add(singleItem);
                }
                // object
                var tuple = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"bin", bin}, {"sub", sub}, {"list", list}, {"single", single}};
                // list
                var indexListLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var indexList = new System.Collections.Generic.List<System.Object>(indexListLength);
                while (indexListLength-- > 0)
                {
                    // 
                    // tuple bin
                    var bin = reader.ReadBytes(6);
                    // tuple tuple
                    // tuple tuple u8
                    var u8 = reader.ReadByte();
                    // tuple tuple str
                    var strLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var str = encoding.GetString(reader.ReadBytes(strLength));
                    // object
                    var sub = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"u8", u8}, {"str", str}};
                    // tuple list
                    var listLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var list = new System.Collections.Generic.List<System.Object>(listLength);
                    while (listLength-- > 0)
                    {
                        // 
                        // tuple list i16
                        var i16 = System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                        // tuple list bst
                        var bstLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                        var bst = encoding.GetString(reader.ReadBytes(bstLength));
                        // object
                        var listItem = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"i16", i16}, {"bst", bst}};
                        // add
                        list.Add(listItem);
                    }
                    // u8
                    var singleLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var single = new System.Collections.Generic.List<System.Object>(singleLength);
                    while (singleLength-- > 0)
                    {
                        // 
                        var singleItem = reader.ReadByte();
                        // add
                        single.Add(singleItem);
                    }
                    // object
                    var indexListItem = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"bin", bin}, {"sub", sub}, {"list", list}, {"single", single}};
                    // add
                    indexList.Add(indexListItem);
                }
                // 
                var keyListLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var keyList = new System.Collections.Generic.Dictionary<System.Object, System.Collections.Generic.Dictionary<System.String, System.Object>>(keyListLength);
                while (keyListLength-- > 0)
                {
                    // 
                    // bin
                    var bin = reader.ReadBytes(6);
                    // bool
                    var bool = reader.ReadByte() != 0;
                    // u8
                    var u8 = reader.ReadByte();
                    // u16
                    var u16 = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // u32
                    var u32 = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // u64
                    var u64 = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // i8
                    var i8 = reader.ReadSByte();
                    // i16
                    var i16 = System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // i32
                    var i32 = System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // i64
                    var i64 = System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // f32
                    var f32 = System.BitConverter.ToSingle(System.BitConverter.GetBytes(System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32())), 0);
                    // f64
                    var f64 = System.BitConverter.ToDouble(System.BitConverter.GetBytes(System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64())), 0);
                    // str
                    var strLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var str = encoding.GetString(reader.ReadBytes(strLength));
                    // bst
                    var bstLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var bst = encoding.GetString(reader.ReadBytes(bstLength));
                    // object
                    var keyListItem = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"bin", bin}, {"bool", bool}, {"u8", u8}, {"u16", u16}, {"u32", u32}, {"u64", u64}, {"i8", i8}, {"i16", i16}, {"i32", i32}, {"i64", i64}, {"f32", f32}, {"f64", f64}, {"str", str}, {"bst", bst}};
                    // add
                    keyList[u8] = keyListItem;
                }
                // object
                var  = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"bin", bin}, {"bool", bool}, {"u8", u8}, {"u16", u16}, {"u32", u32}, {"u64", u64}, {"i8", i8}, {"i16", i16}, {"i32", i32}, {"i64", i64}, {"f32", f32}, {"f64", f64}, {"str", str}, {"bst", bst}, {"tuple", tuple}, {"indexList", indexList}, {"keyList", keyList}};
                return ;
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }
}