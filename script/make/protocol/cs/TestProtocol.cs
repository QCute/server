public static class TestProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, System.Object data) 
    {
        switch (protocol) 
        {
            case 65532:
            {
                // single i16
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)data));
                return;
            }
            case 65533:
            {
                // single list
                var dataCast = (System.Collections.Generic.List<System.Object>)data;
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)dataCast.Count));
                foreach(var dataCastItemRaw in dataCast)
                {
                    // single u32
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int32)(System.UInt32)dataCastItemRaw));
                }
                return;
            }
            case 65534:
            {
                // key single list
                var dataCast = (System.Collections.Generic.Dictionary<System.Object, System.Collections.Generic.Dictionary<System.String, System.Object>>)data;
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)dataCast.Count));
                foreach(var dataCastItemRaw in dataCast)
                {
                    // key single u32
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int32)(System.UInt32)dataCastItemRaw.Value));
                }
                return;
            }
            case 65535:
            {
                // convert
                var dataCast = (System.Collections.Generic.Dictionary<System.String, System.Object>)data;
                // binary
                writer.Write((System.Byte[])dataCast["binary"]);
                // bool
                writer.Write((System.Byte)((System.Boolean)dataCast["boolean"] ? 1 : 0));
                // u8
                writer.Write((System.Byte)dataCast["u8"]);
                // u16
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)(System.UInt16)dataCast["u16"]));
                // u32
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int32)(System.UInt32)dataCast["u32"]));
                // u64
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)dataCast["u64"]));
                // i8
                writer.Write((System.SByte)dataCast["i8"]);
                // i16
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)dataCast["i16"]));
                // i32
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int32)dataCast["i32"]));
                // i16
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)dataCast["i64"]));
                // f32
                var f32Bytes = System.BitConverter.GetBytes((System.Single)dataCast["f32"]);
                if (System.BitConverter.IsLittleEndian) System.Array.Reverse(f32Bytes);
                writer.Write(f32Bytes);
                // f64
                var f64Bytes = System.BitConverter.GetBytes((System.Double)dataCast["f64"]);
                if (System.BitConverter.IsLittleEndian) System.Array.Reverse(f64Bytes);
                writer.Write(f64Bytes);
                // str
                var strBytes = encoding.GetBytes((System.String)dataCast["str"]);
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)strBytes.Length));
                writer.Write(strBytes);
                // bst
                var bstBytes = encoding.GetBytes((System.String)dataCast["bst"]);
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)bstBytes.Length));
                writer.Write(bstBytes);
                // convert
                var tuple = (System.Collections.Generic.Dictionary<System.String, System.Object>)dataCast["tuple"];
                // tuple binary
                writer.Write((System.Byte[])tuple["binary"]);
                // convert
                var tupleSub = (System.Collections.Generic.Dictionary<System.String, System.Object>)tuple["sub"];
                // tuple tuple u8
                writer.Write((System.Byte)tupleSub["u8"]);
                // tuple tuple str
                var tupleSubStrBytes = encoding.GetBytes((System.String)tupleSub["str"]);
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)tupleSubStrBytes.Length));
                writer.Write(tupleSubStrBytes);
                // tuple list
                var tupleList = (System.Collections.Generic.List<System.Object>)tuple["list"];
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)tupleList.Count));
                foreach(var tupleListItemRaw in tupleList)
                {
                    // convert
                    var tupleListItem = (System.Collections.Generic.Dictionary<System.String, System.Object>)tupleListItemRaw;
                    // tuple list i16
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)tupleListItem["i16"]));
                    // tuple list bst
                    var tupleListItemBstBytes = encoding.GetBytes((System.String)tupleListItem["bst"]);
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)tupleListItemBstBytes.Length));
                    writer.Write(tupleListItemBstBytes);
                }
                // 
                var tupleSingle = (System.Collections.Generic.List<System.Object>)tuple["single"];
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)tupleSingle.Count));
                foreach(var tupleSingleItemRaw in tupleSingle)
                {
                    // bool
                    writer.Write((System.Byte)((System.Boolean)tupleSingleItemRaw ? 1 : 0));
                }
                // list
                var indexList = (System.Collections.Generic.List<System.Object>)dataCast["indexList"];
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)indexList.Count));
                foreach(var indexListItemRaw in indexList)
                {
                    // convert
                    var indexListItem = (System.Collections.Generic.Dictionary<System.String, System.Object>)indexListItemRaw;
                    // tuple binary
                    writer.Write((System.Byte[])indexListItem["binary"]);
                    // convert
                    var indexListItemSub = (System.Collections.Generic.Dictionary<System.String, System.Object>)indexListItem["sub"];
                    // tuple tuple u8
                    writer.Write((System.Byte)indexListItemSub["u8"]);
                    // tuple tuple str
                    var indexListItemSubStrBytes = encoding.GetBytes((System.String)indexListItemSub["str"]);
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)indexListItemSubStrBytes.Length));
                    writer.Write(indexListItemSubStrBytes);
                    // tuple list
                    var indexListItemList = (System.Collections.Generic.List<System.Object>)indexListItem["list"];
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)indexListItemList.Count));
                    foreach(var indexListItemListItemRaw in indexListItemList)
                    {
                        // convert
                        var indexListItemListItem = (System.Collections.Generic.Dictionary<System.String, System.Object>)indexListItemListItemRaw;
                        // tuple list i16
                        writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)indexListItemListItem["i16"]));
                        // tuple list bst
                        var indexListItemListItemBstBytes = encoding.GetBytes((System.String)indexListItemListItem["bst"]);
                        writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)indexListItemListItemBstBytes.Length));
                        writer.Write(indexListItemListItemBstBytes);
                    }
                    // 
                    var indexListItemSingle = (System.Collections.Generic.List<System.Object>)indexListItem["single"];
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)indexListItemSingle.Count));
                    foreach(var indexListItemSingleItemRaw in indexListItemSingle)
                    {
                        // bool
                        writer.Write((System.Byte)((System.Boolean)indexListItemSingleItemRaw ? 1 : 0));
                    }
                }
                // 
                var keyList = (System.Collections.Generic.Dictionary<System.Object, System.Collections.Generic.Dictionary<System.String, System.Object>>)dataCast["keyList"];
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)keyList.Count));
                foreach(var keyListItemRaw in keyList)
                {
                    // convert
                    var keyListItem = (System.Collections.Generic.Dictionary<System.String, System.Object>)keyListItemRaw.Value;
                    // binary
                    writer.Write((System.Byte[])keyListItem["binary"]);
                    // bool
                    writer.Write((System.Byte)((System.Boolean)keyListItem["boolean"] ? 1 : 0));
                    // u8
                    writer.Write((System.Byte)keyListItem["u8"]);
                    // u16
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)(System.UInt16)keyListItem["u16"]));
                    // u32
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int32)(System.UInt32)keyListItem["u32"]));
                    // u64
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)keyListItem["u64"]));
                    // i8
                    writer.Write((System.SByte)keyListItem["i8"]);
                    // i16
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)keyListItem["i16"]));
                    // i32
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int32)keyListItem["i32"]));
                    // i64
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)keyListItem["i64"]));
                    // f32
                    var keyListItemF32Bytes = System.BitConverter.GetBytes((System.Single)keyListItem["f32"]);
                    if (System.BitConverter.IsLittleEndian) System.Array.Reverse(keyListItemF32Bytes);
                    writer.Write(keyListItemF32Bytes);
                    // f64
                    var keyListItemF64Bytes = System.BitConverter.GetBytes((System.Double)keyListItem["f64"]);
                    if (System.BitConverter.IsLittleEndian) System.Array.Reverse(keyListItemF64Bytes);
                    writer.Write(keyListItemF64Bytes);
                    // str
                    var keyListItemStrBytes = encoding.GetBytes((System.String)keyListItem["str"]);
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)keyListItemStrBytes.Length));
                    writer.Write(keyListItemStrBytes);
                    // bst
                    var keyListItemBstBytes = encoding.GetBytes((System.String)keyListItem["bst"]);
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)keyListItemBstBytes.Length));
                    writer.Write(keyListItemBstBytes);
                }
                return;
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }

    public static System.Object Decode(System.Text.Encoding encoding, System.IO.BinaryReader reader, System.UInt16 protocol) 
    {
        switch (protocol) 
        {
            case 65532:
            {
                // single i16
                var data = System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                return data;
            }
            case 65533:
            {
                // single list
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<System.Object>(dataLength);
                while (dataLength-- > 0)
                {
                    // single u32
                    var item = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // add
                    data.Add(item);
                }
                return data;
            }
            case 65534:
            {
                // key single list
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.Dictionary<System.Object, System.Collections.Generic.Dictionary<System.String, System.Object>>(dataLength);
                while (dataLength-- > 0)
                {
                    // key single u32
                    var item = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // add
                    data[u32] = item;
                }
                return data;
            }
            case 65535:
            {
                // 
                // binary
                var binary = reader.ReadBytes(6);
                // bool
                var boolean = reader.ReadByte() != 0;
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
                // tuple binary
                var tupleBinary = reader.ReadBytes(6);
                // tuple tuple
                // tuple tuple u8
                var tupleSubU8 = reader.ReadByte();
                // tuple tuple str
                var tupleSubStrLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var tupleSubStr = encoding.GetString(reader.ReadBytes(tupleSubStrLength));
                // object
                var tupleSub = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"u8", tupleSubU8}, {"str", tupleSubStr}};
                // tuple list
                var tupleListLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var tupleList = new System.Collections.Generic.List<System.Object>(tupleListLength);
                while (tupleListLength-- > 0)
                {
                    // 
                    // tuple list i16
                    var tupleListI16 = System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // tuple list bst
                    var tupleListBstLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var tupleListBst = encoding.GetString(reader.ReadBytes(tupleListBstLength));
                    // object
                    var tupleListItem = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"i16", tupleListI16}, {"bst", tupleListBst}};
                    // add
                    tupleList.Add(tupleListItem);
                }
                // 
                var tupleSingleLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var tupleSingle = new System.Collections.Generic.List<System.Object>(tupleSingleLength);
                while (tupleSingleLength-- > 0)
                {
                    // bool
                    var tupleSingleItem = reader.ReadByte() != 0;
                    // add
                    tupleSingle.Add(tupleSingleItem);
                }
                // object
                var tuple = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"binary", tupleBinary}, {"sub", tupleSub}, {"list", tupleList}, {"single", tupleSingle}};
                // list
                var indexListLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var indexList = new System.Collections.Generic.List<System.Object>(indexListLength);
                while (indexListLength-- > 0)
                {
                    // 
                    // tuple binary
                    var indexListBinary = reader.ReadBytes(6);
                    // tuple tuple
                    // tuple tuple u8
                    var indexListSubU8 = reader.ReadByte();
                    // tuple tuple str
                    var indexListSubStrLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var indexListSubStr = encoding.GetString(reader.ReadBytes(indexListSubStrLength));
                    // object
                    var indexListSub = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"u8", indexListSubU8}, {"str", indexListSubStr}};
                    // tuple list
                    var indexListListLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var indexListList = new System.Collections.Generic.List<System.Object>(indexListListLength);
                    while (indexListListLength-- > 0)
                    {
                        // 
                        // tuple list i16
                        var indexListListI16 = System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                        // tuple list bst
                        var indexListListBstLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                        var indexListListBst = encoding.GetString(reader.ReadBytes(indexListListBstLength));
                        // object
                        var indexListListItem = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"i16", indexListListI16}, {"bst", indexListListBst}};
                        // add
                        indexListList.Add(indexListListItem);
                    }
                    // 
                    var indexListSingleLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var indexListSingle = new System.Collections.Generic.List<System.Object>(indexListSingleLength);
                    while (indexListSingleLength-- > 0)
                    {
                        // bool
                        var indexListSingleItem = reader.ReadByte() != 0;
                        // add
                        indexListSingle.Add(indexListSingleItem);
                    }
                    // object
                    var indexListItem = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"binary", indexListBinary}, {"sub", indexListSub}, {"list", indexListList}, {"single", indexListSingle}};
                    // add
                    indexList.Add(indexListItem);
                }
                // 
                var keyListLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var keyList = new System.Collections.Generic.Dictionary<System.Object, System.Collections.Generic.Dictionary<System.String, System.Object>>(keyListLength);
                while (keyListLength-- > 0)
                {
                    // 
                    // binary
                    var keyListBinary = reader.ReadBytes(6);
                    // boolean
                    var keyListBoolean = reader.ReadByte() != 0;
                    // u8
                    var keyListU8 = reader.ReadByte();
                    // u16
                    var keyListU16 = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // u32
                    var keyListU32 = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // u64
                    var keyListU64 = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // i8
                    var keyListI8 = reader.ReadSByte();
                    // i16
                    var keyListI16 = System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // i32
                    var keyListI32 = System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // i64
                    var keyListI64 = System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // f32
                    var keyListF32 = System.BitConverter.ToSingle(System.BitConverter.GetBytes(System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32())), 0);
                    // f64
                    var keyListF64 = System.BitConverter.ToDouble(System.BitConverter.GetBytes(System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64())), 0);
                    // str
                    var keyListStrLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var keyListStr = encoding.GetString(reader.ReadBytes(keyListStrLength));
                    // bst
                    var keyListBstLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var keyListBst = encoding.GetString(reader.ReadBytes(keyListBstLength));
                    // object
                    var keyListItem = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"binary", keyListBinary}, {"boolean", keyListBoolean}, {"u8", keyListU8}, {"u16", keyListU16}, {"u32", keyListU32}, {"u64", keyListU64}, {"i8", keyListI8}, {"i16", keyListI16}, {"i32", keyListI32}, {"i64", keyListI64}, {"f32", keyListF32}, {"f64", keyListF64}, {"str", keyListStr}, {"bst", keyListBst}};
                    // add
                    keyList[u8] = keyListItem;
                }
                // object
                var data = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"binary", binary}, {"boolean", boolean}, {"u8", u8}, {"u16", u16}, {"u32", u32}, {"u64", u64}, {"i8", i8}, {"i16", i16}, {"i32", i32}, {"i64", i64}, {"f32", f32}, {"f64", f64}, {"str", str}, {"bst", bst}, {"tuple", tuple}, {"index_list", indexList}, {"key_list", keyList}};
                return data;
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }
}