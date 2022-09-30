public static class TestProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, System.Collections.Generic.Dictionary<System.String, System.Object> data) 
    {
        switch (protocol) 
        {
            case 65535:
            {
                // binary
                writer.Write((System.Byte[])data["binary"]);
                // boolean
                writer.Write((System.Byte)((System.Boolean)data["boolean"] ? 1 : 0));
                // u8
                writer.Write((System.Byte)data["u8"]);
                // u16
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)(System.UInt16)data["u16"]));
                // u32
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int32)(System.UInt32)data["u32"]));
                // u64
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)data["u64"]));
                // i8
                writer.Write((System.SByte)data["i8"]);
                // i16
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)data["i16"]));
                // i32
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int32)data["i32"]));
                // i64
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)data["i64"]));
                // f32
                var f32Bytes = System.BitConverter.GetBytes((System.Single)data["f32"]);
                if (System.BitConverter.IsLittleEndian) System.Array.Reverse(f32Bytes);
                writer.Write(f32Bytes);
                // f64
                var f64Bytes = System.BitConverter.GetBytes((System.Double)data["f64"]);
                if (System.BitConverter.IsLittleEndian) System.Array.Reverse(f64Bytes);
                writer.Write(f64Bytes);
                // str
                var strBytes = encoding.GetBytes((System.String)data["str"]);
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)strBytes.Length));
                writer.Write(strBytes);
                // bst
                var bstBytes = encoding.GetBytes((System.String)data["bst"]);
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)bstBytes.Length));
                writer.Write(bstBytes);
                // list
                var indexListData = (System.Collections.ArrayList)data["indexList"];
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)indexListData.Count));
                foreach(System.Collections.Generic.Dictionary<System.String, System.Object> indexListDataItem in indexListData)
                {
                    // list_binary
                    writer.Write((System.Byte[])indexListDataItem["listBinary"]);
                    // list_boolean
                    writer.Write((System.Byte)((System.Boolean)indexListDataItem["listBoolean"] ? 1 : 0));
                    // list_u8
                    writer.Write((System.Byte)indexListDataItem["listU8"]);
                    // list_u16
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)(System.UInt16)indexListDataItem["listU16"]));
                    // list_u32
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int32)(System.UInt32)indexListDataItem["listU32"]));
                    // list_u64
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)indexListDataItem["listU64"]));
                    // list_i8
                    writer.Write((System.SByte)indexListDataItem["listI8"]);
                    // list_i16
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)indexListDataItem["listI16"]));
                    // list_i32
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int32)indexListDataItem["listI32"]));
                    // list_i64
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)indexListDataItem["listI64"]));
                    // list_f32
                    var listF32Bytes = System.BitConverter.GetBytes((System.Single)indexListDataItem["listF32"]);
                    if (System.BitConverter.IsLittleEndian) System.Array.Reverse(listF32Bytes);
                    writer.Write(listF32Bytes);
                    // list_f64
                    var listF64Bytes = System.BitConverter.GetBytes((System.Double)indexListDataItem["listF64"]);
                    if (System.BitConverter.IsLittleEndian) System.Array.Reverse(listF64Bytes);
                    writer.Write(listF64Bytes);
                    // list_str
                    var listStrBytes = encoding.GetBytes((System.String)indexListDataItem["listStr"]);
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)listStrBytes.Length));
                    writer.Write(listStrBytes);
                    // list_bst
                    var listBstBytes = encoding.GetBytes((System.String)indexListDataItem["listBst"]);
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)listBstBytes.Length));
                    writer.Write(listBstBytes);
                }
                // key_list
                var keyListData = (System.Collections.Generic.Dictionary<System.Object, System.Collections.Generic.Dictionary<System.String, System.Object>>)data["keyList"];
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)keyListData.Count));
                foreach(System.Collections.Generic.KeyValuePair<System.Object, System.Collections.Generic.Dictionary<System.String, System.Object>> keyListDataItem in keyListData)
                {
                    // list_binary
                    writer.Write((System.Byte[])keyListDataItem.Value["listBinary"]);
                    // list_boolean
                    writer.Write((System.Byte)((System.Boolean)keyListDataItem.Value["listBoolean"] ? 1 : 0));
                    // list_u8
                    writer.Write((System.Byte)keyListDataItem.Value["listU8"]);
                    // list_u16
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)(System.UInt16)keyListDataItem.Value["listU16"]));
                    // list_u32
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int32)(System.UInt32)keyListDataItem.Value["listU32"]));
                    // list_u64
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)keyListDataItem.Value["listU64"]));
                    // list_i8
                    writer.Write((System.SByte)keyListDataItem.Value["listI8"]);
                    // list_i16
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)keyListDataItem.Value["listI16"]));
                    // list_i32
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int32)keyListDataItem.Value["listI32"]));
                    // list_i64
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)keyListDataItem.Value["listI64"]));
                    // list_f32
                    var listF32Bytes = System.BitConverter.GetBytes((System.Single)keyListDataItem.Value["listF32"]);
                    if (System.BitConverter.IsLittleEndian) System.Array.Reverse(listF32Bytes);
                    writer.Write(listF32Bytes);
                    // list_f64
                    var listF64Bytes = System.BitConverter.GetBytes((System.Double)keyListDataItem.Value["listF64"]);
                    if (System.BitConverter.IsLittleEndian) System.Array.Reverse(listF64Bytes);
                    writer.Write(listF64Bytes);
                    // list_str
                    var listStrBytes = encoding.GetBytes((System.String)keyListDataItem.Value["listStr"]);
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)listStrBytes.Length));
                    writer.Write(listStrBytes);
                    // list_bst
                    var listBstBytes = encoding.GetBytes((System.String)keyListDataItem.Value["listBst"]);
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)listBstBytes.Length));
                    writer.Write(listBstBytes);
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
                // binary
                var binary = reader.ReadBytes(6);
                // boolean
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
                // list
                var indexListLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var indexList = new System.Collections.ArrayList(indexListLength);
                while (indexListLength-- > 0)
                {
                    // list_binary
                    var listBinary = reader.ReadBytes(6);
                    // list_boolean
                    var listBoolean = reader.ReadByte() != 0;
                    // list_u8
                    var listU8 = reader.ReadByte();
                    // list_u16
                    var listU16 = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // list_u32
                    var listU32 = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // list_u64
                    var listU64 = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // list_i8
                    var listI8 = reader.ReadSByte();
                    // list_i16
                    var listI16 = System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // list_i32
                    var listI32 = System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // list_i64
                    var listI64 = System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // list_f32
                    var listF32 = System.BitConverter.ToSingle(System.BitConverter.GetBytes(System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32())), 0);
                    // list_f64
                    var listF64 = System.BitConverter.ToDouble(System.BitConverter.GetBytes(System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64())), 0);
                    // list_str
                    var listStrLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var listStr = encoding.GetString(reader.ReadBytes(listStrLength));
                    // list_bst
                    var listBstLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var listBst = encoding.GetString(reader.ReadBytes(listBstLength));
                    // add
                    indexList.Add(new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"listBinary", listBinary}, {"listBoolean", listBoolean}, {"listU8", listU8}, {"listU16", listU16}, {"listU32", listU32}, {"listU64", listU64}, {"listI8", listI8}, {"listI16", listI16}, {"listI32", listI32}, {"listI64", listI64}, {"listF32", listF32}, {"listF64", listF64}, {"listStr", listStr}, {"listBst", listBst}});
                }
                // key_list
                var keyListLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var keyList = new System.Collections.Generic.Dictionary<System.Object, System.Collections.Generic.Dictionary<System.String, System.Object>>(keyListLength);
                while (keyListLength-- > 0)
                {
                    // list_binary
                    var listBinary = reader.ReadBytes(6);
                    // list_boolean
                    var listBoolean = reader.ReadByte() != 0;
                    // list_u8
                    var listU8 = reader.ReadByte();
                    // list_u16
                    var listU16 = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // list_u32
                    var listU32 = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // list_u64
                    var listU64 = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // list_i8
                    var listI8 = reader.ReadSByte();
                    // list_i16
                    var listI16 = System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // list_i32
                    var listI32 = System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // list_i64
                    var listI64 = System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // list_f32
                    var listF32 = System.BitConverter.ToSingle(System.BitConverter.GetBytes(System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32())), 0);
                    // list_f64
                    var listF64 = System.BitConverter.ToDouble(System.BitConverter.GetBytes(System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64())), 0);
                    // list_str
                    var listStrLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var listStr = encoding.GetString(reader.ReadBytes(listStrLength));
                    // list_bst
                    var listBstLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var listBst = encoding.GetString(reader.ReadBytes(listBstLength));
                    // add
                    keyList[listU8] = new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"listBinary", listBinary}, {"listBoolean", listBoolean}, {"listU8", listU8}, {"listU16", listU16}, {"listU32", listU32}, {"listU64", listU64}, {"listI8", listI8}, {"listI16", listI16}, {"listI32", listI32}, {"listI64", listI64}, {"listF32", listF32}, {"listF64", listF64}, {"listStr", listStr}, {"listBst", listBst}};
                }
                return new System.Collections.Generic.Dictionary<System.String, System.Object>() {{"binary", binary}, {"boolean", boolean}, {"u8", u8}, {"u16", u16}, {"u32", u32}, {"u64", u64}, {"i8", i8}, {"i16", i16}, {"i32", i32}, {"i64", i64}, {"f32", f32}, {"f64", f64}, {"str", str}, {"bst", bst}, {"indexList", indexList}, {"keyList", keyList}};
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }
}