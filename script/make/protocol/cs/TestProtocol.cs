public class TestTestSingleProtocolRequest
{
    public System.UInt16 protocol = 65531;
    public System.Int16 data;
}

public class TestTestSingleProtocolResponse
{
    public System.UInt16 protocol = 65531;
    public System.Int16 data;
}

public class TestTestListProtocolRequest
{
    public System.UInt16 protocol = 65532;
    public System.Collections.Generic.List<System.UInt32> data;
}

public class TestTestListProtocolResponse
{
    public System.UInt16 protocol = 65532;
    public System.Collections.Generic.List<System.UInt32> data;
}

public class TestTestListTupleProtocolRequest
{
    public System.UInt16 protocol = 65533;
    public System.Collections.Generic.List<(
        System.UInt64 id,                                                       // single u64
        Empty _                                                                 // 
    )> data;
}

public class TestTestListTupleProtocolResponse
{
    public System.UInt16 protocol = 65533;
    public System.Collections.Generic.List<(
        System.String name,                                                     // single ast
        Empty _                                                                 // 
    )> data;
}

public class TestTestLsProtocolRequest
{
    public System.UInt16 protocol = 65534;
    public (
        System.UInt32 id,                                                       // single u32
        (
            System.UInt32 itemId,                                               // 
            System.UInt16 type                                                  // single u16
        ) theItemData,                                                          // 
        (
            System.Collections.Generic.List<(
                System.UInt32 ix,                                               // 
                System.String nx,                                               // 
                System.Collections.Generic.List<System.UInt32> rx,              // 
                System.Collections.Generic.List<(
                    System.UInt32 ii,                                           // a list ii u32
                    System.String nn                                            // a list nn bst
                )> sx                                                           // 
            )> ls,                                                              // 
            System.Collections.Generic.List<System.Byte> lss                    // 
        ) name                                                                  // 
    ) data;
}

public class TestTestLsProtocolResponse
{
    public System.UInt16 protocol = 65534;
    public System.Collections.Generic.List<(
        System.UInt32 name,                                                     // single u32
        Empty _                                                                 // 
    )> data;
}

public class TestTestProtocolRequest
{
    public System.UInt16 protocol = 65535;
    public (
        System.Byte[] binary,                                                   // binary
        System.Boolean boolean,                                                 // bool
        System.Byte u8,                                                         // u8
        System.UInt16 u16,                                                      // u16
        System.UInt32 u32,                                                      // u32
        System.UInt64 u64,                                                      // u64
        System.SByte i8,                                                        // i8
        System.Int16 i16,                                                       // i16
        System.Int32 i32,                                                       // i32
        System.Int64 i64,                                                       // i16
        System.Single f32,                                                      // f32
        System.Double f64,                                                      // f64
        System.String str,                                                      // str
        System.String bst,                                                      // bst
        (
            System.Byte[] binary,                                               // tuple binary
            (
                System.Byte u8,                                                 // tuple tuple u8
                System.String str                                               // tuple tuple str
            ) sub,                                                              // tuple tuple
            System.Collections.Generic.List<(
                System.Int16 i16,                                               // tuple list i16
                System.String bst                                               // tuple list bst
            )> list,                                                            // tuple list
            System.Collections.Generic.List<System.Boolean> single              // 
        ) tuple,                                                                // tuple
        System.Collections.Generic.List<(
            System.Byte[] binary,                                               // tuple binary
            (
                System.Byte u8,                                                 // tuple tuple u8
                System.String str                                               // tuple tuple str
            ) sub,                                                              // tuple tuple
            System.Collections.Generic.List<(
                System.Int16 i16,                                               // tuple list i16
                System.String bst                                               // tuple list bst
            )> list,                                                            // tuple list
            System.Collections.Generic.List<System.Boolean> single              // 
        )> indexList,                                                           // list
        System.Collections.Generic.Dictionary<System.Object, (
            System.Byte[] binary,                                               // binary
            System.Boolean boolean,                                             // bool
            System.Byte u8,                                                     // u8
            System.UInt16 u16,                                                  // u16
            System.UInt32 u32,                                                  // u32
            System.UInt64 u64,                                                  // u64
            System.SByte i8,                                                    // i8
            System.Int16 i16,                                                   // i16
            System.Int32 i32,                                                   // i32
            System.Int64 i64,                                                   // i64
            System.Single f32,                                                  // f32
            System.Double f64,                                                  // f64
            System.String str,                                                  // str
            System.String bst                                                   // bst
        )> keyList                                                              // 
    ) data;
}

public class TestTestProtocolResponse
{
    public System.UInt16 protocol = 65535;
    public (
        System.Byte[] binary,                                                   // binary
        System.Boolean boolean,                                                 // bool
        System.Byte u8,                                                         // u8
        System.UInt16 u16,                                                      // u16
        System.UInt32 u32,                                                      // u32
        System.UInt64 u64,                                                      // u64
        System.SByte i8,                                                        // i8
        System.Int16 i16,                                                       // i16
        System.Int32 i32,                                                       // i32
        System.Int64 i64,                                                       // i16
        System.Single f32,                                                      // f32
        System.Double f64,                                                      // f64
        System.String str,                                                      // str
        System.String bst,                                                      // bst
        (
            System.Byte[] binary,                                               // tuple binary
            (
                System.Byte u8,                                                 // tuple tuple u8
                System.String str                                               // tuple tuple str
            ) sub,                                                              // tuple tuple
            System.Collections.Generic.List<(
                System.Int16 i16,                                               // tuple list i16
                System.String bst                                               // tuple list bst
            )> list,                                                            // tuple list
            System.Collections.Generic.List<System.Boolean> single              // 
        ) tuple,                                                                // tuple
        System.Collections.Generic.List<(
            System.Byte[] binary,                                               // tuple binary
            (
                System.Byte u8,                                                 // tuple tuple u8
                System.String str                                               // tuple tuple str
            ) sub,                                                              // tuple tuple
            System.Collections.Generic.List<(
                System.Int16 i16,                                               // tuple list i16
                System.String bst                                               // tuple list bst
            )> list,                                                            // tuple list
            System.Collections.Generic.List<System.Boolean> single              // 
        )> indexList,                                                           // list
        System.Collections.Generic.Dictionary<System.Object, (
            System.Byte[] binary,                                               // binary
            System.Boolean boolean,                                             // boolean
            System.Byte u8,                                                     // u8
            System.UInt16 u16,                                                  // u16
            System.UInt32 u32,                                                  // u32
            System.UInt64 u64,                                                  // u64
            System.SByte i8,                                                    // i8
            System.Int16 i16,                                                   // i16
            System.Int32 i32,                                                   // i32
            System.Int64 i64,                                                   // i64
            System.Single f32,                                                  // f32
            System.Double f64,                                                  // f64
            System.String str,                                                  // str
            System.String bst                                                   // bst
        )> keyList                                                              // 
    ) data;
}

public static class TestProtocol
{
    public static void Encode(System.Text.Encoding encoding, System.IO.BinaryWriter writer, System.UInt16 protocol, System.Object dataRaw) 
    {
        switch (protocol) 
        {
            case 65531:
            {
                var data = (System.Int16)dataRaw;
                // single i16
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)data));
                return;
            }
            case 65532:
            {
                var data = (System.Collections.Generic.List<System.UInt32>)dataRaw;

                // single list
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)data.Count));
                foreach(var dataData in data)
                {
                    // single u32
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int32)(System.UInt32)dataData));
                }
                return;
            }
            case 65533:
            {
                var data = (System.Collections.Generic.List<(System.UInt64 id, Empty _)>)dataRaw;

                // single list
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)data.Count));
                foreach(var dataData in data)
                {

                    // single u64
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)dataData.id));
                }
                return;
            }
            case 65534:
            {
                var data = ((System.UInt32 id, (System.UInt32 itemId, System.UInt16 type) theItemData, (System.Collections.Generic.List<(System.UInt32 ix, System.String nx, System.Collections.Generic.List<System.UInt32> rx, System.Collections.Generic.List<(System.UInt32 ii, System.String nn)> sx)> ls, System.Collections.Generic.List<System.Byte> lss) name))dataRaw;

                // single u32
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int32)(System.UInt32)data.id));
                var dataTheItemData = data.theItemData;
                // 
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int32)(System.UInt32)dataTheItemData.itemId));
                // single u16
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)(System.UInt16)dataTheItemData.type));
                var dataName = data.name;
                var dataNameLs = dataName.ls;
                // 
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)dataNameLs.Count));
                foreach(var dataNameLsData in dataNameLs)
                {

                    // 
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int32)(System.UInt32)dataNameLsData.ix));
                    // 
                    var dataNameLsDataNxBytes = encoding.GetBytes((System.String)dataNameLsData.nx);
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)dataNameLsDataNxBytes.Length));
                    writer.Write(dataNameLsDataNxBytes);
                    var dataNameLsDataRx = dataNameLsData.rx;
                    // 
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)dataNameLsDataRx.Count));
                    foreach(var dataNameLsDataRxData in dataNameLsDataRx)
                    {
                        // a list u32
                        writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int32)(System.UInt32)dataNameLsDataRxData));
                    }
                    var dataNameLsDataSx = dataNameLsData.sx;
                    // 
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)dataNameLsDataSx.Count));
                    foreach(var dataNameLsDataSxData in dataNameLsDataSx)
                    {

                        // a list ii u32
                        writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int32)(System.UInt32)dataNameLsDataSxData.ii));
                        // a list nn bst
                        var dataNameLsDataSxDataNnBytes = encoding.GetBytes((System.String)dataNameLsDataSxData.nn);
                        writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)dataNameLsDataSxDataNnBytes.Length));
                        writer.Write(dataNameLsDataSxDataNnBytes);
                    }
                }
                var dataNameLss = dataName.lss;
                // 
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)dataNameLss.Count));
                foreach(var dataNameLssData in dataNameLss)
                {
                    // single u8
                    writer.Write((System.Byte)dataNameLssData);
                }
                return;
            }
            case 65535:
            {
                var data = ((System.Byte[] binary, System.Boolean boolean, System.Byte u8, System.UInt16 u16, System.UInt32 u32, System.UInt64 u64, System.SByte i8, System.Int16 i16, System.Int32 i32, System.Int64 i64, System.Single f32, System.Double f64, System.String str, System.String bst, (System.Byte[] binary, (System.Byte u8, System.String str) sub, System.Collections.Generic.List<(System.Int16 i16, System.String bst)> list, System.Collections.Generic.List<System.Boolean> single) tuple, System.Collections.Generic.List<(System.Byte[] binary, (System.Byte u8, System.String str) sub, System.Collections.Generic.List<(System.Int16 i16, System.String bst)> list, System.Collections.Generic.List<System.Boolean> single)> indexList, System.Collections.Generic.Dictionary<System.Object, (System.Byte[] binary, System.Boolean boolean, System.Byte u8, System.UInt16 u16, System.UInt32 u32, System.UInt64 u64, System.SByte i8, System.Int16 i16, System.Int32 i32, System.Int64 i64, System.Single f32, System.Double f64, System.String str, System.String bst)> keyList))dataRaw;

                // binary
                writer.Write((System.Byte[])data.binary);
                // bool
                writer.Write((System.Byte)((System.Boolean)data.boolean ? 1 : 0));
                // u8
                writer.Write((System.Byte)data.u8);
                // u16
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)(System.UInt16)data.u16));
                // u32
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int32)(System.UInt32)data.u32));
                // u64
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)data.u64));
                // i8
                writer.Write((System.SByte)data.i8);
                // i16
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)data.i16));
                // i32
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int32)data.i32));
                // i16
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)data.i64));
                // f32
                var dataF32Bytes = System.BitConverter.GetBytes((System.Single)data.f32);
                if (System.BitConverter.IsLittleEndian) System.Array.Reverse(dataF32Bytes);
                writer.Write(dataF32Bytes);
                // f64
                var dataF64Bytes = System.BitConverter.GetBytes((System.Double)data.f64);
                if (System.BitConverter.IsLittleEndian) System.Array.Reverse(dataF64Bytes);
                writer.Write(dataF64Bytes);
                // str
                var dataStrBytes = encoding.GetBytes((System.String)data.str);
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)dataStrBytes.Length));
                writer.Write(dataStrBytes);
                // bst
                var dataBstBytes = encoding.GetBytes((System.String)data.bst);
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)dataBstBytes.Length));
                writer.Write(dataBstBytes);
                var dataTuple = data.tuple;
                // tuple binary
                writer.Write((System.Byte[])dataTuple.binary);
                var dataTupleSub = dataTuple.sub;
                // tuple tuple u8
                writer.Write((System.Byte)dataTupleSub.u8);
                // tuple tuple str
                var dataTupleSubStrBytes = encoding.GetBytes((System.String)dataTupleSub.str);
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)dataTupleSubStrBytes.Length));
                writer.Write(dataTupleSubStrBytes);
                var dataTupleList = dataTuple.list;
                // tuple list
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)dataTupleList.Count));
                foreach(var dataTupleListData in dataTupleList)
                {

                    // tuple list i16
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)dataTupleListData.i16));
                    // tuple list bst
                    var dataTupleListDataBstBytes = encoding.GetBytes((System.String)dataTupleListData.bst);
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)dataTupleListDataBstBytes.Length));
                    writer.Write(dataTupleListDataBstBytes);
                }
                var dataTupleSingle = dataTuple.single;
                // 
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)dataTupleSingle.Count));
                foreach(var dataTupleSingleData in dataTupleSingle)
                {
                    // bool
                    writer.Write((System.Byte)((System.Boolean)dataTupleSingleData ? 1 : 0));
                }
                var dataIndexList = data.indexList;
                // list
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)dataIndexList.Count));
                foreach(var dataIndexListData in dataIndexList)
                {

                    // tuple binary
                    writer.Write((System.Byte[])dataIndexListData.binary);
                    var dataIndexListDataSub = dataIndexListData.sub;
                    // tuple tuple u8
                    writer.Write((System.Byte)dataIndexListDataSub.u8);
                    // tuple tuple str
                    var dataIndexListDataSubStrBytes = encoding.GetBytes((System.String)dataIndexListDataSub.str);
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)dataIndexListDataSubStrBytes.Length));
                    writer.Write(dataIndexListDataSubStrBytes);
                    var dataIndexListDataList = dataIndexListData.list;
                    // tuple list
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)dataIndexListDataList.Count));
                    foreach(var dataIndexListDataListData in dataIndexListDataList)
                    {

                        // tuple list i16
                        writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)dataIndexListDataListData.i16));
                        // tuple list bst
                        var dataIndexListDataListDataBstBytes = encoding.GetBytes((System.String)dataIndexListDataListData.bst);
                        writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)dataIndexListDataListDataBstBytes.Length));
                        writer.Write(dataIndexListDataListDataBstBytes);
                    }
                    var dataIndexListDataSingle = dataIndexListData.single;
                    // 
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)dataIndexListDataSingle.Count));
                    foreach(var dataIndexListDataSingleData in dataIndexListDataSingle)
                    {
                        // bool
                        writer.Write((System.Byte)((System.Boolean)dataIndexListDataSingleData ? 1 : 0));
                    }
                }
                var dataKeyList = data.keyList;
                // 
                writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)dataKeyList.Count));
                foreach(var dataKeyListData in dataKeyList.Values)
                {

                    // binary
                    writer.Write((System.Byte[])dataKeyListData.binary);
                    // bool
                    writer.Write((System.Byte)((System.Boolean)dataKeyListData.boolean ? 1 : 0));
                    // u8
                    writer.Write((System.Byte)dataKeyListData.u8);
                    // u16
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)(System.UInt16)dataKeyListData.u16));
                    // u32
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int32)(System.UInt32)dataKeyListData.u32));
                    // u64
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)(System.UInt64)dataKeyListData.u64));
                    // i8
                    writer.Write((System.SByte)dataKeyListData.i8);
                    // i16
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)dataKeyListData.i16));
                    // i32
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int32)dataKeyListData.i32));
                    // i64
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int64)dataKeyListData.i64));
                    // f32
                    var dataKeyListDataF32Bytes = System.BitConverter.GetBytes((System.Single)dataKeyListData.f32);
                    if (System.BitConverter.IsLittleEndian) System.Array.Reverse(dataKeyListDataF32Bytes);
                    writer.Write(dataKeyListDataF32Bytes);
                    // f64
                    var dataKeyListDataF64Bytes = System.BitConverter.GetBytes((System.Double)dataKeyListData.f64);
                    if (System.BitConverter.IsLittleEndian) System.Array.Reverse(dataKeyListDataF64Bytes);
                    writer.Write(dataKeyListDataF64Bytes);
                    // str
                    var dataKeyListDataStrBytes = encoding.GetBytes((System.String)dataKeyListData.str);
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)dataKeyListDataStrBytes.Length));
                    writer.Write(dataKeyListDataStrBytes);
                    // bst
                    var dataKeyListDataBstBytes = encoding.GetBytes((System.String)dataKeyListData.bst);
                    writer.Write(System.Net.IPAddress.HostToNetworkOrder((System.Int16)dataKeyListDataBstBytes.Length));
                    writer.Write(dataKeyListDataBstBytes);
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
            case 65531:
            {
                // single i16
                var data = System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                return (protocol: 65531, data: data);
            }
            case 65532:
            {
                // single list
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<System.UInt32>(dataLength);
                while (dataLength-- > 0)
                {
                    // single u32
                    var dataData = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // add
                    data.Add(dataData);
                }
                return (protocol: 65532, data: data);
            }
            case 65533:
            {
                // single list
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<(System.String name, Empty _)>(dataLength);
                while (dataLength-- > 0)
                {
                    // 
                    // single ast
                    var dataDataNameLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var dataDataName = encoding.GetString(reader.ReadBytes(dataDataNameLength));
                    // object
                    var dataData = (name: dataDataName, _: new Empty());
                    // add
                    data.Add(dataData);
                }
                return (protocol: 65533, data: data);
            }
            case 65534:
            {
                // single list
                var dataLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var data = new System.Collections.Generic.List<(System.UInt32 name, Empty _)>(dataLength);
                while (dataLength-- > 0)
                {
                    // 
                    // single u32
                    var dataDataName = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // object
                    var dataData = (name: dataDataName, _: new Empty());
                    // add
                    data.Add(dataData);
                }
                return (protocol: 65534, data: data);
            }
            case 65535:
            {
                // 
                // binary
                var dataBinary = reader.ReadBytes(6);
                // bool
                var dataBoolean = reader.ReadByte() != 0;
                // u8
                var dataU8 = reader.ReadByte();
                // u16
                var dataU16 = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                // u32
                var dataU32 = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                // u64
                var dataU64 = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // i8
                var dataI8 = reader.ReadSByte();
                // i16
                var dataI16 = System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                // i32
                var dataI32 = System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                // i16
                var dataI64 = System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                // f32
                var dataF32 = System.BitConverter.ToSingle(System.BitConverter.GetBytes(System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32())), 0);
                // f64
                var dataF64 = System.BitConverter.ToDouble(System.BitConverter.GetBytes(System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64())), 0);
                // str
                var dataStrLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var dataStr = encoding.GetString(reader.ReadBytes(dataStrLength));
                // bst
                var dataBstLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var dataBst = encoding.GetString(reader.ReadBytes(dataBstLength));
                // tuple
                // tuple binary
                var dataTupleBinary = reader.ReadBytes(6);
                // tuple tuple
                // tuple tuple u8
                var dataTupleSubU8 = reader.ReadByte();
                // tuple tuple str
                var dataTupleSubStrLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var dataTupleSubStr = encoding.GetString(reader.ReadBytes(dataTupleSubStrLength));
                // object
                var dataTupleSub = (u8: dataTupleSubU8, str: dataTupleSubStr);
                // tuple list
                var dataTupleListLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var dataTupleList = new System.Collections.Generic.List<(System.Int16 i16, System.String bst)>(dataTupleListLength);
                while (dataTupleListLength-- > 0)
                {
                    // 
                    // tuple list i16
                    var dataTupleListDataI16 = System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // tuple list bst
                    var dataTupleListDataBstLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var dataTupleListDataBst = encoding.GetString(reader.ReadBytes(dataTupleListDataBstLength));
                    // object
                    var dataTupleListData = (i16: dataTupleListDataI16, bst: dataTupleListDataBst);
                    // add
                    dataTupleList.Add(dataTupleListData);
                }
                // 
                var dataTupleSingleLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var dataTupleSingle = new System.Collections.Generic.List<System.Boolean>(dataTupleSingleLength);
                while (dataTupleSingleLength-- > 0)
                {
                    // bool
                    var dataTupleSingleData = reader.ReadByte() != 0;
                    // add
                    dataTupleSingle.Add(dataTupleSingleData);
                }
                // object
                var dataTuple = (binary: dataTupleBinary, sub: dataTupleSub, list: dataTupleList, single: dataTupleSingle);
                // list
                var dataIndexListLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var dataIndexList = new System.Collections.Generic.List<(System.Byte[] binary, (System.Byte u8, System.String str) sub, System.Collections.Generic.List<(System.Int16 i16, System.String bst)> list, System.Collections.Generic.List<System.Boolean> single)>(dataIndexListLength);
                while (dataIndexListLength-- > 0)
                {
                    // 
                    // tuple binary
                    var dataIndexListDataBinary = reader.ReadBytes(6);
                    // tuple tuple
                    // tuple tuple u8
                    var dataIndexListDataSubU8 = reader.ReadByte();
                    // tuple tuple str
                    var dataIndexListDataSubStrLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var dataIndexListDataSubStr = encoding.GetString(reader.ReadBytes(dataIndexListDataSubStrLength));
                    // object
                    var dataIndexListDataSub = (u8: dataIndexListDataSubU8, str: dataIndexListDataSubStr);
                    // tuple list
                    var dataIndexListDataListLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var dataIndexListDataList = new System.Collections.Generic.List<(System.Int16 i16, System.String bst)>(dataIndexListDataListLength);
                    while (dataIndexListDataListLength-- > 0)
                    {
                        // 
                        // tuple list i16
                        var dataIndexListDataListDataI16 = System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                        // tuple list bst
                        var dataIndexListDataListDataBstLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                        var dataIndexListDataListDataBst = encoding.GetString(reader.ReadBytes(dataIndexListDataListDataBstLength));
                        // object
                        var dataIndexListDataListData = (i16: dataIndexListDataListDataI16, bst: dataIndexListDataListDataBst);
                        // add
                        dataIndexListDataList.Add(dataIndexListDataListData);
                    }
                    // 
                    var dataIndexListDataSingleLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var dataIndexListDataSingle = new System.Collections.Generic.List<System.Boolean>(dataIndexListDataSingleLength);
                    while (dataIndexListDataSingleLength-- > 0)
                    {
                        // bool
                        var dataIndexListDataSingleData = reader.ReadByte() != 0;
                        // add
                        dataIndexListDataSingle.Add(dataIndexListDataSingleData);
                    }
                    // object
                    var dataIndexListData = (binary: dataIndexListDataBinary, sub: dataIndexListDataSub, list: dataIndexListDataList, single: dataIndexListDataSingle);
                    // add
                    dataIndexList.Add(dataIndexListData);
                }
                // 
                var dataKeyListLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                var dataKeyList = new System.Collections.Generic.Dictionary<System.Object, (System.Byte[] binary, System.Boolean boolean, System.Byte u8, System.UInt16 u16, System.UInt32 u32, System.UInt64 u64, System.SByte i8, System.Int16 i16, System.Int32 i32, System.Int64 i64, System.Single f32, System.Double f64, System.String str, System.String bst)>(dataKeyListLength);
                while (dataKeyListLength-- > 0)
                {
                    // 
                    // binary
                    var dataKeyListDataBinary = reader.ReadBytes(6);
                    // boolean
                    var dataKeyListDataBoolean = reader.ReadByte() != 0;
                    // u8
                    var dataKeyListDataU8 = reader.ReadByte();
                    // u16
                    var dataKeyListDataU16 = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // u32
                    var dataKeyListDataU32 = (System.UInt32)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // u64
                    var dataKeyListDataU64 = (System.UInt64)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // i8
                    var dataKeyListDataI8 = reader.ReadSByte();
                    // i16
                    var dataKeyListDataI16 = System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    // i32
                    var dataKeyListDataI32 = System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32());
                    // i64
                    var dataKeyListDataI64 = System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64());
                    // f32
                    var dataKeyListDataF32 = System.BitConverter.ToSingle(System.BitConverter.GetBytes(System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt32())), 0);
                    // f64
                    var dataKeyListDataF64 = System.BitConverter.ToDouble(System.BitConverter.GetBytes(System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt64())), 0);
                    // str
                    var dataKeyListDataStrLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var dataKeyListDataStr = encoding.GetString(reader.ReadBytes(dataKeyListDataStrLength));
                    // bst
                    var dataKeyListDataBstLength = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(reader.ReadInt16());
                    var dataKeyListDataBst = encoding.GetString(reader.ReadBytes(dataKeyListDataBstLength));
                    // object
                    var dataKeyListData = (binary: dataKeyListDataBinary, boolean: dataKeyListDataBoolean, u8: dataKeyListDataU8, u16: dataKeyListDataU16, u32: dataKeyListDataU32, u64: dataKeyListDataU64, i8: dataKeyListDataI8, i16: dataKeyListDataI16, i32: dataKeyListDataI32, i64: dataKeyListDataI64, f32: dataKeyListDataF32, f64: dataKeyListDataF64, str: dataKeyListDataStr, bst: dataKeyListDataBst);
                    // add
                    dataKeyList[dataKeyListDataU8] = dataKeyListData;
                }
                // object
                var data = (binary: dataBinary, boolean: dataBoolean, u8: dataU8, u16: dataU16, u32: dataU32, u64: dataU64, i8: dataI8, i16: dataI16, i32: dataI32, i64: dataI64, f32: dataF32, f64: dataF64, str: dataStr, bst: dataBst, tuple: dataTuple, indexList: dataIndexList, keyList: dataKeyList);
                return (protocol: 65535, data: data);
            }
            default:throw new System.ArgumentException(System.String.Format("unknown protocol define: {0}", protocol));
        }
    }
}