
class Test
{
    public static void Main(System.String[] args)
    {
        TestEncoderDecoder();
        TestNetworkEncoderDecoder();
    }

    static TestFullRequest packet = new TestFullRequest() {
        protocol = (System.UInt16)65535,
        data = (
            binary:  new byte [] {97, 98, 99, 100, 101, 102},
            boolean:  true,

            u8:  (System.Byte)1,
            u16:  (System.UInt16)2,
            u32:  (System.UInt32)3,
            u64:  (System.UInt64)4,

            i8:  (System.SByte)4,
            i16:  (System.Int16)3,
            i32:  (System.Int32)2,
            i64:  (System.Int64)1,

            f32:  (System.Single)1.23,
            f64:  (System.Double)4.56,

            str:  "一23",
            bst:  "1二三",

            tuple: (
                binary:  new byte [] {97, 98, 99, 100, 101, 102},
                sub: (
                    u8:  (System.Byte)95,
                    str:  "xyz"
                ),
                list:  new System.Collections.Generic.List<(System.Int16 i16, string bst)>() {
                    (i16:  (System.Int16)456, bst:  "wow"),
                    (i16:  (System.Int16)369, bst:  "oops"),
                },
                single:  new System.Collections.Generic.List<System.Boolean>() {true, false, false, true, false}
            ),

            indexList:  new System.Collections.Generic.List<(System.Byte[] binary, (System.Byte u8, System.String str) sub, System.Collections.Generic.List<(System.Int16 i16, System.String bst)> list, System.Collections.Generic.List<System.Boolean> single)>() {
                (
                    binary:  new byte [] {97, 98, 99, 100, 101, 102},
                    sub:  (
                        u8:  (System.Byte)95,
                        str:  "xyz"
                    ),
                    list:  new System.Collections.Generic.List<(System.Int16 i16, string bst)>() {
                        (i16:  (System.Int16)456, bst:  "wow"),
                        (i16:  (System.Int16)369, bst:  "oops")
                    },
                    single:  new System.Collections.Generic.List<System.Boolean>() {true, false, false, true, false}
                )
            },

            keyList: new System.Collections.Generic.Dictionary<System.Object, (System.Byte[] binary, System.Boolean boolean, System.Byte u8, System.UInt16 u16, System.UInt32 u32, System.UInt64 u64, System.SByte i8, System.Int16 i16, System.Int32 i32, System.Int64 i64, System.Single f32, System.Double f64, System.String str, System.String bst)>() {
                {(System.Object)1, (
                    binary:  new byte [] {97, 98, 99, 100, 101, 102},
                    boolean:  true,

                    u8:  (System.Byte)1,
                    u16:  (System.UInt16)2,
                    u32:  (System.UInt32)3,
                    u64:  (System.UInt64)4,

                    i8:  (System.SByte)4,
                    i16:  (System.Int16)3,
                    i32:  (System.Int32)2,
                    i64:  (System.Int64)1,

                    f32:  (System.Single)1.23,
                    f64:  (System.Double)4.56,

                    str:  "一23",
                    bst:  "1二三"
                )}
            }
        )
    };

    public static void TestEncoderDecoder()
    {
        System.Console.WriteLine(Stringify(packet));
        System.Console.WriteLine();
        var encoder = new Encoder();
        var buffer = encoder.Encode((System.UInt16)packet.protocol, packet.data);
        System.Console.WriteLine(Dump(buffer));
        System.Console.WriteLine();
        var stream = new System.IO.MemoryStream(buffer.Length * 2);
        stream.Position = 0;
        stream.Write(buffer, 0, buffer.Length);
        stream.Write(buffer, 0, buffer.Length);
        buffer = stream.GetBuffer();
        var decoder = new Decoder();
        for(int i = 0; i < buffer.Length; i+=10)
        {
            // var result = decoder.Decode(new System.ArraySegment<byte>(buffer, i, System.Math.Min(10, buffer.Length - i)));
            var data = new System.ArraySegment<byte>(buffer, i, System.Math.Min(10, buffer.Length - i));
            var result = decoder.AppendData(data).Decode();
            if(result == null)continue;
            System.Console.WriteLine(Stringify(result));
            // Assert
            System.Diagnostics.Debug.Assert(Stringify(packet) == Stringify(result));
            System.Console.WriteLine();
        }
    }

    public static void TestNetworkEncoderDecoder()
    {

        var socket = new System.Net.Sockets.Socket(System.Net.Sockets.AddressFamily.InterNetwork, System.Net.Sockets.SocketType.Stream, System.Net.Sockets.ProtocolType.Tcp);
        socket.Connect("127.0.0.1", 33333);

        var encoder = new Encoder();
        var buffer = encoder.Encode((System.UInt16)packet.protocol, packet.data);
        System.Console.WriteLine(Dump(buffer));
        System.Console.WriteLine();
        socket.Send(buffer, 0, (int)buffer.Length, System.Net.Sockets.SocketFlags.None);

        byte[] data = new byte[1024];
        var decoder = new Decoder();
        while (true)
        {
            int count = socket.Receive(data);
            decoder.AppendData(new System.ArraySegment<byte>(data, 0, count));
            while(true)
            {
                var result = decoder.Decode();
                if(result == null)break;
                System.Console.WriteLine(Stringify(result));
            }
        }
    }

    public static System.String Dump(byte[] data)
    {
        var str = "[";
        foreach (var i in data)
        {
            str = str + i + ", ";
        }
        return str + "]";
    }

    public static string Stringify(object obj)
    {
        if (obj == null)
        {
            return "null";
        }

        var sb = new System.Text.StringBuilder();
        int indentation = 0;
        var indentationString = "    ";


        string GetIndentation()
        {
            return new string(' ', indentation * indentationString.Length);
        }

        void DumpInternal(object currentObj)
        {
            if (currentObj == null)
            {
                sb.Append("null");
                return;
            }

            System.Type type = currentObj.GetType();

            if (type.IsPrimitive || currentObj is string || currentObj is decimal)
            {
                sb.Append(currentObj.ToString());
                return;
            }

            if (obj is System.Collections.IDictionary dictionary)
            {
                sb.AppendLine("{");
                indentation++;
                var isFirst = true;
                foreach (System.Collections.DictionaryEntry entry in dictionary)
                {
                    if (!isFirst) sb.AppendLine(",");
                    sb.Append(GetIndentation());
                    sb.Append($"\"{entry.Key}\": ");
                    DumpInternal(entry.Value);
                    isFirst = false;
                }
                indentation--;
                sb.AppendLine();
                sb.Append(GetIndentation()).Append("}");
                return;
            }

            if (currentObj is System.Collections.IList list)
            {
                sb.AppendLine("[");
                indentation++;
                foreach (var item in list)
                {
                    sb.Append(GetIndentation());
                    DumpInternal(item);
                    sb.AppendLine(",");
                }
                indentation--;
                if (list.Count > 0)
                {
                    sb.Length -= System.Environment.NewLine.Length + 1;
                }
                sb.AppendLine();
                sb.Append(GetIndentation()).Append("]");
                return;
            }

            sb.AppendLine("{");
            indentation++;

            var fields = type.GetFields(System.Reflection.BindingFlags.Public | System.Reflection.BindingFlags.Instance);
            foreach (var field in fields)
            {
                sb.Append(GetIndentation());
                sb.Append($"\"{field.Name}\": ");
                object fieldValue = field.GetValue(currentObj);
                DumpInternal(fieldValue);
                sb.AppendLine(",");
            }
            indentation--;
            if (fields.Length > 0)
            {
                sb.Length -= System.Environment.NewLine.Length + 1;
            }
            sb.AppendLine();
            sb.Append(GetIndentation()).Append("}");
        }

        DumpInternal(obj);
        return sb.ToString();
    }
}
