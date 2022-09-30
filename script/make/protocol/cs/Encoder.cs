public class ProtocolEncoder
{
    System.Text.Encoding encoding = new System.Text.UTF8Encoding(false);

    public byte[] Encode(System.UInt16 protocol, System.Collections.Generic.Dictionary<System.String, System.Object> data)
    {
        var stream = new System.IO.MemoryStream(1024);
        var writer = new System.IO.BinaryWriter(stream);
        writer.Seek(4, System.IO.SeekOrigin.Begin);
        ProtocolRouter.Encode(this.encoding, writer, protocol, data);
        var length = stream.Position - 4;
        writer.Seek(0, System.IO.SeekOrigin.Begin);
        writer.Write((System.UInt16)length);
        writer.Write((System.UInt16)protocol);
        return stream.ToArray();
    }
}
