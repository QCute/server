public class Decoder
{
    System.Int32 Length = 0;
    System.Text.Encoding encoding = new System.Text.UTF8Encoding(false);
    System.IO.MemoryStream stream = new System.IO.MemoryStream(1024);
    System.IO.BinaryReader reader = new System.IO.BinaryReader(new System.IO.MemoryStream(0));

    public Decoder AppendData(System.ArraySegment<byte> segment)
    {
        this.stream.Position = this.Length;
        this.stream.Write(segment.Array, segment.Offset, segment.Count);
        this.stream.Position = 0;
        this.reader = new System.IO.BinaryReader(this.stream);
        this.Length = this.Length + segment.Count;
        return this;
    }

    public System.Collections.Generic.Dictionary<System.String, System.Object> Decode()
    {
        // @tag protocol data length 2 bytes(without header 4 byte), protocol 2 bytes
        if(this.Length >= 4)
        {
            var length = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(this.reader.ReadInt16());
            if(this.Length >= 4 + length)
            {
                var protocol = (System.UInt16)System.Net.IPAddress.NetworkToHostOrder(this.reader.ReadInt16());
                var packet = this.reader.ReadBytes(length);
                this.Length = this.Length - length - 4;
                var reader = new System.IO.BinaryReader(new System.IO.MemoryStream(packet));
                var data = ProtocolRouter.Decode(this.encoding, reader, protocol);
                this.stream.Write(this.stream.GetBuffer(), length + 4, this.Length);
                return new System.Collections.Generic.Dictionary<System.String, System.Object>() { {"protocol", protocol}, {"data", data} };
            }
        }
        return null;
    }
}
