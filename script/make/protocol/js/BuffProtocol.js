export default class BuffProtocol {
    static encode(textEncoder, view, offset, protocol, data) {
        switch (protocol) {
            case 11801: {
                return new DataView(view.buffer.slice(0, offset));
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }

    static decode(textDecoder, view, offset, protocol) {
        switch (protocol) {
            case 11801: {
                // Buff列表
                const data = [];
                let dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataLength >= 0) {
                    // 
                    // BuffID
                    const buffId = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 结束时间
                    const expireTime = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 叠加数量
                    const overlap = view.getUint16(offset, false);
                    offset = offset + 2;
                    // object
                    const buff = {"buffId": buffId, "expireTime": expireTime, "overlap": overlap};
                    // add
                    data.push(buff);
                }
                return data;
            }
            case 11802: {
                // BuffID列表
                const data = [];
                let dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataLength >= 0) {
                    // BuffID
                    const item = view.getUint32(offset, false);
                    offset = offset + 4;
                    // add
                    data.push(item);
                }
                return data;
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }
}