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
                const list = [];
                let listLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--listLength >= 0) {
                    // Buff
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
                    const buff = {buffId, expireTime, overlap};
                    // add
                    list.push(buff);
                }
                return {list};
            }
            case 11802: {
                // Buff列表
                const list = [];
                let listLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--listLength >= 0) {
                    // Buff
                    // BuffID
                    const buffId = view.getUint32(offset, false);
                    offset = offset + 4;
                    // object
                    const buff = {buffId};
                    // add
                    list.push(buff);
                }
                return {list};
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }
}