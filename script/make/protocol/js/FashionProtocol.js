export default class FashionProtocol {
    static encode(textEncoder, view, offset, protocol, data) {
        switch (protocol) {
            case 12001: {
                return new DataView(view.buffer.slice(0, offset));
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }

    static decode(textDecoder, view, offset, protocol) {
        switch (protocol) {
            case 12001: {
                // 时装列表
                const list = [];
                let listLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--listLength >= 0) {
                    // 时装ID
                    const fashionId = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 过期时间
                    const expireTime = view.getUint32(offset, false);
                    offset = offset + 4;
                    // add
                    list.push({fashionId, expireTime});
                }
                return {list};
            }
            case 12002: {
                // 时装ID列表
                const list = [];
                let listLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--listLength >= 0) {
                    // 时装ID
                    const fashionId = view.getUint32(offset, false);
                    offset = offset + 4;
                    // add
                    list.push({fashionId});
                }
                return {list};
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }
}