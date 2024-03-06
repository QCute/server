export default class TitleProtocol {
    static encode(textEncoder, view, offset, protocol, data) {
        switch (protocol) {
            case 11901: {
                return new DataView(view.buffer.slice(0, offset));
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }

    static decode(textDecoder, view, offset, protocol) {
        switch (protocol) {
            case 11901: {
                // 称号列表
                const list = [];
                let listLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--listLength >= 0) {
                    // 称号ID
                    const titleId = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 过期时间
                    const expireTime = view.getUint32(offset, false);
                    offset = offset + 4;
                    // add
                    list.push({titleId, expireTime});
                }
                return {list};
            }
            case 11902: {
                // 称号ID列表
                const list = [];
                let listLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--listLength >= 0) {
                    // 称号ID
                    const titleId = view.getUint32(offset, false);
                    offset = offset + 4;
                    // add
                    list.push({titleId});
                }
                return {list};
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }
}