export function encodeBubbleProtocol(textEncoder, view, offset, protocol, data) {
    switch (protocol) {

        default:throw("unknown protocol define: " + protocol)
    }
}

export function decodeBubbleProtocol(textDecoder, view, offset, protocol) {
    switch (protocol) {
        case 12101: {
            // 气泡列表
            const list = [];
            let listLength = view.getUint16(offset, false);
            offset = offset + 2;
            while (--listLength >= 0) {
                // 气泡ID
                const bubbleId = view.getUint32(offset, false);
                offset = offset + 4;
                // 过期时间
                const expireTime = view.getUint32(offset, false);
                offset = offset + 4;
                // add
                list.push({bubbleId, expireTime});
            }
            return {list};
        }
        case 12102: {
            // 气泡ID列表
            const list = [];
            let listLength = view.getUint16(offset, false);
            offset = offset + 2;
            while (--listLength >= 0) {
                // 气泡ID
                const bubbleId = view.getUint32(offset, false);
                offset = offset + 4;
                // add
                list.push({bubbleId});
            }
            return {list};
        }
        default:throw("unknown protocol define: " + protocol)
    }
}