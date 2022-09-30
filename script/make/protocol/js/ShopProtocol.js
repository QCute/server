export function encodeShopProtocol(textEncoder, view, offset, protocol, data) {
    switch (protocol) {
        case 11302: {
            // extend
            while (view.byteLength < offset + 4) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            // 商店ID
            view.setUint32(offset, data["shopId"], false);
            offset = offset + 4;
            // extend
            while (view.byteLength < offset + 2) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            // 数量
            view.setUint16(offset, data["number"], false);
            offset = offset + 2;
            return new DataView(view.buffer.slice(0, offset));
        }
        default:throw("unknown protocol define: " + protocol)
    }
}

export function decodeShopProtocol(textDecoder, view, offset, protocol) {
    switch (protocol) {
        case 11301: {
            // 已购买列表
            const list = [];
            let listLength = view.getUint16(offset, false);
            offset = offset + 2;
            while (--listLength >= 0) {
                // 商店ID
                const shopId = view.getUint32(offset, false);
                offset = offset + 4;
                // 数量
                const number = view.getUint16(offset, false);
                offset = offset + 2;
                // add
                list.push({shopId, number});
            }
            return {list};
        }
        case 11302: {
            // 结果
            const resultLength = view.getUint16(offset, false);
            offset = offset + 2;
            const resultArray = new Uint8Array(view.buffer.slice(offset, offset + resultLength));
            const result = textDecoder.decode(resultArray);
            offset = offset + resultLength;
            return {result};
        }
        default:throw("unknown protocol define: " + protocol)
    }
}