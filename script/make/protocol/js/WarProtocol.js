export function encodeWarProtocol(textEncoder, view, offset, protocol, data) {
    switch (protocol) {
        case 18001: {
            // extend
            while (view.byteLength < offset + 4) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            // 怪物Id
            view.setUint32(offset, data["monsterId"], false);
            offset = offset + 4;
            return new DataView(view.buffer.slice(0, offset));
        }
        default:throw("unknown protocol define: " + protocol)
    }
}

export function decodeWarProtocol(textDecoder, view, offset, protocol) {
    switch (protocol) {
        case 18001: {
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