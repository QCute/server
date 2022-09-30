export function encodeDungeonProtocol(textEncoder, view, offset, protocol, data) {
    switch (protocol) {
        case 17002: {
            // extend
            while (view.byteLength < offset + 4) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            // 副本Id
            view.setUint32(offset, data["dungeonId"], false);
            offset = offset + 4;
            return new DataView(view.buffer.slice(0, offset));
        }
        default:throw("unknown protocol define: " + protocol)
    }
}

export function decodeDungeonProtocol(textDecoder, view, offset, protocol) {
    switch (protocol) {
        case 17001: {
            // 
            const list = [];
            let listLength = view.getUint16(offset, false);
            offset = offset + 2;
            while (--listLength >= 0) {
                // 副本Id
                const dungeonId = view.getUint32(offset, false);
                offset = offset + 4;
                // 今天次数
                const todayNumber = view.getUint16(offset, false);
                offset = offset + 2;
                // 总次数
                const totalNumber = view.getUint16(offset, false);
                offset = offset + 2;
                // add
                list.push({dungeonId, todayNumber, totalNumber});
            }
            return {list};
        }
        case 17002: {
            // 结果
            const resultLength = view.getUint16(offset, false);
            offset = offset + 2;
            const resultArray = new Uint8Array(view.buffer.slice(offset, offset + resultLength));
            const result = textDecoder.decode(resultArray);
            offset = offset + resultLength;
            return {result};
        }
        case 17003: {
            // 结果
            const resultLength = view.getUint16(offset, false);
            offset = offset + 2;
            const resultArray = new Uint8Array(view.buffer.slice(offset, offset + resultLength));
            const result = textDecoder.decode(resultArray);
            offset = offset + resultLength;
            return {result};
        }
        case 17004: {
            // 结果
            const resultLength = view.getUint16(offset, false);
            offset = offset + 2;
            const resultArray = new Uint8Array(view.buffer.slice(offset, offset + resultLength));
            const result = textDecoder.decode(resultArray);
            offset = offset + resultLength;
            return {result};
        }
        case 17005: {
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