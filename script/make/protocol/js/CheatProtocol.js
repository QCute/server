export function encodeCheatProtocol(textEncoder, view, offset, protocol, data) {
    switch (protocol) {
        case 60002: {
            // extend
            while (view.byteLength < offset + 2) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            // 命令
            const commandArray = textEncoder.encode(data["command"]);
            view.setUint16(offset, commandArray.length, false);
            offset = offset + 2;
            // extend
            while (view.byteLength < offset + commandArray.length) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            (new Uint8Array(view.buffer, offset)).set(commandArray);
            offset = offset + commandArray.length;
            return new DataView(view.buffer.slice(0, offset));
        }
        default:throw("unknown protocol define: " + protocol)
    }
}

export function decodeCheatProtocol(textDecoder, view, offset, protocol) {
    switch (protocol) {
        case 60001: {
            // 秘籍列表
            const cheatList = [];
            let cheatListLength = view.getUint16(offset, false);
            offset = offset + 2;
            while (--cheatListLength >= 0) {
                // 描述
                const descriptionLength = view.getUint16(offset, false);
                offset = offset + 2;
                const descriptionArray = new Uint8Array(view.buffer.slice(offset, offset + descriptionLength));
                const description = textDecoder.decode(descriptionArray);
                offset = offset + descriptionLength;
                // 命令
                const commandLength = view.getUint16(offset, false);
                offset = offset + 2;
                const commandArray = new Uint8Array(view.buffer.slice(offset, offset + commandLength));
                const command = textDecoder.decode(commandArray);
                offset = offset + commandLength;
                // add
                cheatList.push({description, command});
            }
            return {cheatList};
        }
        case 60002: {
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