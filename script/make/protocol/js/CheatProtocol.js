export default class CheatProtocol {
    static encode(textEncoder, view, offset, protocol, data) {
        switch (protocol) {
            case 60001: {
                return new DataView(view.buffer.slice(0, offset));
            }
            case 60002: {
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 命令
                const dataArray = textEncoder.encode(data);
                view.setUint16(offset, dataArray.length, false);
                offset = offset + 2;
                // extend
                while (view.byteLength < offset + dataArray.length) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                (new Uint8Array(view.buffer, offset)).set(dataArray);
                offset = offset + dataArray.length;
                return new DataView(view.buffer.slice(0, offset));
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }

    static decode(textDecoder, view, offset, protocol) {
        switch (protocol) {
            case 60001: {
                // 命令列表
                const data = [];
                let dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataLength >= 0) {
                    // 
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
                    // object
                    const item = {"description": description, "command": command};
                    // add
                    data.push(item);
                }
                return data;
            }
            case 60002: {
                // 结果
                const dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataArray = new Uint8Array(view.buffer.slice(offset, offset + dataLength));
                const data = textDecoder.decode(dataArray);
                offset = offset + dataLength;
                return data;
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }
}