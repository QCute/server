export class CheatQueryRequest {
    /** @type {number} protocol **/
    protocol = 60001;
    /**
     * @type {{
     * }} data
    **/
    data;
}

export class CheatQueryResponse {
    /** @type {number} protocol **/
    protocol = 60001;
    /**
     * @type {Array<{
     *     description: string;                                                                     // 描述
     *     command: string;                                                                         // 命令
     * }>} data
    **/
    data;
}

export class CheatCheatRequest {
    /** @type {number} protocol **/
    protocol = 60002;
    /**
     * @type {string} data
    **/
    data;
}

export class CheatCheatResponse {
    /** @type {number} protocol **/
    protocol = 60002;
    /**
     * @type {string} data
    **/
    data;
}

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
                    const dataDataDescriptionLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    const dataDataDescriptionArray = new Uint8Array(view.buffer.slice(offset, offset + dataDataDescriptionLength));
                    const dataDataDescription = textDecoder.decode(dataDataDescriptionArray);
                    offset = offset + dataDataDescriptionLength;
                    // 命令
                    const dataDataCommandLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    const dataDataCommandArray = new Uint8Array(view.buffer.slice(offset, offset + dataDataCommandLength));
                    const dataDataCommand = textDecoder.decode(dataDataCommandArray);
                    offset = offset + dataDataCommandLength;
                    // object
                    const dataData = {"description": dataDataDescription, "command": dataDataCommand};
                    // add
                    data.push(dataData);
                }
                return {"protocol": 60001, "data": data};
            }
            case 60002: {
                // 结果
                const dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataArray = new Uint8Array(view.buffer.slice(offset, offset + dataLength));
                const data = textDecoder.decode(dataArray);
                offset = offset + dataLength;
                return {"protocol": 60002, "data": data};
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }
}