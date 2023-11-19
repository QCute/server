export class WarBattleRequest {
    /** @type {number} protocol **/
    protocol = 18001;
    /**
     * @type {number} data
    **/
    data;
}

export class WarBattleResponse {
    /** @type {number} protocol **/
    protocol = 18001;
    /**
     * @type {string} data
    **/
    data;
}

export default class WarProtocol {
    static encode(textEncoder, view, offset, protocol, data) {
        switch (protocol) {
            case 18001: {
                // extend
                while (view.byteLength < offset + 4) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 怪物Id
                view.setUint32(offset, data, false);
                offset = offset + 4;
                return new DataView(view.buffer.slice(0, offset));
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }

    static decode(textDecoder, view, offset, protocol) {
        switch (protocol) {
            case 18001: {
                // 结果
                const dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataArray = new Uint8Array(view.buffer.slice(offset, offset + dataLength));
                const data = textDecoder.decode(dataArray);
                offset = offset + dataLength;
                return {"protocol": 18001, "data": data};
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }
}