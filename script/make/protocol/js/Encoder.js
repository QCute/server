import ProtocolRouter from "./ProtocolRouter.js";

export default class Encoder {

    constructor() {
        this.textEncoder = new TextEncoder();
    }

    /**
     * encode data
     * 
     * @param {number} protocol the protocol number
     * @param {boolean|number|string|object|Array<boolean|number|string|object>} data the data object
     * @return {ArrayBuffer}
     */
    encode(protocol, data) {
        let view = ProtocolRouter.encode(this.textEncoder, new DataView(new ArrayBuffer(1024)), 4, protocol, data);
        // @tag protocol data length 2 bytes(without header 4 byte), protocol 2 bytes
        view.setUint16(0, view.byteLength - 4, false);
        view.setUint16(2, protocol, false);
        return view.buffer;
    }
}
