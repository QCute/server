import { encodeProtocol } from "./ProtocolRouter.js";

export default class Encoder {

    constructor() {
        this.textEncoder = new TextEncoder();
    }

    /**
     * encode data
     * 
     * @param protocol the protocol number
     * @param data the data object
     * @return ArrayBuffer
     */
    encode(protocol, data) {
        let view = encodeProtocol(this.textEncoder, new DataView(new ArrayBuffer(1024)), 4, protocol, data);
        // @tag protocol content length 2 bytes(without header 4 byte), protocol 2 bytes
        view.setUint16(0, view.byteLength - 4, false);
        view.setUint16(2, protocol, false);
        return view.buffer;
    }
}
