import ProtocolRouter from "./ProtocolRouter.js";

export default class Decoder {

    constructor() {
        this.length = 0;
        this.view = new DataView(new Uint8Array(1024).buffer);
        this.textDecoder = new TextDecoder();
    }

    /**
     * append data
     * 
     * @param {ArrayBuffer} buffer the WebSocket message ArrayBuffer
     * @return {this}
     */
    appendData(buffer) {
        if(this.view.byteLength <= 0) {
            this.view = new DataView(new Uint8Array(1024).buffer);
        }
        // extend
        while (this.view.byteLength - this.length < buffer.byteLength) {
            let extendView = new DataView(new ArrayBuffer(this.view.byteLength * 2));
            (new Uint8Array(extendView.buffer)).set(new Uint8Array(this.view.buffer));
            this.view = extendView;
        }
        // append
        (new Uint8Array(this.view.buffer, this.length)).set(new Uint8Array(buffer));
        this.length = this.length + buffer.byteLength;
        return this;
    }

    /**
     * decode packet
     * 
     * @return {{protocol: number; data: boolean|number|string|object|Array<boolean|number|string|object>;}|undefined}, the packet definition
     */
    decode() {
        // @tag protocol data length 2 bytes(without header 4 byte), protocol 2 bytes
        if (this.length >= 4) {
            const packetLength = this.view.getUint16(0, false);
            if (this.length >= 4 + packetLength) {
                // completed packet
                const protocol = this.view.getUint16(2, false);
                // the packet view
                const packetView = new DataView(this.view.buffer.slice(4, 4 + packetLength));
                // the rest view
                this.view = new DataView(this.view.buffer.slice(4 + packetLength));
                this.length = this.length - packetLength - 4;
                // decode
                return ProtocolRouter.decode(this.textDecoder, packetView, 0, protocol);
            }
        }
    }
}
