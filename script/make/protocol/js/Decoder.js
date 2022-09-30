import { decodeProtocol } from "./ProtocolRouter.js";

export default class Decoder {

    constructor() {
        this.length = 0;
        this.view = new DataView(new Uint8Array(1024).buffer);
        this.textDecoder = new TextDecoder();
    }

    /**
     * decode package
     * 
     * @param buffer the WebSocket message ArrayBuffer
     * @return Object, the package definition
     */
    decode(buffer) {
        if (buffer) {
            // extend
            while (this.view.byteLength - this.length < buffer.byteLength) {
                let extendView = new DataView(new ArrayBuffer(this.view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(this.view.buffer));
                this.view = extendView;
            }
            // append
            (new Uint8Array(this.view.buffer, this.length)).set(new Uint8Array(buffer));
            this.length = this.length + buffer.byteLength;
        }
        // @tag protocol content length 2 bytes(without header 4 byte), protocol 2 bytes
        if (this.length >= 4) {
            const packageLength = this.view.getUint16(0, false);
            if (this.length >= 4 + packageLength) {
                // completed package
                const protocol = this.view.getUint16(2, false);
                // the package view
                const packageView = new DataView(this.view.buffer.slice(4, 4 + packageLength));
                // the rest view
                this.view = new DataView(this.view.buffer.slice(4 + packageLength));
                this.length = this.length - packageLength - 4;
                // decode
                const content = decodeProtocol(this.textDecoder, packageView, 0, protocol);
                return { protocol, content };
            }
        }
        return undefined;
    }
}
