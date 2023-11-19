import ProtocolDefine from "./ProtocolDefine.js";

export default class Reader {

    constructor() {
        this.length = 0;
        this.view = new DataView(new Uint8Array(1024).buffer);
        this.textDecoder = new TextDecoder();
    }

    /**
     * append data
     * 
     * @param buffer the WebSocket message ArrayBuffer
     * @return this
     */
    appendData(buffer) {
        // extend
        while (this.view.byteLength - this.length < buffer.byteLength) {
            let extendView = new DataView(new ArrayBuffer(this.view.byteLength * 2));
            (new Uint8Array(extendView.buffer)).set(new Uint8Array(this.view.buffer));
            this.view = extendView;
        }
        // append
        (new Uint8Array(this.view.buffer)).set(new Uint8Array(buffer), this.length);
        this.length = this.length + buffer.byteLength;
        return this;
    }

    /**
     * read packet
     * 
     * @return Object, the packet definition
     */
    read() {
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
                const meta = ProtocolDefine.getRead(protocol);
                const { data } = this.__read__(meta, 0, packetView);
                return { protocol, data }
            }
        }
        return undefined;
    }

    __read__(meta, offset, view) {
        let data = null;
        const type = meta["type"];
        const explain = meta["explain"];
        const key = meta["key"];
        switch (type) {
            case "binary": {
                data = view.buffer.slice(offset, offset + explain);
                offset = offset + explain;
            } break;
            case "bool": {
                data = view.getUint8(offset, false) !== 0;
                offset = offset + 1;
            } break;
            case "u8": {
                data = view.getUint8(offset, false);
                offset = offset + 1;
            } break;
            case "u16": {
                data = view.getUint16(offset, false);
                offset = offset + 2;
            } break;
            case "u32": {
                data = view.getUint32(offset, false);
                offset = offset + 4;
            } break;
            case "u64": {
                data = view.getBigUint64(offset, false);
                offset = offset + 8;
            } break;
            case "i8": {
                data = view.getInt8(offset, false);
                offset = offset + 1;
            } break;
            case "i16": {
                data = view.getInt16(offset, false);
                offset = offset + 2;
            } break;
            case "i32": {
                data = view.getInt32(offset, false);
                offset = offset + 4;
            } break;
            case "i64": {
                data = view.getBigInt64(offset, false);
                offset = offset + 8;
            } break;
            case "f32": {
                data = view.getFloat32(offset, false);
                offset = offset + 4;
            } break;
            case "f64": {
                data = view.getFloat64(offset, false);
                offset = offset + 8;
            } break;
            case "str":
            case "bst":
            case "ast": {
                let length = view.getUint16(offset, false);
                offset = offset + 2;
                let array = new Uint8Array(view.buffer.slice(offset, offset + length));
                data = this.textDecoder.decode(array);
                offset = offset + length;
            } break;
            case "list": {
                if(typeof key == 'undefined') {
                    data = [];
                    let length = view.getUint16(offset, false);
                    offset = offset + 2;
                    while (--length >= 0) {
                        // list only one explain
                        const result = this.__read__(explain[0], offset, view);
                        data.push(result.data);
                        offset = result.offset;
                    }
                } else {
                    data = {};
                    let length = view.getUint16(offset, false);
                    offset = offset + 2;
                    while (--length >= 0) {
                        // key list only one explain
                        const result = this.__read__(explain[0], offset, view);
                        data[result.data[key]] = result.data;
                        offset = result.offset;
                    }
                }
            } break;
            case "map": {
                data = {};
                for(const sub of explain) {
                    const result = this.__read__(sub, offset, view);
                    data[sub["name"]] = result.data;
                    offset = result.offset;
                }
            } break;
            default: throw ("unknown meta type: " + meta["type"])
        }

        return { data, offset };
    }
}
