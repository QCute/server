import { getReadProtocolDefine } from "./ProtocolDefine.js";

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
                const meta = getReadProtocolDefine(protocol);
                const { data } = this.__read__(meta, 0, packetView);
                return { protocol, data }
            }
        }
        return undefined;
    }

    __read__(metadata, offset, view) {
        let data = {};
        for (const meta of metadata) {
            switch (meta["type"]) {
                case "u8": {
                    data[meta["name"]] = view.getUint8(offset, false);
                    offset = offset + 1;
                } break;
                case "u16": {
                    data[meta["name"]] = view.getUint16(offset, false);
                    offset = offset + 2;
                } break;
                case "u32": {
                    data[meta["name"]] = view.getUint32(offset, false);
                    offset = offset + 4;
                } break;
                case "u64": {
                    data[meta["name"]] = view.getBigUint64(offset, false);
                    offset = offset + 8;
                } break;
                case "i8": {
                    data[meta["name"]] = view.getInt8(offset, false);
                    offset = offset + 1;
                } break;
                case "i16": {
                    data[meta["name"]] = view.getInt16(offset, false);
                    offset = offset + 2;
                } break;
                case "i32": {
                    data[meta["name"]] = view.getInt32(offset, false);
                    offset = offset + 4;
                } break;
                case "i64": {
                    data[meta["name"]] = view.getBigInt64(offset, false);
                    offset = offset + 8;
                } break;
                case "f32": {
                    data[meta["name"]] = view.getFloat32(offset, false);
                    offset = offset + 4;
                } break;
                case "f64": {
                    data[meta["name"]] = view.getFloat64(offset, false);
                    offset = offset + 8;
                } break;
                case "bool": {
                    data[meta["name"]] = view.getUint8(offset, false) !== 0;
                    offset = offset + 1;
                } break;
                case "binary": {
                    const length = meta["explain"];
                    data[meta["name"]] = view.buffer.slice(offset, offset + length);
                    offset = offset + length;
                } break;
                case "str":
                case "bst":
                case "rst": {
                    let length = view.getUint16(offset, false);
                    offset = offset + 2;
                    let array = new Uint8Array(view.buffer.slice(offset, offset + length));
                    data[meta["name"]] = this.textDecoder.decode(array);
                    offset = offset + length;
                } break;
                case "list": {
                    let list = [];
                    let length = view.getUint16(offset, false);
                    offset = offset + 2;
                    const explain = meta["explain"];
                    while (--length >= 0) {
                        const result = this.__read__(explain, offset, view);
                        list.push(result["data"]);
                        offset = result["offset"];
                    }
                    data[meta["name"]] = list;
                } break;
                case "map": {
                    let map = {};
                    const key = meta["key"];
                    let length = view.getUint16(offset, false);
                    offset = offset + 2;
                    const explain = meta["explain"];
                    while (--length >= 0) {
                        const result = this.__read__(explain, offset, view);
                        const sub = result["data"];
                        map[sub[key]] = sub;
                        offset = result["offset"];
                    }
                    data[meta["name"]] = map;
                } break;
                default: throw ("unknown meta type: " + meta["type"])
            }
        }
        return { data, offset };
    }
}
