import { getReadProtocolDefine } from "./ProtocolDefine.js";

export default class Reader {

    constructor() {
        this.length = 0;
        this.view = new DataView(new Uint8Array(1024).buffer);
        this.textDecoder = new TextDecoder();
    }

    /**
     * read package
     * 
     * @param buffer the WebSocket message ArrayBuffer
     * @return Object, the package definition
     */
    read(buffer) {
        if (buffer) {
            // extend
            while (this.view.byteLength - this.length < buffer.byteLength) {
                let extendView = new DataView(new ArrayBuffer(this.view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(this.view.buffer));
                this.view = extendView;
            }
            // append
            (new Uint8Array(this.view.buffer)).set(new Uint8Array(buffer), this.length);
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
                const meta = getReadProtocolDefine(protocol);
                const result = this.__read(meta, 0, packageView);
                return { protocol, content: result.content }
            }
        }
        return undefined;
    }

    __read(metadata, offset, view) {
        let content = {};
        for (const meta of metadata) {
            switch (meta["type"]) {
                case "u8": {
                    content[meta["name"]] = view.getUint8(offset, false);
                    offset = offset + 1;
                } break;
                case "u16": {
                    content[meta["name"]] = view.getUint16(offset, false);
                    offset = offset + 2;
                } break;
                case "u32": {
                    content[meta["name"]] = view.getUint32(offset, false);
                    offset = offset + 4;
                } break;
                case "u64": {
                    content[meta["name"]] = view.getBigUint64(offset, false);
                    offset = offset + 8;
                } break;
                case "i8": {
                    content[meta["name"]] = view.getInt8(offset, false);
                    offset = offset + 1;
                } break;
                case "i16": {
                    content[meta["name"]] = view.getInt16(offset, false);
                    offset = offset + 2;
                } break;
                case "i32": {
                    content[meta["name"]] = view.getInt32(offset, false);
                    offset = offset + 4;
                } break;
                case "i64": {
                    content[meta["name"]] = view.getBigInt64(offset, false);
                    offset = offset + 8;
                } break;
                case "f32": {
                    content[meta["name"]] = view.getFloat32(offset, false);
                    offset = offset + 4;
                } break;
                case "f64": {
                    content[meta["name"]] = view.getFloat64(offset, false);
                    offset = offset + 8;
                } break;
                case "bool": {
                    content[meta["name"]] = view.getUint8(offset, false) !== 0;
                    offset = offset + 1;
                } break;
                case "binary": {
                    const length = meta["explain"];
                    content[meta["name"]] = view.buffer.slice(offset, offset + length);
                    offset = offset + length;
                } break;
                case "str":
                case "bst":
                case "rst": {
                    let length = view.getUint16(offset, false);
                    offset = offset + 2;
                    let array = new Uint8Array(view.buffer.slice(offset, offset + length));
                    content[meta["name"]] = this.textDecoder.decode(array);
                    offset = offset + length;
                } break;
                case "list": {
                    let list = [];
                    let length = view.getUint16(offset, false);
                    offset = offset + 2;
                    const explain = meta["explain"];
                    while (--length >= 0) {
                        const result = this.__read(explain, offset, view);
                        list.push(result["content"]);
                        offset = result["offset"];
                    }
                    content[meta["name"]] = list;
                } break;
                case "map": {
                    let map = {};
                    const key = meta["key"];
                    let length = view.getUint16(offset, false);
                    offset = offset + 2;
                    const explain = meta["explain"];
                    while (--length >= 0) {
                        const result = this.__read(explain, offset, view);
                        const sub = result["content"];
                        map[sub[key]] = sub;
                        offset = result["offset"];
                    }
                    content[meta["name"]] = map;
                } break;
                default: throw ("unknown meta type: " + meta["type"])
            }
        }
        return { content, offset };
    }
}
