import { getWriteProtocolDefine } from "./ProtocolDefine.js";

export default class Writer {

    constructor() {
        this.textEncoder = new TextEncoder();
    }

    /**
     * write data
     * 
     * @param protocol the protocol number
     * @param data the data object
     * @return ArrayBuffer
     */
    write(protocol, data) {
        // write binary
        const meta = getWriteProtocolDefine(protocol, "write");
        let { view, offset } = this.__write(meta, data, new DataView(new ArrayBuffer(1024)), 4);
        view.setUint16(0, offset - 4, false);
        view.setUint16(2, protocol, false);
        return view.buffer.slice(0, offset);
    }

    __write(metadata, data, view, offset) {
        for (const meta of metadata) {
            switch (meta["type"]) {
                case "u8": {
                    while (view.byteLength < offset + 2) {
                        let extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    view.setUint8(offset, data[meta["name"]], false);
                    offset = offset + 1;
                } break;
                case "u16": {
                    while (view.byteLength < offset + 2) {
                        let extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    view.setUint16(offset, data[meta["name"]], false);
                    offset = offset + 2;
                } break;
                case "u32": {
                    while (view.byteLength < offset + 4) {
                        let extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    view.setUint32(offset, data[meta["name"]], false);
                    offset = offset + 4;
                } break;
                case "u64": {
                    while (view.byteLength < offset + 8) {
                        let extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    view.setBigUint64(offset, data[meta["name"]], false);
                    offset = offset + 8;
                } break;
                case "i8": {
                    while (view.byteLength < offset + 8) {
                        let extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    view.setInt8(offset, data[meta["name"]], false);
                    offset = offset + 1;
                } break;
                case "i16": {
                    while (view.byteLength < offset + 2) {
                        let extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    view.setInt16(offset, data[meta["name"]], false);
                    offset = offset + 2;
                } break;
                case "i32": {
                    while (view.byteLength < offset + 4) {
                        let extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    view.setInt32(offset, data[meta["name"]], false);
                    offset = offset + 4;
                } break;
                case "i64": {
                    while (view.byteLength < offset + 8) {
                        let extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    view.setBigInt64(offset, data[meta["name"]], false);
                    offset = offset + 8;
                } break;
                case "f32": {
                    while (view.byteLength < offset + 4) {
                        let extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    view.setFloat32(offset, data[meta["name"]], false);
                    offset = offset + 4;
                } break;
                case "f64": {
                    while (view.byteLength < offset + 8) {
                        let extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    view.setFloat64(offset, data[meta["name"]], false);
                    offset = offset + 8;
                } break;
                case "bool": {
                    while (view.byteLength < offset + 2) {
                        let extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    view.setUint8(offset, data[meta["name"]] ? 1 : 0, false);
                    offset = offset + 1;
                } break;
                case "binary": {
                    while (view.byteLength < offset + 6) {
                        let extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // binary
                    const binary = data[meta["name"]];
                    (new Uint8Array(view.buffer, offset)).set(new Uint8Array(binary));
                    offset = offset + binary.byteLength;
                } break;
                case "bst":
                case "str":
                case "rst": {
                    // str
                    let strArray = this.textEncoder.encode(data[meta["name"]]);
                    while (view.byteLength < offset + 2) {
                        let extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    view.setUint16(offset, strArray.length, false);
                    offset = offset + 2;
                    // extend
                    while (view.byteLength < offset + strArray.length) {
                        let extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    (new Uint8Array(view.buffer, offset)).set(strArray);
                    offset = offset + strArray.length;
                } break;
                case "list": {
                    while (view.byteLength < offset + 2) {
                        let extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // u16
                    view.setUint16(offset, data[meta["name"]].length, false);
                    offset = offset + 2;
                    // list explain
                    const explain = meta["explain"];
                    const listData = data[meta["name"]];
                    for (const item of listData) {
                        const result = this.__write(explain, item, view, offset);
                        view = result.view;
                        offset = result.offset;
                    }
                } break;
                case "map": {
                    while (view.byteLength < offset + 2) {
                        let extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // u16
                    view.setUint16(offset, Object.keys(data[meta["name"]]).length, false);
                    offset = offset + 2;
                    // list explain
                    const explain = meta["explain"];
                    const mapData = data[meta["name"]];
                    for (const key in mapData) {
                        const result = this.__write(explain, mapData[key], view, offset);
                        view = result.view;
                        offset = result.offset;
                    }
                } break;
                default: throw ("unknown type: " + meta["type"]);
            }
        }
        return { view, offset };
    }
}
