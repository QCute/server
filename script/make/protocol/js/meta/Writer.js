import ProtocolDefine from "./ProtocolDefine.js";

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
        const meta = ProtocolDefine.getWrite(protocol);
        let { view, offset } = this.__write__(meta, data, new DataView(new ArrayBuffer(1024)), 4);
        view.setUint16(0, offset - 4, false);
        view.setUint16(2, protocol, false);
        return view.buffer.slice(0, offset);
    }

    __write__(meta, data, view, offset) {
        const type = meta["type"];
        const explain = meta["explain"];
        const key = meta["key"];
        switch (type) {
            case "binary": {
                while (view.byteLength < offset + 6) {
                    let extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // binary
                (new Uint8Array(view.buffer, offset)).set(new Uint8Array(data));
                offset = offset + data.byteLength;
            } break;
            case "bool": {
                while (view.byteLength < offset + 2) {
                    let extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                view.setUint8(offset, data ? 1 : 0, false);
                offset = offset + 1;
            } break;
            case "u8": {
                while (view.byteLength < offset + 2) {
                    let extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                view.setUint8(offset, data, false);
                offset = offset + 1;
            } break;
            case "u16": {
                while (view.byteLength < offset + 2) {
                    let extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                view.setUint16(offset, data, false);
                offset = offset + 2;
            } break;
            case "u32": {
                while (view.byteLength < offset + 4) {
                    let extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                view.setUint32(offset, data, false);
                offset = offset + 4;
            } break;
            case "u64": {
                while (view.byteLength < offset + 8) {
                    let extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                view.setBigUint64(offset, data, false);
                offset = offset + 8;
            } break;
            case "i8": {
                while (view.byteLength < offset + 8) {
                    let extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                view.setInt8(offset, data, false);
                offset = offset + 1;
            } break;
            case "i16": {
                while (view.byteLength < offset + 2) {
                    let extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                view.setInt16(offset, data, false);
                offset = offset + 2;
            } break;
            case "i32": {
                while (view.byteLength < offset + 4) {
                    let extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                view.setInt32(offset, data, false);
                offset = offset + 4;
            } break;
            case "i64": {
                while (view.byteLength < offset + 8) {
                    let extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                view.setBigInt64(offset, data, false);
                offset = offset + 8;
            } break;
            case "f32": {
                while (view.byteLength < offset + 4) {
                    let extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                view.setFloat32(offset, data, false);
                offset = offset + 4;
            } break;
            case "f64": {
                while (view.byteLength < offset + 8) {
                    let extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                view.setFloat64(offset, data, false);
                offset = offset + 8;
            } break;
            case "bst":
            case "str":
            case "ast": {
                // str
                let strArray = this.textEncoder.encode(data);
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
                if(typeof key == 'undefined') {
                    while (view.byteLength < offset + 2) {
                        let extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // u16
                    view.setUint16(offset, data.length, false);
                    offset = offset + 2;
                    for (const item of data) {
                        // list only one explain
                        const result = this.__write__(explain[0], item, view, offset);
                        view = result.view;
                        offset = result.offset;
                    }
                } else {
                    while (view.byteLength < offset + 2) {
                        let extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // u16
                    view.setUint16(offset, Object.keys(data).length, false);
                    offset = offset + 2;
                    for (const item of Object.values(data)) {
                        // key list only one explain
                        const result = this.__write__(explain[0], item, view, offset);
                        view = result.view;
                        offset = result.offset;
                    }
                }
            } break;
            case "map": {
                for(const sub of explain) {
                    const result = this.__write__(sub, data[sub["name"]], view, offset);
                    view = result.view;
                    offset = result.offset;
                }
            } break;
            default: throw ("unknown type: " + meta["type"]);
        }
        return { view, offset };
    }
}
