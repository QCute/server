export function encodeTestProtocol(textEncoder, view, offset, protocol, data) {
    switch (protocol) {
        case 65535: {
            // extend
            while (view.byteLength < offset + 6) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            // binary
            const binary = data["binary"];
            (new Uint8Array(view.buffer, offset)).set(new Uint8Array(binary));
            offset = offset + binary.byteLength;
            // extend
            while (view.byteLength < offset + 1) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            // boolean
            view.setUint8(offset, data["boolean"] ? 1 : 0, false);
            offset = offset + 1;
            // extend
            while (view.byteLength < offset + 1) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            // u8
            view.setUint8(offset, data["u8"], false);
            offset = offset + 1;
            // extend
            while (view.byteLength < offset + 2) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            // u16
            view.setUint16(offset, data["u16"], false);
            offset = offset + 2;
            // extend
            while (view.byteLength < offset + 4) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            // u32
            view.setUint32(offset, data["u32"], false);
            offset = offset + 4;
            // extend
            while (view.byteLength < offset + 8) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            // u64
            view.setBigUint64(offset, data["u64"], false);
            offset = offset + 8;
            // extend
            while (view.byteLength < offset + 1) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            // i8
            view.setInt8(offset, data["i8"], false);
            offset = offset + 1;
            // extend
            while (view.byteLength < offset + 2) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            // i16
            view.setInt16(offset, data["i16"], false);
            offset = offset + 2;
            // extend
            while (view.byteLength < offset + 4) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            // i32
            view.setInt32(offset, data["i32"], false);
            offset = offset + 4;
            // extend
            while (view.byteLength < offset + 8) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            // i64
            view.setBigInt64(offset, data["i64"], false);
            offset = offset + 8;
            // extend
            while (view.byteLength < offset + 4) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            // f32
            view.setFloat32(offset, data["f32"], false);
            offset = offset + 4;
            // extend
            while (view.byteLength < offset + 8) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            // f64
            view.setFloat64(offset, data["f64"], false);
            offset = offset + 8;
            // extend
            while (view.byteLength < offset + 2) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            // str
            const strArray = textEncoder.encode(data["str"]);
            view.setUint16(offset, strArray.length, false);
            offset = offset + 2;
            // extend
            while (view.byteLength < offset + strArray.length) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            (new Uint8Array(view.buffer, offset)).set(strArray);
            offset = offset + strArray.length;
            // extend
            while (view.byteLength < offset + 2) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            // bst
            const bstArray = textEncoder.encode(data["bst"]);
            view.setUint16(offset, bstArray.length, false);
            offset = offset + 2;
            // extend
            while (view.byteLength < offset + bstArray.length) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            (new Uint8Array(view.buffer, offset)).set(bstArray);
            offset = offset + bstArray.length;
            // extend
            while (view.byteLength < offset + 2) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            // list
            const indexListData = data["indexList"];
            view.setUint16(offset, indexListData.length, false);
            offset = offset + 2;
            for (const indexListDataItem of indexListData) {
                // extend
                while (view.byteLength < offset + 6) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // list_binary
                const listBinary = indexListDataItem["listBinary"];
                (new Uint8Array(view.buffer, offset)).set(new Uint8Array(listBinary));
                offset = offset + listBinary.byteLength;
                // extend
                while (view.byteLength < offset + 1) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // list_boolean
                view.setUint8(offset, indexListDataItem["listBoolean"] ? 1 : 0, false);
                offset = offset + 1;
                // extend
                while (view.byteLength < offset + 1) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // list_u8
                view.setUint8(offset, indexListDataItem["listU8"], false);
                offset = offset + 1;
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // list_u16
                view.setUint16(offset, indexListDataItem["listU16"], false);
                offset = offset + 2;
                // extend
                while (view.byteLength < offset + 4) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // list_u32
                view.setUint32(offset, indexListDataItem["listU32"], false);
                offset = offset + 4;
                // extend
                while (view.byteLength < offset + 8) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // list_u64
                view.setBigUint64(offset, indexListDataItem["listU64"], false);
                offset = offset + 8;
                // extend
                while (view.byteLength < offset + 1) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // list_i8
                view.setInt8(offset, indexListDataItem["listI8"], false);
                offset = offset + 1;
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // list_i16
                view.setInt16(offset, indexListDataItem["listI16"], false);
                offset = offset + 2;
                // extend
                while (view.byteLength < offset + 4) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // list_i32
                view.setInt32(offset, indexListDataItem["listI32"], false);
                offset = offset + 4;
                // extend
                while (view.byteLength < offset + 8) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // list_i64
                view.setBigInt64(offset, indexListDataItem["listI64"], false);
                offset = offset + 8;
                // extend
                while (view.byteLength < offset + 4) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // list_f32
                view.setFloat32(offset, indexListDataItem["listF32"], false);
                offset = offset + 4;
                // extend
                while (view.byteLength < offset + 8) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // list_f64
                view.setFloat64(offset, indexListDataItem["listF64"], false);
                offset = offset + 8;
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // list_str
                const listStrArray = textEncoder.encode(indexListDataItem["listStr"]);
                view.setUint16(offset, listStrArray.length, false);
                offset = offset + 2;
                // extend
                while (view.byteLength < offset + listStrArray.length) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                (new Uint8Array(view.buffer, offset)).set(listStrArray);
                offset = offset + listStrArray.length;
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // list_bst
                const listBstArray = textEncoder.encode(indexListDataItem["listBst"]);
                view.setUint16(offset, listBstArray.length, false);
                offset = offset + 2;
                // extend
                while (view.byteLength < offset + listBstArray.length) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                (new Uint8Array(view.buffer, offset)).set(listBstArray);
                offset = offset + listBstArray.length;
            }
            // extend
            while (view.byteLength < offset + 2) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            // key_list
            const keyListData = data["keyList"];
            view.setUint16(offset, Object.keys(keyListData).length, false);
            offset = offset + 2;
            for (const keyListDataKey in keyListData) {
                // extend
                while (view.byteLength < offset + 6) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // list_binary
                const listBinary = keyListData[keyListDataKey]["listBinary"];
                (new Uint8Array(view.buffer, offset)).set(new Uint8Array(listBinary));
                offset = offset + listBinary.byteLength;
                // extend
                while (view.byteLength < offset + 1) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // list_boolean
                view.setUint8(offset, keyListData[keyListDataKey]["listBoolean"] ? 1 : 0, false);
                offset = offset + 1;
                // extend
                while (view.byteLength < offset + 1) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // list_u8
                view.setUint8(offset, keyListData[keyListDataKey]["listU8"], false);
                offset = offset + 1;
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // list_u16
                view.setUint16(offset, keyListData[keyListDataKey]["listU16"], false);
                offset = offset + 2;
                // extend
                while (view.byteLength < offset + 4) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // list_u32
                view.setUint32(offset, keyListData[keyListDataKey]["listU32"], false);
                offset = offset + 4;
                // extend
                while (view.byteLength < offset + 8) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // list_u64
                view.setBigUint64(offset, keyListData[keyListDataKey]["listU64"], false);
                offset = offset + 8;
                // extend
                while (view.byteLength < offset + 1) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // list_i8
                view.setInt8(offset, keyListData[keyListDataKey]["listI8"], false);
                offset = offset + 1;
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // list_i16
                view.setInt16(offset, keyListData[keyListDataKey]["listI16"], false);
                offset = offset + 2;
                // extend
                while (view.byteLength < offset + 4) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // list_i32
                view.setInt32(offset, keyListData[keyListDataKey]["listI32"], false);
                offset = offset + 4;
                // extend
                while (view.byteLength < offset + 8) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // list_i64
                view.setBigInt64(offset, keyListData[keyListDataKey]["listI64"], false);
                offset = offset + 8;
                // extend
                while (view.byteLength < offset + 4) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // list_f32
                view.setFloat32(offset, keyListData[keyListDataKey]["listF32"], false);
                offset = offset + 4;
                // extend
                while (view.byteLength < offset + 8) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // list_f64
                view.setFloat64(offset, keyListData[keyListDataKey]["listF64"], false);
                offset = offset + 8;
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // list_str
                const listStrArray = textEncoder.encode(keyListData[keyListDataKey]["listStr"]);
                view.setUint16(offset, listStrArray.length, false);
                offset = offset + 2;
                // extend
                while (view.byteLength < offset + listStrArray.length) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                (new Uint8Array(view.buffer, offset)).set(listStrArray);
                offset = offset + listStrArray.length;
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // list_bst
                const listBstArray = textEncoder.encode(keyListData[keyListDataKey]["listBst"]);
                view.setUint16(offset, listBstArray.length, false);
                offset = offset + 2;
                // extend
                while (view.byteLength < offset + listBstArray.length) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                (new Uint8Array(view.buffer, offset)).set(listBstArray);
                offset = offset + listBstArray.length;
            }
            return new DataView(view.buffer.slice(0, offset));
        }
        default:throw("unknown protocol define: " + protocol)
    }
}

export function decodeTestProtocol(textDecoder, view, offset, protocol) {
    switch (protocol) {
        case 65535: {
            // binary
            const binary = view.buffer.slice(offset, offset + 6);
            offset = offset + 6;
            // boolean
            const boolean = view.getUint8(offset, false) !== 0;
            offset = offset + 1;
            // u8
            const u8 = view.getUint8(offset, false);
            offset = offset + 1;
            // u16
            const u16 = view.getUint16(offset, false);
            offset = offset + 2;
            // u32
            const u32 = view.getUint32(offset, false);
            offset = offset + 4;
            // u64
            const u64 = view.getBigUint64(offset, false);
            offset = offset + 8;
            // i8
            const i8 = view.getInt8(offset, false);
            offset = offset + 1;
            // i16
            const i16 = view.getInt16(offset, false);
            offset = offset + 2;
            // i32
            const i32 = view.getInt32(offset, false);
            offset = offset + 4;
            // i64
            const i64 = view.getBigInt64(offset, false);
            offset = offset + 8;
            // f32
            const f32 = view.getFloat32(offset, false);
            offset = offset + 4;
            // f64
            const f64 = view.getFloat64(offset, false);
            offset = offset + 8;
            // str
            const strLength = view.getUint16(offset, false);
            offset = offset + 2;
            const strArray = new Uint8Array(view.buffer.slice(offset, offset + strLength));
            const str = textDecoder.decode(strArray);
            offset = offset + strLength;
            // bst
            const bstLength = view.getUint16(offset, false);
            offset = offset + 2;
            const bstArray = new Uint8Array(view.buffer.slice(offset, offset + bstLength));
            const bst = textDecoder.decode(bstArray);
            offset = offset + bstLength;
            // list
            const indexList = [];
            let indexListLength = view.getUint16(offset, false);
            offset = offset + 2;
            while (--indexListLength >= 0) {
                // list_binary
                const listBinary = view.buffer.slice(offset, offset + 6);
                offset = offset + 6;
                // list_boolean
                const listBoolean = view.getUint8(offset, false) !== 0;
                offset = offset + 1;
                // list_u8
                const listU8 = view.getUint8(offset, false);
                offset = offset + 1;
                // list_u16
                const listU16 = view.getUint16(offset, false);
                offset = offset + 2;
                // list_u32
                const listU32 = view.getUint32(offset, false);
                offset = offset + 4;
                // list_u64
                const listU64 = view.getBigUint64(offset, false);
                offset = offset + 8;
                // list_i8
                const listI8 = view.getInt8(offset, false);
                offset = offset + 1;
                // list_i16
                const listI16 = view.getInt16(offset, false);
                offset = offset + 2;
                // list_i32
                const listI32 = view.getInt32(offset, false);
                offset = offset + 4;
                // list_i64
                const listI64 = view.getBigInt64(offset, false);
                offset = offset + 8;
                // list_f32
                const listF32 = view.getFloat32(offset, false);
                offset = offset + 4;
                // list_f64
                const listF64 = view.getFloat64(offset, false);
                offset = offset + 8;
                // list_str
                const listStrLength = view.getUint16(offset, false);
                offset = offset + 2;
                const listStrArray = new Uint8Array(view.buffer.slice(offset, offset + listStrLength));
                const listStr = textDecoder.decode(listStrArray);
                offset = offset + listStrLength;
                // list_bst
                const listBstLength = view.getUint16(offset, false);
                offset = offset + 2;
                const listBstArray = new Uint8Array(view.buffer.slice(offset, offset + listBstLength));
                const listBst = textDecoder.decode(listBstArray);
                offset = offset + listBstLength;
                // add
                indexList.push({listBinary, listBoolean, listU8, listU16, listU32, listU64, listI8, listI16, listI32, listI64, listF32, listF64, listStr, listBst});
            }
            // key_list
            const keyList = {};
            let keyListLength = view.getUint16(offset, false);
            offset = offset + 2;
            while (--keyListLength >= 0) {
                // list_binary
                const listBinary = view.buffer.slice(offset, offset + 6);
                offset = offset + 6;
                // list_boolean
                const listBoolean = view.getUint8(offset, false) !== 0;
                offset = offset + 1;
                // list_u8
                const listU8 = view.getUint8(offset, false);
                offset = offset + 1;
                // list_u16
                const listU16 = view.getUint16(offset, false);
                offset = offset + 2;
                // list_u32
                const listU32 = view.getUint32(offset, false);
                offset = offset + 4;
                // list_u64
                const listU64 = view.getBigUint64(offset, false);
                offset = offset + 8;
                // list_i8
                const listI8 = view.getInt8(offset, false);
                offset = offset + 1;
                // list_i16
                const listI16 = view.getInt16(offset, false);
                offset = offset + 2;
                // list_i32
                const listI32 = view.getInt32(offset, false);
                offset = offset + 4;
                // list_i64
                const listI64 = view.getBigInt64(offset, false);
                offset = offset + 8;
                // list_f32
                const listF32 = view.getFloat32(offset, false);
                offset = offset + 4;
                // list_f64
                const listF64 = view.getFloat64(offset, false);
                offset = offset + 8;
                // list_str
                const listStrLength = view.getUint16(offset, false);
                offset = offset + 2;
                const listStrArray = new Uint8Array(view.buffer.slice(offset, offset + listStrLength));
                const listStr = textDecoder.decode(listStrArray);
                offset = offset + listStrLength;
                // list_bst
                const listBstLength = view.getUint16(offset, false);
                offset = offset + 2;
                const listBstArray = new Uint8Array(view.buffer.slice(offset, offset + listBstLength));
                const listBst = textDecoder.decode(listBstArray);
                offset = offset + listBstLength;
                // add
                keyList[listU8] = {listBinary, listBoolean, listU8, listU16, listU32, listU64, listI8, listI16, listI32, listI64, listF32, listF64, listStr, listBst};
            }
            return {binary, boolean, u8, u16, u32, u64, i8, i16, i32, i64, f32, f64, str, bst, indexList, keyList};
        }
        default:throw("unknown protocol define: " + protocol)
    }
}