export default class TestProtocol {
    static encode(textEncoder, view, offset, protocol, data) {
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
                while (view.byteLength < offset + 6) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // tuple_binary
                const tupleBinary = data["tuple"]["tupleBinary"];
                (new Uint8Array(view.buffer, offset)).set(new Uint8Array(tupleBinary));
                offset = offset + tupleBinary.byteLength;
                // extend
                while (view.byteLength < offset + 1) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // tuple_sub_tuple_u8
                view.setUint8(offset, data["tuple"]["tupleSubTuple"]["tupleSubTupleU8"], false);
                offset = offset + 1;
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // tuple_sub_tuple_str
                const tupleSubTupleStrArray = textEncoder.encode(data["tuple"]["tupleSubTuple"]["tupleSubTupleStr"]);
                view.setUint16(offset, tupleSubTupleStrArray.length, false);
                offset = offset + 2;
                // extend
                while (view.byteLength < offset + tupleSubTupleStrArray.length) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                (new Uint8Array(view.buffer, offset)).set(tupleSubTupleStrArray);
                offset = offset + tupleSubTupleStrArray.length;
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // tuple_sub_list
                const tupleSubListData = data["tuple"]["tupleSubList"];
                view.setUint16(offset, tupleSubListData.length, false);
                offset = offset + 2;
                for (const tupleSubListDataItem of tupleSubListData) {
                    // extend
                    while (view.byteLength < offset + 2) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // tuple_sub_list_i16
                    view.setInt16(offset, tupleSubListDataItem["tupleSubListI16"], false);
                    offset = offset + 2;
                    // extend
                    while (view.byteLength < offset + 2) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // tuple_sub_list_bst
                    const tupleSubListBstArray = textEncoder.encode(tupleSubListDataItem["tupleSubListBst"]);
                    view.setUint16(offset, tupleSubListBstArray.length, false);
                    offset = offset + 2;
                    // extend
                    while (view.byteLength < offset + tupleSubListBstArray.length) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    (new Uint8Array(view.buffer, offset)).set(tupleSubListBstArray);
                    offset = offset + tupleSubListBstArray.length;
                }
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // tuple_sub_list_single
                const tupleSubListSingleData = data["tuple"]["tupleSubListSingle"];
                view.setUint16(offset, tupleSubListSingleData.length, false);
                offset = offset + 2;
                for (const tupleSubListSingleDataItem of tupleSubListSingleData) {
                    // extend
                    while (view.byteLength < offset + 1) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // tuple_sub_list_single_bool
                    view.setUint8(offset, tupleSubListSingleDataItem ? 1 : 0, false);
                    offset = offset + 1;
                }
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
                    // list_sub_tuple_u8
                    view.setUint8(offset, indexListDataItem["listSubTuple"]["listSubTupleU8"], false);
                    offset = offset + 1;
                    // extend
                    while (view.byteLength < offset + 2) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // list_sub_tuple_str
                    const listSubTupleStrArray = textEncoder.encode(indexListDataItem["listSubTuple"]["listSubTupleStr"]);
                    view.setUint16(offset, listSubTupleStrArray.length, false);
                    offset = offset + 2;
                    // extend
                    while (view.byteLength < offset + listSubTupleStrArray.length) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    (new Uint8Array(view.buffer, offset)).set(listSubTupleStrArray);
                    offset = offset + listSubTupleStrArray.length;
                    // extend
                    while (view.byteLength < offset + 2) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // list_sub_list
                    const listSubListData = indexListDataItem["listSubList"];
                    view.setUint16(offset, listSubListData.length, false);
                    offset = offset + 2;
                    for (const listSubListDataItem of listSubListData) {
                        // extend
                        while (view.byteLength < offset + 2) {
                            const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                            (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                            view = extendView;
                        }
                        // list_sub_list_i16
                        view.setInt16(offset, listSubListDataItem["listSubListI16"], false);
                        offset = offset + 2;
                        // extend
                        while (view.byteLength < offset + 2) {
                            const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                            (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                            view = extendView;
                        }
                        // list_sub_list_bst
                        const listSubListBstArray = textEncoder.encode(listSubListDataItem["listSubListBst"]);
                        view.setUint16(offset, listSubListBstArray.length, false);
                        offset = offset + 2;
                        // extend
                        while (view.byteLength < offset + listSubListBstArray.length) {
                            const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                            (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                            view = extendView;
                        }
                        (new Uint8Array(view.buffer, offset)).set(listSubListBstArray);
                        offset = offset + listSubListBstArray.length;
                    }
                    // extend
                    while (view.byteLength < offset + 2) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // list_sub_list_single
                    const listSubListSingleData = indexListDataItem["listSubListSingle"];
                    view.setUint16(offset, listSubListSingleData.length, false);
                    offset = offset + 2;
                    for (const listSubListSingleDataItem of listSubListSingleData) {
                        // extend
                        while (view.byteLength < offset + 1) {
                            const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                            (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                            view = extendView;
                        }
                        // list_sub_list_single_bool
                        view.setUint8(offset, listSubListSingleDataItem ? 1 : 0, false);
                        offset = offset + 1;
                    }
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
                    const keyListDataItem = keyListData[keyListDataKey];
                    // extend
                    while (view.byteLength < offset + 6) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // list_binary
                    const listBinary = keyListDataItem["listBinary"];
                    (new Uint8Array(view.buffer, offset)).set(new Uint8Array(listBinary));
                    offset = offset + listBinary.byteLength;
                    // extend
                    while (view.byteLength < offset + 1) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // list_boolean
                    view.setUint8(offset, keyListDataItem["listBoolean"] ? 1 : 0, false);
                    offset = offset + 1;
                    // extend
                    while (view.byteLength < offset + 1) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // list_u8
                    view.setUint8(offset, keyListDataItem["listU8"], false);
                    offset = offset + 1;
                    // extend
                    while (view.byteLength < offset + 2) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // list_u16
                    view.setUint16(offset, keyListDataItem["listU16"], false);
                    offset = offset + 2;
                    // extend
                    while (view.byteLength < offset + 4) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // list_u32
                    view.setUint32(offset, keyListDataItem["listU32"], false);
                    offset = offset + 4;
                    // extend
                    while (view.byteLength < offset + 8) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // list_u64
                    view.setBigUint64(offset, keyListDataItem["listU64"], false);
                    offset = offset + 8;
                    // extend
                    while (view.byteLength < offset + 1) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // list_i8
                    view.setInt8(offset, keyListDataItem["listI8"], false);
                    offset = offset + 1;
                    // extend
                    while (view.byteLength < offset + 2) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // list_i16
                    view.setInt16(offset, keyListDataItem["listI16"], false);
                    offset = offset + 2;
                    // extend
                    while (view.byteLength < offset + 4) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // list_i32
                    view.setInt32(offset, keyListDataItem["listI32"], false);
                    offset = offset + 4;
                    // extend
                    while (view.byteLength < offset + 8) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // list_i64
                    view.setBigInt64(offset, keyListDataItem["listI64"], false);
                    offset = offset + 8;
                    // extend
                    while (view.byteLength < offset + 4) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // list_f32
                    view.setFloat32(offset, keyListDataItem["listF32"], false);
                    offset = offset + 4;
                    // extend
                    while (view.byteLength < offset + 8) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // list_f64
                    view.setFloat64(offset, keyListDataItem["listF64"], false);
                    offset = offset + 8;
                    // extend
                    while (view.byteLength < offset + 2) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // list_str
                    const listStrArray = textEncoder.encode(keyListDataItem["listStr"]);
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
                    const listBstArray = textEncoder.encode(keyListDataItem["listBst"]);
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
            default: throw("unknown protocol define: " + protocol)
        }
    }

    static decode(textDecoder, view, offset, protocol) {
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
                // tuple
                // tuple_binary
                const tupleBinary = view.buffer.slice(offset, offset + 6);
                offset = offset + 6;
                // tuple_sub_tuple
                // tuple_sub_tuple_u8
                const tupleSubTupleU8 = view.getUint8(offset, false);
                offset = offset + 1;
                // tuple_sub_tuple_str
                const tupleSubTupleStrLength = view.getUint16(offset, false);
                offset = offset + 2;
                const tupleSubTupleStrArray = new Uint8Array(view.buffer.slice(offset, offset + tupleSubTupleStrLength));
                const tupleSubTupleStr = textDecoder.decode(tupleSubTupleStrArray);
                offset = offset + tupleSubTupleStrLength;
                // object
                const tupleSubTuple = {tupleSubTupleU8, tupleSubTupleStr};
                // tuple_sub_list
                const tupleSubList = [];
                let tupleSubListLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--tupleSubListLength >= 0) {
                    // tuple_sub_list_i16
                    const tupleSubListI16 = view.getInt16(offset, false);
                    offset = offset + 2;
                    // tuple_sub_list_bst
                    const tupleSubListBstLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    const tupleSubListBstArray = new Uint8Array(view.buffer.slice(offset, offset + tupleSubListBstLength));
                    const tupleSubListBst = textDecoder.decode(tupleSubListBstArray);
                    offset = offset + tupleSubListBstLength;
                    // add
                    tupleSubList.push({tupleSubListI16, tupleSubListBst});
                }
                // tuple_sub_list_single
                const tupleSubListSingle = [];
                let tupleSubListSingleLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--tupleSubListSingleLength >= 0) {
                    // tuple_sub_list_single_bool
                    const tupleSubListSingleBool = view.getUint8(offset, false) !== 0;
                    offset = offset + 1;
                    // add
                    tupleSubListSingle.push(tupleSubListSingleBool);
                }
                // object
                const tuple = {tupleBinary, tupleSubTuple, tupleSubList, tupleSubListSingle};
                // list
                const indexList = [];
                let indexListLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--indexListLength >= 0) {
                    // list_binary
                    const listBinary = view.buffer.slice(offset, offset + 6);
                    offset = offset + 6;
                    // list_sub_tuple
                    // list_sub_tuple_u8
                    const listSubTupleU8 = view.getUint8(offset, false);
                    offset = offset + 1;
                    // list_sub_tuple_str
                    const listSubTupleStrLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    const listSubTupleStrArray = new Uint8Array(view.buffer.slice(offset, offset + listSubTupleStrLength));
                    const listSubTupleStr = textDecoder.decode(listSubTupleStrArray);
                    offset = offset + listSubTupleStrLength;
                    // object
                    const listSubTuple = {listSubTupleU8, listSubTupleStr};
                    // list_sub_list
                    const listSubList = [];
                    let listSubListLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    while (--listSubListLength >= 0) {
                        // list_sub_list_i16
                        const listSubListI16 = view.getInt16(offset, false);
                        offset = offset + 2;
                        // list_sub_list_bst
                        const listSubListBstLength = view.getUint16(offset, false);
                        offset = offset + 2;
                        const listSubListBstArray = new Uint8Array(view.buffer.slice(offset, offset + listSubListBstLength));
                        const listSubListBst = textDecoder.decode(listSubListBstArray);
                        offset = offset + listSubListBstLength;
                        // add
                        listSubList.push({listSubListI16, listSubListBst});
                    }
                    // list_sub_list_single
                    const listSubListSingle = [];
                    let listSubListSingleLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    while (--listSubListSingleLength >= 0) {
                        // list_sub_list_single_bool
                        const listSubListSingleBool = view.getUint8(offset, false) !== 0;
                        offset = offset + 1;
                        // add
                        listSubListSingle.push(listSubListSingleBool);
                    }
                    // add
                    indexList.push({listBinary, listSubTuple, listSubList, listSubListSingle});
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
                return {binary, boolean, u8, u16, u32, u64, i8, i16, i32, i64, f32, f64, str, bst, tuple, indexList, keyList};
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }
}