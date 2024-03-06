export default class TestProtocol {
    static encode(textEncoder, view, offset, protocol, data) {
        switch (protocol) {
            case 65533: {
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // single i16
                view.setInt16(offset, data, false);
                offset = offset + 2;
                return new DataView(view.buffer.slice(0, offset));
            }
            case 65534: {
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // single list
                view.setUint16(offset, data.length, false);
                offset = offset + 2;
                for (const dataItem of data) {
                    // extend
                    while (view.byteLength < offset + 4) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // single u32
                    view.setUint32(offset, dataItem, false);
                    offset = offset + 4;
                }
                return new DataView(view.buffer.slice(0, offset));
            }
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
                // bool
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
                // i16
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
                // convert
                const tuple = data["tuple"];
                // extend
                while (view.byteLength < offset + 6) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // tuple binary
                const tupleBinary = tuple["binary"];
                (new Uint8Array(view.buffer, offset)).set(new Uint8Array(tupleBinary));
                offset = offset + tupleBinary.byteLength;
                // convert
                const tupleSub = tuple["sub"];
                // extend
                while (view.byteLength < offset + 1) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // tuple tuple u8
                view.setUint8(offset, tupleSub["u8"], false);
                offset = offset + 1;
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // tuple tuple str
                const tupleSubStrArray = textEncoder.encode(tupleSub["str"]);
                view.setUint16(offset, tupleSubStrArray.length, false);
                offset = offset + 2;
                // extend
                while (view.byteLength < offset + tupleSubStrArray.length) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                (new Uint8Array(view.buffer, offset)).set(tupleSubStrArray);
                offset = offset + tupleSubStrArray.length;
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // tuple list
                const tupleList = tuple["list"];
                view.setUint16(offset, tupleList.length, false);
                offset = offset + 2;
                for (const tupleListItem of tupleList) {
                    // extend
                    while (view.byteLength < offset + 2) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // tuple list i16
                    view.setInt16(offset, tupleListItem["i16"], false);
                    offset = offset + 2;
                    // extend
                    while (view.byteLength < offset + 2) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // tuple list bst
                    const tupleListItemBstArray = textEncoder.encode(tupleListItem["bst"]);
                    view.setUint16(offset, tupleListItemBstArray.length, false);
                    offset = offset + 2;
                    // extend
                    while (view.byteLength < offset + tupleListItemBstArray.length) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    (new Uint8Array(view.buffer, offset)).set(tupleListItemBstArray);
                    offset = offset + tupleListItemBstArray.length;
                }
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 
                const tupleSingle = tuple["single"];
                view.setUint16(offset, tupleSingle.length, false);
                offset = offset + 2;
                for (const tupleSingleItem of tupleSingle) {
                    // extend
                    while (view.byteLength < offset + 1) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // bool
                    view.setUint8(offset, tupleSingleItem ? 1 : 0, false);
                    offset = offset + 1;
                }
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // list
                const indexList = data["indexList"];
                view.setUint16(offset, indexList.length, false);
                offset = offset + 2;
                for (const indexListItem of indexList) {
                    // extend
                    while (view.byteLength < offset + 6) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // tuple binary
                    const indexListItemBinary = indexListItem["binary"];
                    (new Uint8Array(view.buffer, offset)).set(new Uint8Array(indexListItemBinary));
                    offset = offset + indexListItemBinary.byteLength;
                    // convert
                    const indexListItemSub = indexListItem["sub"];
                    // extend
                    while (view.byteLength < offset + 1) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // tuple tuple u8
                    view.setUint8(offset, indexListItemSub["u8"], false);
                    offset = offset + 1;
                    // extend
                    while (view.byteLength < offset + 2) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // tuple tuple str
                    const indexListItemSubStrArray = textEncoder.encode(indexListItemSub["str"]);
                    view.setUint16(offset, indexListItemSubStrArray.length, false);
                    offset = offset + 2;
                    // extend
                    while (view.byteLength < offset + indexListItemSubStrArray.length) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    (new Uint8Array(view.buffer, offset)).set(indexListItemSubStrArray);
                    offset = offset + indexListItemSubStrArray.length;
                    // extend
                    while (view.byteLength < offset + 2) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // tuple list
                    const indexListItemList = indexListItem["list"];
                    view.setUint16(offset, indexListItemList.length, false);
                    offset = offset + 2;
                    for (const indexListItemListItem of indexListItemList) {
                        // extend
                        while (view.byteLength < offset + 2) {
                            const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                            (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                            view = extendView;
                        }
                        // tuple list i16
                        view.setInt16(offset, indexListItemListItem["i16"], false);
                        offset = offset + 2;
                        // extend
                        while (view.byteLength < offset + 2) {
                            const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                            (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                            view = extendView;
                        }
                        // tuple list bst
                        const indexListItemListItemBstArray = textEncoder.encode(indexListItemListItem["bst"]);
                        view.setUint16(offset, indexListItemListItemBstArray.length, false);
                        offset = offset + 2;
                        // extend
                        while (view.byteLength < offset + indexListItemListItemBstArray.length) {
                            const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                            (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                            view = extendView;
                        }
                        (new Uint8Array(view.buffer, offset)).set(indexListItemListItemBstArray);
                        offset = offset + indexListItemListItemBstArray.length;
                    }
                    // extend
                    while (view.byteLength < offset + 2) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // 
                    const indexListItemSingle = indexListItem["single"];
                    view.setUint16(offset, indexListItemSingle.length, false);
                    offset = offset + 2;
                    for (const indexListItemSingleItem of indexListItemSingle) {
                        // extend
                        while (view.byteLength < offset + 1) {
                            const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                            (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                            view = extendView;
                        }
                        // bool
                        view.setUint8(offset, indexListItemSingleItem ? 1 : 0, false);
                        offset = offset + 1;
                    }
                }
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 
                const keyList = data["keyList"];
                view.setUint16(offset, Object.keys(keyList).length, false);
                offset = offset + 2;
                for (const keyListKey in keyList) {
                    const keyListItem = keyList[keyListKey];
                    // extend
                    while (view.byteLength < offset + 6) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // binary
                    const keyListItemBinary = keyListItem["binary"];
                    (new Uint8Array(view.buffer, offset)).set(new Uint8Array(keyListItemBinary));
                    offset = offset + keyListItemBinary.byteLength;
                    // extend
                    while (view.byteLength < offset + 1) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // bool
                    view.setUint8(offset, keyListItem["boolean"] ? 1 : 0, false);
                    offset = offset + 1;
                    // extend
                    while (view.byteLength < offset + 1) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // u8
                    view.setUint8(offset, keyListItem["u8"], false);
                    offset = offset + 1;
                    // extend
                    while (view.byteLength < offset + 2) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // u16
                    view.setUint16(offset, keyListItem["u16"], false);
                    offset = offset + 2;
                    // extend
                    while (view.byteLength < offset + 4) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // u32
                    view.setUint32(offset, keyListItem["u32"], false);
                    offset = offset + 4;
                    // extend
                    while (view.byteLength < offset + 8) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // u64
                    view.setBigUint64(offset, keyListItem["u64"], false);
                    offset = offset + 8;
                    // extend
                    while (view.byteLength < offset + 1) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // i8
                    view.setInt8(offset, keyListItem["i8"], false);
                    offset = offset + 1;
                    // extend
                    while (view.byteLength < offset + 2) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // i16
                    view.setInt16(offset, keyListItem["i16"], false);
                    offset = offset + 2;
                    // extend
                    while (view.byteLength < offset + 4) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // i32
                    view.setInt32(offset, keyListItem["i32"], false);
                    offset = offset + 4;
                    // extend
                    while (view.byteLength < offset + 8) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // i64
                    view.setBigInt64(offset, keyListItem["i64"], false);
                    offset = offset + 8;
                    // extend
                    while (view.byteLength < offset + 4) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // f32
                    view.setFloat32(offset, keyListItem["f32"], false);
                    offset = offset + 4;
                    // extend
                    while (view.byteLength < offset + 8) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // f64
                    view.setFloat64(offset, keyListItem["f64"], false);
                    offset = offset + 8;
                    // extend
                    while (view.byteLength < offset + 2) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // str
                    const keyListItemStrArray = textEncoder.encode(keyListItem["str"]);
                    view.setUint16(offset, keyListItemStrArray.length, false);
                    offset = offset + 2;
                    // extend
                    while (view.byteLength < offset + keyListItemStrArray.length) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    (new Uint8Array(view.buffer, offset)).set(keyListItemStrArray);
                    offset = offset + keyListItemStrArray.length;
                    // extend
                    while (view.byteLength < offset + 2) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // bst
                    const keyListItemBstArray = textEncoder.encode(keyListItem["bst"]);
                    view.setUint16(offset, keyListItemBstArray.length, false);
                    offset = offset + 2;
                    // extend
                    while (view.byteLength < offset + keyListItemBstArray.length) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    (new Uint8Array(view.buffer, offset)).set(keyListItemBstArray);
                    offset = offset + keyListItemBstArray.length;
                }
                return new DataView(view.buffer.slice(0, offset));
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }

    static decode(textDecoder, view, offset, protocol) {
        switch (protocol) {
            case 65533: {
                // single i16
                const data = view.getInt16(offset, false);
                offset = offset + 2;
                return data;
            }
            case 65534: {
                // single list
                const data = [];
                let dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataLength >= 0) {
                    // single u32
                    const item = view.getUint32(offset, false);
                    offset = offset + 4;
                    // add
                    data.push(item);
                }
                return data;
            }
            case 65535: {
                // 
                // binary
                const binary = view.buffer.slice(offset, offset + 6);
                offset = offset + 6;
                // bool
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
                // i16
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
                // tuple binary
                const tupleBinary = view.buffer.slice(offset, offset + 6);
                offset = offset + 6;
                // tuple tuple
                // tuple tuple u8
                const tupleSubU8 = view.getUint8(offset, false);
                offset = offset + 1;
                // tuple tuple str
                const tupleSubStrLength = view.getUint16(offset, false);
                offset = offset + 2;
                const tupleSubStrArray = new Uint8Array(view.buffer.slice(offset, offset + tupleSubStrLength));
                const tupleSubStr = textDecoder.decode(tupleSubStrArray);
                offset = offset + tupleSubStrLength;
                // object
                const tupleSub = {"u8": tupleSubU8, "str": tupleSubStr};
                // tuple list
                const tupleList = [];
                let tupleListLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--tupleListLength >= 0) {
                    // 
                    // tuple list i16
                    const tupleListI16 = view.getInt16(offset, false);
                    offset = offset + 2;
                    // tuple list bst
                    const tupleListBstLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    const tupleListBstArray = new Uint8Array(view.buffer.slice(offset, offset + tupleListBstLength));
                    const tupleListBst = textDecoder.decode(tupleListBstArray);
                    offset = offset + tupleListBstLength;
                    // object
                    const tupleListItem = {"i16": tupleListI16, "bst": tupleListBst};
                    // add
                    tupleList.push(tupleListItem);
                }
                // 
                const tupleSingle = [];
                let tupleSingleLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--tupleSingleLength >= 0) {
                    // bool
                    const tupleSingleItem = view.getUint8(offset, false) !== 0;
                    offset = offset + 1;
                    // add
                    tupleSingle.push(tupleSingleItem);
                }
                // object
                const tuple = {"binary": tupleBinary, "sub": tupleSub, "list": tupleList, "single": tupleSingle};
                // list
                const indexList = [];
                let indexListLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--indexListLength >= 0) {
                    // 
                    // tuple binary
                    const indexListBinary = view.buffer.slice(offset, offset + 6);
                    offset = offset + 6;
                    // tuple tuple
                    // tuple tuple u8
                    const indexListSubU8 = view.getUint8(offset, false);
                    offset = offset + 1;
                    // tuple tuple str
                    const indexListSubStrLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    const indexListSubStrArray = new Uint8Array(view.buffer.slice(offset, offset + indexListSubStrLength));
                    const indexListSubStr = textDecoder.decode(indexListSubStrArray);
                    offset = offset + indexListSubStrLength;
                    // object
                    const indexListSub = {"u8": indexListSubU8, "str": indexListSubStr};
                    // tuple list
                    const indexListList = [];
                    let indexListListLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    while (--indexListListLength >= 0) {
                        // 
                        // tuple list i16
                        const indexListListI16 = view.getInt16(offset, false);
                        offset = offset + 2;
                        // tuple list bst
                        const indexListListBstLength = view.getUint16(offset, false);
                        offset = offset + 2;
                        const indexListListBstArray = new Uint8Array(view.buffer.slice(offset, offset + indexListListBstLength));
                        const indexListListBst = textDecoder.decode(indexListListBstArray);
                        offset = offset + indexListListBstLength;
                        // object
                        const indexListListItem = {"i16": indexListListI16, "bst": indexListListBst};
                        // add
                        indexListList.push(indexListListItem);
                    }
                    // 
                    const indexListSingle = [];
                    let indexListSingleLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    while (--indexListSingleLength >= 0) {
                        // bool
                        const indexListSingleItem = view.getUint8(offset, false) !== 0;
                        offset = offset + 1;
                        // add
                        indexListSingle.push(indexListSingleItem);
                    }
                    // object
                    const indexListItem = {"binary": indexListBinary, "sub": indexListSub, "list": indexListList, "single": indexListSingle};
                    // add
                    indexList.push(indexListItem);
                }
                // 
                const keyList = {};
                let keyListLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--keyListLength >= 0) {
                    // 
                    // binary
                    const keyListBinary = view.buffer.slice(offset, offset + 6);
                    offset = offset + 6;
                    // boolean
                    const keyListBoolean = view.getUint8(offset, false) !== 0;
                    offset = offset + 1;
                    // u8
                    const keyListU8 = view.getUint8(offset, false);
                    offset = offset + 1;
                    // u16
                    const keyListU16 = view.getUint16(offset, false);
                    offset = offset + 2;
                    // u32
                    const keyListU32 = view.getUint32(offset, false);
                    offset = offset + 4;
                    // u64
                    const keyListU64 = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // i8
                    const keyListI8 = view.getInt8(offset, false);
                    offset = offset + 1;
                    // i16
                    const keyListI16 = view.getInt16(offset, false);
                    offset = offset + 2;
                    // i32
                    const keyListI32 = view.getInt32(offset, false);
                    offset = offset + 4;
                    // i64
                    const keyListI64 = view.getBigInt64(offset, false);
                    offset = offset + 8;
                    // f32
                    const keyListF32 = view.getFloat32(offset, false);
                    offset = offset + 4;
                    // f64
                    const keyListF64 = view.getFloat64(offset, false);
                    offset = offset + 8;
                    // str
                    const keyListStrLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    const keyListStrArray = new Uint8Array(view.buffer.slice(offset, offset + keyListStrLength));
                    const keyListStr = textDecoder.decode(keyListStrArray);
                    offset = offset + keyListStrLength;
                    // bst
                    const keyListBstLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    const keyListBstArray = new Uint8Array(view.buffer.slice(offset, offset + keyListBstLength));
                    const keyListBst = textDecoder.decode(keyListBstArray);
                    offset = offset + keyListBstLength;
                    // object
                    const keyListItem = {"binary": keyListBinary, "boolean": keyListBoolean, "u8": keyListU8, "u16": keyListU16, "u32": keyListU32, "u64": keyListU64, "i8": keyListI8, "i16": keyListI16, "i32": keyListI32, "i64": keyListI64, "f32": keyListF32, "f64": keyListF64, "str": keyListStr, "bst": keyListBst};
                    // add
                    keyList[u8] = keyListItem;
                }
                // object
                const data = {"binary": binary, "boolean": boolean, "u8": u8, "u16": u16, "u32": u32, "u64": u64, "i8": i8, "i16": i16, "i32": i32, "i64": i64, "f32": f32, "f64": f64, "str": str, "bst": bst, "tuple": tuple, "indexList": indexList, "keyList": keyList};
                return data;
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }
}