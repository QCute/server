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
                const binary = data["data"]["binary"];
                (new Uint8Array(view.buffer, offset)).set(new Uint8Array(binary));
                offset = offset + binary.byteLength;
                // extend
                while (view.byteLength < offset + 1) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // bool
                view.setUint8(offset, data["data"]["bool"] ? 1 : 0, false);
                offset = offset + 1;
                // extend
                while (view.byteLength < offset + 1) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // u8
                view.setUint8(offset, data["data"]["u8"], false);
                offset = offset + 1;
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // u16
                view.setUint16(offset, data["data"]["u16"], false);
                offset = offset + 2;
                // extend
                while (view.byteLength < offset + 4) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // u32
                view.setUint32(offset, data["data"]["u32"], false);
                offset = offset + 4;
                // extend
                while (view.byteLength < offset + 8) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // u64
                view.setBigUint64(offset, data["data"]["u64"], false);
                offset = offset + 8;
                // extend
                while (view.byteLength < offset + 1) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // i8
                view.setInt8(offset, data["data"]["i8"], false);
                offset = offset + 1;
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // i16
                view.setInt16(offset, data["data"]["i16"], false);
                offset = offset + 2;
                // extend
                while (view.byteLength < offset + 4) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // i32
                view.setInt32(offset, data["data"]["i32"], false);
                offset = offset + 4;
                // extend
                while (view.byteLength < offset + 8) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // i16
                view.setBigInt64(offset, data["data"]["i64"], false);
                offset = offset + 8;
                // extend
                while (view.byteLength < offset + 4) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // f32
                view.setFloat32(offset, data["data"]["f32"], false);
                offset = offset + 4;
                // extend
                while (view.byteLength < offset + 8) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // f64
                view.setFloat64(offset, data["data"]["f64"], false);
                offset = offset + 8;
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // str
                const strArray = textEncoder.encode(data["data"]["str"]);
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
                const bstArray = textEncoder.encode(data["data"]["bst"]);
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
                // tuple binary
                const binary = data["data"]["tuple"]["binary"];
                (new Uint8Array(view.buffer, offset)).set(new Uint8Array(binary));
                offset = offset + binary.byteLength;
                // extend
                while (view.byteLength < offset + 1) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // tuple tuple u8
                view.setUint8(offset, data["data"]["tuple"]["sub"]["u8"], false);
                offset = offset + 1;
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // tuple tuple str
                const strArray = textEncoder.encode(data["data"]["tuple"]["sub"]["str"]);
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
                // tuple list
                const listData = data["data"]["tuple"]["list"];
                view.setUint16(offset, listData.length, false);
                offset = offset + 2;
                for (const listDataItem of listData) {
                    // extend
                    while (view.byteLength < offset + 2) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // tuple list i16
                    view.setInt16(offset, listDataItem["i16"], false);
                    offset = offset + 2;
                    // extend
                    while (view.byteLength < offset + 2) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // tuple list bst
                    const bstArray = textEncoder.encode(listDataItem["bst"]);
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
                }
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 
                const singleData = data["data"]["tuple"]["single"];
                view.setUint16(offset, singleData.length, false);
                offset = offset + 2;
                for (const singleDataItem of singleData) {
                    // extend
                    while (view.byteLength < offset + 1) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // bool
                    view.setUint8(offset, singleDataItem ? 1 : 0, false);
                    offset = offset + 1;
                }
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // list
                const indexListData = data["data"]["indexList"];
                view.setUint16(offset, indexListData.length, false);
                offset = offset + 2;
                for (const indexListDataItem of indexListData) {
                    // extend
                    while (view.byteLength < offset + 6) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // tuple binary
                    const binary = indexListDataItem["binary"];
                    (new Uint8Array(view.buffer, offset)).set(new Uint8Array(binary));
                    offset = offset + binary.byteLength;
                    // extend
                    while (view.byteLength < offset + 1) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // tuple tuple u8
                    view.setUint8(offset, indexListDataItem["sub"]["u8"], false);
                    offset = offset + 1;
                    // extend
                    while (view.byteLength < offset + 2) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // tuple tuple str
                    const strArray = textEncoder.encode(indexListDataItem["sub"]["str"]);
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
                    // tuple list
                    const listData = indexListDataItem["list"];
                    view.setUint16(offset, listData.length, false);
                    offset = offset + 2;
                    for (const listDataItem of listData) {
                        // extend
                        while (view.byteLength < offset + 2) {
                            const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                            (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                            view = extendView;
                        }
                        // tuple list i16
                        view.setInt16(offset, listDataItem["i16"], false);
                        offset = offset + 2;
                        // extend
                        while (view.byteLength < offset + 2) {
                            const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                            (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                            view = extendView;
                        }
                        // tuple list bst
                        const bstArray = textEncoder.encode(listDataItem["bst"]);
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
                    }
                    // extend
                    while (view.byteLength < offset + 2) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // 
                    const singleData = indexListDataItem["single"];
                    view.setUint16(offset, singleData.length, false);
                    offset = offset + 2;
                    for (const singleDataItem of singleData) {
                        // extend
                        while (view.byteLength < offset + 1) {
                            const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                            (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                            view = extendView;
                        }
                        // bool
                        view.setUint8(offset, singleDataItem ? 1 : 0, false);
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
                const keyListData = data["data"]["keyList"];
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
                    // binary
                    const binary = keyListDataItem["binary"];
                    (new Uint8Array(view.buffer, offset)).set(new Uint8Array(binary));
                    offset = offset + binary.byteLength;
                    // extend
                    while (view.byteLength < offset + 1) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // bool
                    view.setUint8(offset, keyListDataItem["bool"] ? 1 : 0, false);
                    offset = offset + 1;
                    // extend
                    while (view.byteLength < offset + 1) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // u8
                    view.setUint8(offset, keyListDataItem["u8"], false);
                    offset = offset + 1;
                    // extend
                    while (view.byteLength < offset + 2) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // u16
                    view.setUint16(offset, keyListDataItem["u16"], false);
                    offset = offset + 2;
                    // extend
                    while (view.byteLength < offset + 4) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // u32
                    view.setUint32(offset, keyListDataItem["u32"], false);
                    offset = offset + 4;
                    // extend
                    while (view.byteLength < offset + 8) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // u64
                    view.setBigUint64(offset, keyListDataItem["u64"], false);
                    offset = offset + 8;
                    // extend
                    while (view.byteLength < offset + 1) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // i8
                    view.setInt8(offset, keyListDataItem["i8"], false);
                    offset = offset + 1;
                    // extend
                    while (view.byteLength < offset + 2) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // i16
                    view.setInt16(offset, keyListDataItem["i16"], false);
                    offset = offset + 2;
                    // extend
                    while (view.byteLength < offset + 4) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // i32
                    view.setInt32(offset, keyListDataItem["i32"], false);
                    offset = offset + 4;
                    // extend
                    while (view.byteLength < offset + 8) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // i64
                    view.setBigInt64(offset, keyListDataItem["i64"], false);
                    offset = offset + 8;
                    // extend
                    while (view.byteLength < offset + 4) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // f32
                    view.setFloat32(offset, keyListDataItem["f32"], false);
                    offset = offset + 4;
                    // extend
                    while (view.byteLength < offset + 8) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // f64
                    view.setFloat64(offset, keyListDataItem["f64"], false);
                    offset = offset + 8;
                    // extend
                    while (view.byteLength < offset + 2) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // str
                    const strArray = textEncoder.encode(keyListDataItem["str"]);
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
                    const bstArray = textEncoder.encode(keyListDataItem["bst"]);
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
                }
                return new DataView(view.buffer.slice(0, offset));
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }

    static decode(textDecoder, view, offset, protocol) {
        switch (protocol) {
            case 65535: {
                // 
                // binary
                const binary = view.buffer.slice(offset, offset + 6);
                offset = offset + 6;
                // bool
                const bool = view.getUint8(offset, false) !== 0;
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
                const binary = view.buffer.slice(offset, offset + 6);
                offset = offset + 6;
                // tuple tuple
                // tuple tuple u8
                const u8 = view.getUint8(offset, false);
                offset = offset + 1;
                // tuple tuple str
                const strLength = view.getUint16(offset, false);
                offset = offset + 2;
                const strArray = new Uint8Array(view.buffer.slice(offset, offset + strLength));
                const str = textDecoder.decode(strArray);
                offset = offset + strLength;
                // object
                const sub = {u8, str};
                // tuple list
                const list = [];
                let listLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--listLength >= 0) {
                    // 
                    // tuple list i16
                    const i16 = view.getInt16(offset, false);
                    offset = offset + 2;
                    // tuple list bst
                    const bstLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    const bstArray = new Uint8Array(view.buffer.slice(offset, offset + bstLength));
                    const bst = textDecoder.decode(bstArray);
                    offset = offset + bstLength;
                    // object
                    const listItem = {i16, bst};
                    // add
                    list.push(listItem);
                }
                // 
                const single = [];
                let singleLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--singleLength >= 0) {
                    // bool
                    const singleItem = view.getUint8(offset, false) !== 0;
                    offset = offset + 1;
                    // add
                    single.push(singleItem);
                }
                // object
                const tuple = {binary, sub, list, single};
                // list
                const indexList = [];
                let indexListLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--indexListLength >= 0) {
                    // 
                    // tuple binary
                    const binary = view.buffer.slice(offset, offset + 6);
                    offset = offset + 6;
                    // tuple tuple
                    // tuple tuple u8
                    const u8 = view.getUint8(offset, false);
                    offset = offset + 1;
                    // tuple tuple str
                    const strLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    const strArray = new Uint8Array(view.buffer.slice(offset, offset + strLength));
                    const str = textDecoder.decode(strArray);
                    offset = offset + strLength;
                    // object
                    const sub = {u8, str};
                    // tuple list
                    const list = [];
                    let listLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    while (--listLength >= 0) {
                        // 
                        // tuple list i16
                        const i16 = view.getInt16(offset, false);
                        offset = offset + 2;
                        // tuple list bst
                        const bstLength = view.getUint16(offset, false);
                        offset = offset + 2;
                        const bstArray = new Uint8Array(view.buffer.slice(offset, offset + bstLength));
                        const bst = textDecoder.decode(bstArray);
                        offset = offset + bstLength;
                        // object
                        const listItem = {i16, bst};
                        // add
                        list.push(listItem);
                    }
                    // 
                    const single = [];
                    let singleLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    while (--singleLength >= 0) {
                        // bool
                        const singleItem = view.getUint8(offset, false) !== 0;
                        offset = offset + 1;
                        // add
                        single.push(singleItem);
                    }
                    // object
                    const indexListItem = {binary, sub, list, single};
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
                    const binary = view.buffer.slice(offset, offset + 6);
                    offset = offset + 6;
                    // bool
                    const bool = view.getUint8(offset, false) !== 0;
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
                    // object
                    const keyListItem = {binary, bool, u8, u16, u32, u64, i8, i16, i32, i64, f32, f64, str, bst};
                    // add
                    keyList[u8] = keyListItem;
                }
                // object
                const data = {binary, bool, u8, u16, u32, u64, i8, i16, i32, i64, f32, f64, str, bst, tuple, indexList, keyList};
                return data;
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }
}