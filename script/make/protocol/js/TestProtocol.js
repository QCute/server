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
                // bin
                const bin = data[""]["bin"];
                (new Uint8Array(view.buffer, offset)).set(new Uint8Array(bin));
                offset = offset + bin.byteLength;
                // extend
                while (view.byteLength < offset + 1) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // bool
                view.setUint8(offset, data[""]["bool"] ? 1 : 0, false);
                offset = offset + 1;
                // extend
                while (view.byteLength < offset + 1) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // u8
                view.setUint8(offset, data[""]["u8"], false);
                offset = offset + 1;
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // u16
                view.setUint16(offset, data[""]["u16"], false);
                offset = offset + 2;
                // extend
                while (view.byteLength < offset + 4) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // u32
                view.setUint32(offset, data[""]["u32"], false);
                offset = offset + 4;
                // extend
                while (view.byteLength < offset + 8) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // u64
                view.setBigUint64(offset, data[""]["u64"], false);
                offset = offset + 8;
                // extend
                while (view.byteLength < offset + 1) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // i8
                view.setInt8(offset, data[""]["i8"], false);
                offset = offset + 1;
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // i16
                view.setInt16(offset, data[""]["i16"], false);
                offset = offset + 2;
                // extend
                while (view.byteLength < offset + 4) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // i32
                view.setInt32(offset, data[""]["i32"], false);
                offset = offset + 4;
                // extend
                while (view.byteLength < offset + 8) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // i16
                view.setBigInt64(offset, data[""]["i64"], false);
                offset = offset + 8;
                // extend
                while (view.byteLength < offset + 4) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // f32
                view.setFloat32(offset, data[""]["f32"], false);
                offset = offset + 4;
                // extend
                while (view.byteLength < offset + 8) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // f64
                view.setFloat64(offset, data[""]["f64"], false);
                offset = offset + 8;
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // str
                const strArray = textEncoder.encode(data[""]["str"]);
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
                const bstArray = textEncoder.encode(data[""]["bst"]);
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
                // tuple bin
                const bin = data[""]["tuple"]["bin"];
                (new Uint8Array(view.buffer, offset)).set(new Uint8Array(bin));
                offset = offset + bin.byteLength;
                // extend
                while (view.byteLength < offset + 1) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // tuple tuple u8
                view.setUint8(offset, data[""]["tuple"]["sub"]["u8"], false);
                offset = offset + 1;
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // tuple tuple str
                const strArray = textEncoder.encode(data[""]["tuple"]["sub"]["str"]);
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
                const listData = data[""]["tuple"]["list"];
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
                // u8
                const singleData = data[""]["tuple"]["single"];
                view.setUint16(offset, singleData.length, false);
                offset = offset + 2;
                for (const singleDataItem of singleData) {
                    // extend
                    while (view.byteLength < offset + 1) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // 
                    view.setUint8(offset, singleDataItem, false);
                    offset = offset + 1;
                }
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // list
                const indexListData = data[""]["indexList"];
                view.setUint16(offset, indexListData.length, false);
                offset = offset + 2;
                for (const indexListDataItem of indexListData) {
                    // extend
                    while (view.byteLength < offset + 6) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // tuple bin
                    const bin = indexListDataItem["bin"];
                    (new Uint8Array(view.buffer, offset)).set(new Uint8Array(bin));
                    offset = offset + bin.byteLength;
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
                    // u8
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
                        // 
                        view.setUint8(offset, singleDataItem, false);
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
                const keyListData = data[""]["keyList"];
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
                    // bin
                    const bin = keyListDataItem["bin"];
                    (new Uint8Array(view.buffer, offset)).set(new Uint8Array(bin));
                    offset = offset + bin.byteLength;
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
                const dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataArray = new Uint8Array(view.buffer.slice(offset, offset + dataLength));
                const data = textDecoder.decode(dataArray);
                offset = offset + dataLength;
                return data;
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }
}