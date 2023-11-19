export class TestTestSingleProtocolRequest {
    /** @type {number} protocol **/
    protocol = 65531;
    /**
     * @type {number} data
    **/
    data;
}

export class TestTestSingleProtocolResponse {
    /** @type {number} protocol **/
    protocol = 65531;
    /**
     * @type {number} data
    **/
    data;
}

export class TestTestListProtocolRequest {
    /** @type {number} protocol **/
    protocol = 65532;
    /**
     * @type {Array<number>} data
    **/
    data;
}

export class TestTestListProtocolResponse {
    /** @type {number} protocol **/
    protocol = 65532;
    /**
     * @type {Array<number>} data
    **/
    data;
}

export class TestTestListTupleProtocolRequest {
    /** @type {number} protocol **/
    protocol = 65533;
    /**
     * @type {Array<{
     *     id: BigInt;                                                                              // single u64
     * }>} data
    **/
    data;
}

export class TestTestListTupleProtocolResponse {
    /** @type {number} protocol **/
    protocol = 65533;
    /**
     * @type {Array<{
     *     name: string;                                                                            // single ast
     * }>} data
    **/
    data;
}

export class TestTestLsProtocolRequest {
    /** @type {number} protocol **/
    protocol = 65534;
    /**
     * @type {{
     *     id: number;                                                                              // single u32
     *     theItemData: {
     *         itemId: number;                                                                      // 
     *         type: number;                                                                        // single u16
     *     };                                                                                       // 
     *     name: {
     *         ls: Array<{
     *             ix: number;                                                                      // 
     *             nx: string;                                                                      // 
     *             rx: Array<number>;                                                               // 
     *             sx: Array<{
     *                 ii: number;                                                                  // a list ii u32
     *                 nn: string;                                                                  // a list nn bst
     *             }>;                                                                              // 
     *         }>;                                                                                  // 
     *         lss: Array<number>;                                                                  // 
     *     };                                                                                       // 
     * }} data
    **/
    data;
}

export class TestTestLsProtocolResponse {
    /** @type {number} protocol **/
    protocol = 65534;
    /**
     * @type {Array<{
     *     name: number;                                                                            // single u32
     * }>} data
    **/
    data;
}

export class TestTestProtocolRequest {
    /** @type {number} protocol **/
    protocol = 65535;
    /**
     * @type {{
     *     binary: Uint8Array;                                                                      // binary
     *     boolean: boolean;                                                                        // bool
     *     u8: number;                                                                              // u8
     *     u16: number;                                                                             // u16
     *     u32: number;                                                                             // u32
     *     u64: BigInt;                                                                             // u64
     *     i8: number;                                                                              // i8
     *     i16: number;                                                                             // i16
     *     i32: number;                                                                             // i32
     *     i64: BigInt;                                                                             // i16
     *     f32: number;                                                                             // f32
     *     f64: number;                                                                             // f64
     *     str: string;                                                                             // str
     *     bst: string;                                                                             // bst
     *     tuple: {
     *         binary: Uint8Array;                                                                  // tuple binary
     *         sub: {
     *             u8: number;                                                                      // tuple tuple u8
     *             str: string;                                                                     // tuple tuple str
     *         };                                                                                   // tuple tuple
     *         list: Array<{
     *             i16: number;                                                                     // tuple list i16
     *             bst: string;                                                                     // tuple list bst
     *         }>;                                                                                  // tuple list
     *         single: Array<boolean>;                                                              // 
     *     };                                                                                       // tuple
     *     indexList: Array<{
     *         binary: Uint8Array;                                                                  // tuple binary
     *         sub: {
     *             u8: number;                                                                      // tuple tuple u8
     *             str: string;                                                                     // tuple tuple str
     *         };                                                                                   // tuple tuple
     *         list: Array<{
     *             i16: number;                                                                     // tuple list i16
     *             bst: string;                                                                     // tuple list bst
     *         }>;                                                                                  // tuple list
     *         single: Array<boolean>;                                                              // 
     *     }>;                                                                                      // list
     *     keyList: {[u8: boolean|number|string|BigInt]: {
     *         binary: Uint8Array;                                                                  // binary
     *         boolean: boolean;                                                                    // bool
     *         u8: number;                                                                          // u8
     *         u16: number;                                                                         // u16
     *         u32: number;                                                                         // u32
     *         u64: BigInt;                                                                         // u64
     *         i8: number;                                                                          // i8
     *         i16: number;                                                                         // i16
     *         i32: number;                                                                         // i32
     *         i64: BigInt;                                                                         // i64
     *         f32: number;                                                                         // f32
     *         f64: number;                                                                         // f64
     *         str: string;                                                                         // str
     *         bst: string;                                                                         // bst
     *     }};                                                                                      // 
     * }} data
    **/
    data;
}

export class TestTestProtocolResponse {
    /** @type {number} protocol **/
    protocol = 65535;
    /**
     * @type {{
     *     binary: Uint8Array;                                                                      // binary
     *     boolean: boolean;                                                                        // bool
     *     u8: number;                                                                              // u8
     *     u16: number;                                                                             // u16
     *     u32: number;                                                                             // u32
     *     u64: BigInt;                                                                             // u64
     *     i8: number;                                                                              // i8
     *     i16: number;                                                                             // i16
     *     i32: number;                                                                             // i32
     *     i64: BigInt;                                                                             // i16
     *     f32: number;                                                                             // f32
     *     f64: number;                                                                             // f64
     *     str: string;                                                                             // str
     *     bst: string;                                                                             // bst
     *     tuple: {
     *         binary: Uint8Array;                                                                  // tuple binary
     *         sub: {
     *             u8: number;                                                                      // tuple tuple u8
     *             str: string;                                                                     // tuple tuple str
     *         };                                                                                   // tuple tuple
     *         list: Array<{
     *             i16: number;                                                                     // tuple list i16
     *             bst: string;                                                                     // tuple list bst
     *         }>;                                                                                  // tuple list
     *         single: Array<boolean>;                                                              // 
     *     };                                                                                       // tuple
     *     indexList: Array<{
     *         binary: Uint8Array;                                                                  // tuple binary
     *         sub: {
     *             u8: number;                                                                      // tuple tuple u8
     *             str: string;                                                                     // tuple tuple str
     *         };                                                                                   // tuple tuple
     *         list: Array<{
     *             i16: number;                                                                     // tuple list i16
     *             bst: string;                                                                     // tuple list bst
     *         }>;                                                                                  // tuple list
     *         single: Array<boolean>;                                                              // 
     *     }>;                                                                                      // list
     *     keyList: {[u8: boolean|number|string|BigInt]: {
     *         binary: Uint8Array;                                                                  // binary
     *         boolean: boolean;                                                                    // boolean
     *         u8: number;                                                                          // u8
     *         u16: number;                                                                         // u16
     *         u32: number;                                                                         // u32
     *         u64: BigInt;                                                                         // u64
     *         i8: number;                                                                          // i8
     *         i16: number;                                                                         // i16
     *         i32: number;                                                                         // i32
     *         i64: BigInt;                                                                         // i64
     *         f32: number;                                                                         // f32
     *         f64: number;                                                                         // f64
     *         str: string;                                                                         // str
     *         bst: string;                                                                         // bst
     *     }};                                                                                      // 
     * }} data
    **/
    data;
}

export default class TestProtocol {
    static encode(textEncoder, view, offset, protocol, data) {
        switch (protocol) {
            case 65531: {
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
            case 65532: {

                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // single list
                view.setUint16(offset, data.length, false);
                offset = offset + 2;
                for (const dataData of data) {
                    // extend
                    while (view.byteLength < offset + 4) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // single u32
                    view.setUint32(offset, dataData, false);
                    offset = offset + 4;
                }
                return new DataView(view.buffer.slice(0, offset));
            }
            case 65533: {

                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // single list
                view.setUint16(offset, data.length, false);
                offset = offset + 2;
                for (const dataData of data) {

                    // extend
                    while (view.byteLength < offset + 8) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // single u64
                    view.setBigUint64(offset, dataData.id, false);
                    offset = offset + 8;
                }
                return new DataView(view.buffer.slice(0, offset));
            }
            case 65534: {

                // extend
                while (view.byteLength < offset + 4) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // single u32
                view.setUint32(offset, data.id, false);
                offset = offset + 4;
                const dataTheItemData = data.theItemData;
                // extend
                while (view.byteLength < offset + 4) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 
                view.setUint32(offset, dataTheItemData.itemId, false);
                offset = offset + 4;
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // single u16
                view.setUint16(offset, dataTheItemData.type, false);
                offset = offset + 2;
                const dataName = data.name;
                const dataNameLs = dataName.ls;
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 
                view.setUint16(offset, dataNameLs.length, false);
                offset = offset + 2;
                for (const dataNameLsData of dataNameLs) {

                    // extend
                    while (view.byteLength < offset + 4) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // 
                    view.setUint32(offset, dataNameLsData.ix, false);
                    offset = offset + 4;
                    // extend
                    while (view.byteLength < offset + 2) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // 
                    const dataNameLsDataNxArray = textEncoder.encode(dataNameLsData.nx);
                    view.setUint16(offset, dataNameLsDataNxArray.length, false);
                    offset = offset + 2;
                    // extend
                    while (view.byteLength < offset + dataNameLsDataNxArray.length) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    (new Uint8Array(view.buffer, offset)).set(dataNameLsDataNxArray);
                    offset = offset + dataNameLsDataNxArray.length;
                    const dataNameLsDataRx = dataNameLsData.rx;
                    // extend
                    while (view.byteLength < offset + 2) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // 
                    view.setUint16(offset, dataNameLsDataRx.length, false);
                    offset = offset + 2;
                    for (const dataNameLsDataRxData of dataNameLsDataRx) {
                        // extend
                        while (view.byteLength < offset + 4) {
                            const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                            (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                            view = extendView;
                        }
                        // a list u32
                        view.setUint32(offset, dataNameLsDataRxData, false);
                        offset = offset + 4;
                    }
                    const dataNameLsDataSx = dataNameLsData.sx;
                    // extend
                    while (view.byteLength < offset + 2) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // 
                    view.setUint16(offset, dataNameLsDataSx.length, false);
                    offset = offset + 2;
                    for (const dataNameLsDataSxData of dataNameLsDataSx) {

                        // extend
                        while (view.byteLength < offset + 4) {
                            const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                            (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                            view = extendView;
                        }
                        // a list ii u32
                        view.setUint32(offset, dataNameLsDataSxData.ii, false);
                        offset = offset + 4;
                        // extend
                        while (view.byteLength < offset + 2) {
                            const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                            (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                            view = extendView;
                        }
                        // a list nn bst
                        const dataNameLsDataSxDataNnArray = textEncoder.encode(dataNameLsDataSxData.nn);
                        view.setUint16(offset, dataNameLsDataSxDataNnArray.length, false);
                        offset = offset + 2;
                        // extend
                        while (view.byteLength < offset + dataNameLsDataSxDataNnArray.length) {
                            const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                            (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                            view = extendView;
                        }
                        (new Uint8Array(view.buffer, offset)).set(dataNameLsDataSxDataNnArray);
                        offset = offset + dataNameLsDataSxDataNnArray.length;
                    }
                }
                const dataNameLss = dataName.lss;
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 
                view.setUint16(offset, dataNameLss.length, false);
                offset = offset + 2;
                for (const dataNameLssData of dataNameLss) {
                    // extend
                    while (view.byteLength < offset + 1) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // single u8
                    view.setUint8(offset, dataNameLssData, false);
                    offset = offset + 1;
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
                const dataBinary = data.binary;
                (new Uint8Array(view.buffer, offset)).set(new Uint8Array(dataBinary));
                offset = offset + dataBinary.byteLength;
                // extend
                while (view.byteLength < offset + 1) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // bool
                view.setUint8(offset, data.boolean ? 1 : 0, false);
                offset = offset + 1;
                // extend
                while (view.byteLength < offset + 1) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // u8
                view.setUint8(offset, data.u8, false);
                offset = offset + 1;
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // u16
                view.setUint16(offset, data.u16, false);
                offset = offset + 2;
                // extend
                while (view.byteLength < offset + 4) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // u32
                view.setUint32(offset, data.u32, false);
                offset = offset + 4;
                // extend
                while (view.byteLength < offset + 8) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // u64
                view.setBigUint64(offset, data.u64, false);
                offset = offset + 8;
                // extend
                while (view.byteLength < offset + 1) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // i8
                view.setInt8(offset, data.i8, false);
                offset = offset + 1;
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // i16
                view.setInt16(offset, data.i16, false);
                offset = offset + 2;
                // extend
                while (view.byteLength < offset + 4) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // i32
                view.setInt32(offset, data.i32, false);
                offset = offset + 4;
                // extend
                while (view.byteLength < offset + 8) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // i16
                view.setBigInt64(offset, data.i64, false);
                offset = offset + 8;
                // extend
                while (view.byteLength < offset + 4) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // f32
                view.setFloat32(offset, data.f32, false);
                offset = offset + 4;
                // extend
                while (view.byteLength < offset + 8) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // f64
                view.setFloat64(offset, data.f64, false);
                offset = offset + 8;
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // str
                const dataStrArray = textEncoder.encode(data.str);
                view.setUint16(offset, dataStrArray.length, false);
                offset = offset + 2;
                // extend
                while (view.byteLength < offset + dataStrArray.length) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                (new Uint8Array(view.buffer, offset)).set(dataStrArray);
                offset = offset + dataStrArray.length;
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // bst
                const dataBstArray = textEncoder.encode(data.bst);
                view.setUint16(offset, dataBstArray.length, false);
                offset = offset + 2;
                // extend
                while (view.byteLength < offset + dataBstArray.length) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                (new Uint8Array(view.buffer, offset)).set(dataBstArray);
                offset = offset + dataBstArray.length;
                const dataTuple = data.tuple;
                // extend
                while (view.byteLength < offset + 6) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // tuple binary
                const dataTupleBinary = dataTuple.binary;
                (new Uint8Array(view.buffer, offset)).set(new Uint8Array(dataTupleBinary));
                offset = offset + dataTupleBinary.byteLength;
                const dataTupleSub = dataTuple.sub;
                // extend
                while (view.byteLength < offset + 1) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // tuple tuple u8
                view.setUint8(offset, dataTupleSub.u8, false);
                offset = offset + 1;
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // tuple tuple str
                const dataTupleSubStrArray = textEncoder.encode(dataTupleSub.str);
                view.setUint16(offset, dataTupleSubStrArray.length, false);
                offset = offset + 2;
                // extend
                while (view.byteLength < offset + dataTupleSubStrArray.length) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                (new Uint8Array(view.buffer, offset)).set(dataTupleSubStrArray);
                offset = offset + dataTupleSubStrArray.length;
                const dataTupleList = dataTuple.list;
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // tuple list
                view.setUint16(offset, dataTupleList.length, false);
                offset = offset + 2;
                for (const dataTupleListData of dataTupleList) {

                    // extend
                    while (view.byteLength < offset + 2) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // tuple list i16
                    view.setInt16(offset, dataTupleListData.i16, false);
                    offset = offset + 2;
                    // extend
                    while (view.byteLength < offset + 2) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // tuple list bst
                    const dataTupleListDataBstArray = textEncoder.encode(dataTupleListData.bst);
                    view.setUint16(offset, dataTupleListDataBstArray.length, false);
                    offset = offset + 2;
                    // extend
                    while (view.byteLength < offset + dataTupleListDataBstArray.length) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    (new Uint8Array(view.buffer, offset)).set(dataTupleListDataBstArray);
                    offset = offset + dataTupleListDataBstArray.length;
                }
                const dataTupleSingle = dataTuple.single;
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 
                view.setUint16(offset, dataTupleSingle.length, false);
                offset = offset + 2;
                for (const dataTupleSingleData of dataTupleSingle) {
                    // extend
                    while (view.byteLength < offset + 1) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // bool
                    view.setUint8(offset, dataTupleSingleData ? 1 : 0, false);
                    offset = offset + 1;
                }
                const dataIndexList = data.indexList;
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // list
                view.setUint16(offset, dataIndexList.length, false);
                offset = offset + 2;
                for (const dataIndexListData of dataIndexList) {

                    // extend
                    while (view.byteLength < offset + 6) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // tuple binary
                    const dataIndexListDataBinary = dataIndexListData.binary;
                    (new Uint8Array(view.buffer, offset)).set(new Uint8Array(dataIndexListDataBinary));
                    offset = offset + dataIndexListDataBinary.byteLength;
                    const dataIndexListDataSub = dataIndexListData.sub;
                    // extend
                    while (view.byteLength < offset + 1) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // tuple tuple u8
                    view.setUint8(offset, dataIndexListDataSub.u8, false);
                    offset = offset + 1;
                    // extend
                    while (view.byteLength < offset + 2) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // tuple tuple str
                    const dataIndexListDataSubStrArray = textEncoder.encode(dataIndexListDataSub.str);
                    view.setUint16(offset, dataIndexListDataSubStrArray.length, false);
                    offset = offset + 2;
                    // extend
                    while (view.byteLength < offset + dataIndexListDataSubStrArray.length) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    (new Uint8Array(view.buffer, offset)).set(dataIndexListDataSubStrArray);
                    offset = offset + dataIndexListDataSubStrArray.length;
                    const dataIndexListDataList = dataIndexListData.list;
                    // extend
                    while (view.byteLength < offset + 2) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // tuple list
                    view.setUint16(offset, dataIndexListDataList.length, false);
                    offset = offset + 2;
                    for (const dataIndexListDataListData of dataIndexListDataList) {

                        // extend
                        while (view.byteLength < offset + 2) {
                            const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                            (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                            view = extendView;
                        }
                        // tuple list i16
                        view.setInt16(offset, dataIndexListDataListData.i16, false);
                        offset = offset + 2;
                        // extend
                        while (view.byteLength < offset + 2) {
                            const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                            (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                            view = extendView;
                        }
                        // tuple list bst
                        const dataIndexListDataListDataBstArray = textEncoder.encode(dataIndexListDataListData.bst);
                        view.setUint16(offset, dataIndexListDataListDataBstArray.length, false);
                        offset = offset + 2;
                        // extend
                        while (view.byteLength < offset + dataIndexListDataListDataBstArray.length) {
                            const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                            (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                            view = extendView;
                        }
                        (new Uint8Array(view.buffer, offset)).set(dataIndexListDataListDataBstArray);
                        offset = offset + dataIndexListDataListDataBstArray.length;
                    }
                    const dataIndexListDataSingle = dataIndexListData.single;
                    // extend
                    while (view.byteLength < offset + 2) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // 
                    view.setUint16(offset, dataIndexListDataSingle.length, false);
                    offset = offset + 2;
                    for (const dataIndexListDataSingleData of dataIndexListDataSingle) {
                        // extend
                        while (view.byteLength < offset + 1) {
                            const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                            (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                            view = extendView;
                        }
                        // bool
                        view.setUint8(offset, dataIndexListDataSingleData ? 1 : 0, false);
                        offset = offset + 1;
                    }
                }
                const dataKeyList = data.keyList;
                // extend
                while (view.byteLength < offset + 2) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 
                view.setUint16(offset, Object.keys(dataKeyList).length, false);
                offset = offset + 2;
                for (const dataKeyListKey in dataKeyList) {
                    const dataKeyListData = dataKeyList[dataKeyListKey];

                    // extend
                    while (view.byteLength < offset + 6) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // binary
                    const dataKeyListDataBinary = dataKeyListData.binary;
                    (new Uint8Array(view.buffer, offset)).set(new Uint8Array(dataKeyListDataBinary));
                    offset = offset + dataKeyListDataBinary.byteLength;
                    // extend
                    while (view.byteLength < offset + 1) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // bool
                    view.setUint8(offset, dataKeyListData.boolean ? 1 : 0, false);
                    offset = offset + 1;
                    // extend
                    while (view.byteLength < offset + 1) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // u8
                    view.setUint8(offset, dataKeyListData.u8, false);
                    offset = offset + 1;
                    // extend
                    while (view.byteLength < offset + 2) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // u16
                    view.setUint16(offset, dataKeyListData.u16, false);
                    offset = offset + 2;
                    // extend
                    while (view.byteLength < offset + 4) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // u32
                    view.setUint32(offset, dataKeyListData.u32, false);
                    offset = offset + 4;
                    // extend
                    while (view.byteLength < offset + 8) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // u64
                    view.setBigUint64(offset, dataKeyListData.u64, false);
                    offset = offset + 8;
                    // extend
                    while (view.byteLength < offset + 1) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // i8
                    view.setInt8(offset, dataKeyListData.i8, false);
                    offset = offset + 1;
                    // extend
                    while (view.byteLength < offset + 2) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // i16
                    view.setInt16(offset, dataKeyListData.i16, false);
                    offset = offset + 2;
                    // extend
                    while (view.byteLength < offset + 4) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // i32
                    view.setInt32(offset, dataKeyListData.i32, false);
                    offset = offset + 4;
                    // extend
                    while (view.byteLength < offset + 8) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // i64
                    view.setBigInt64(offset, dataKeyListData.i64, false);
                    offset = offset + 8;
                    // extend
                    while (view.byteLength < offset + 4) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // f32
                    view.setFloat32(offset, dataKeyListData.f32, false);
                    offset = offset + 4;
                    // extend
                    while (view.byteLength < offset + 8) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // f64
                    view.setFloat64(offset, dataKeyListData.f64, false);
                    offset = offset + 8;
                    // extend
                    while (view.byteLength < offset + 2) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // str
                    const dataKeyListDataStrArray = textEncoder.encode(dataKeyListData.str);
                    view.setUint16(offset, dataKeyListDataStrArray.length, false);
                    offset = offset + 2;
                    // extend
                    while (view.byteLength < offset + dataKeyListDataStrArray.length) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    (new Uint8Array(view.buffer, offset)).set(dataKeyListDataStrArray);
                    offset = offset + dataKeyListDataStrArray.length;
                    // extend
                    while (view.byteLength < offset + 2) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    // bst
                    const dataKeyListDataBstArray = textEncoder.encode(dataKeyListData.bst);
                    view.setUint16(offset, dataKeyListDataBstArray.length, false);
                    offset = offset + 2;
                    // extend
                    while (view.byteLength < offset + dataKeyListDataBstArray.length) {
                        const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                        (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                        view = extendView;
                    }
                    (new Uint8Array(view.buffer, offset)).set(dataKeyListDataBstArray);
                    offset = offset + dataKeyListDataBstArray.length;
                }
                return new DataView(view.buffer.slice(0, offset));
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }

    static decode(textDecoder, view, offset, protocol) {
        switch (protocol) {
            case 65531: {
                // single i16
                const data = view.getInt16(offset, false);
                offset = offset + 2;
                return {"protocol": 65531, "data": data};
            }
            case 65532: {
                // single list
                const data = [];
                let dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataLength >= 0) {
                    // single u32
                    const dataData = view.getUint32(offset, false);
                    offset = offset + 4;
                    // add
                    data.push(dataData);
                }
                return {"protocol": 65532, "data": data};
            }
            case 65533: {
                // single list
                const data = [];
                let dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataLength >= 0) {
                    // 
                    // single ast
                    const dataDataNameLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    const dataDataNameArray = new Uint8Array(view.buffer.slice(offset, offset + dataDataNameLength));
                    const dataDataName = textDecoder.decode(dataDataNameArray);
                    offset = offset + dataDataNameLength;
                    // object
                    const dataData = {"name": dataDataName};
                    // add
                    data.push(dataData);
                }
                return {"protocol": 65533, "data": data};
            }
            case 65534: {
                // single list
                const data = [];
                let dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataLength >= 0) {
                    // 
                    // single u32
                    const dataDataName = view.getUint32(offset, false);
                    offset = offset + 4;
                    // object
                    const dataData = {"name": dataDataName};
                    // add
                    data.push(dataData);
                }
                return {"protocol": 65534, "data": data};
            }
            case 65535: {
                // 
                // binary
                const dataBinary = view.buffer.slice(offset, offset + 6);
                offset = offset + 6;
                // bool
                const dataBoolean = view.getUint8(offset, false) !== 0;
                offset = offset + 1;
                // u8
                const dataU8 = view.getUint8(offset, false);
                offset = offset + 1;
                // u16
                const dataU16 = view.getUint16(offset, false);
                offset = offset + 2;
                // u32
                const dataU32 = view.getUint32(offset, false);
                offset = offset + 4;
                // u64
                const dataU64 = view.getBigUint64(offset, false);
                offset = offset + 8;
                // i8
                const dataI8 = view.getInt8(offset, false);
                offset = offset + 1;
                // i16
                const dataI16 = view.getInt16(offset, false);
                offset = offset + 2;
                // i32
                const dataI32 = view.getInt32(offset, false);
                offset = offset + 4;
                // i16
                const dataI64 = view.getBigInt64(offset, false);
                offset = offset + 8;
                // f32
                const dataF32 = view.getFloat32(offset, false);
                offset = offset + 4;
                // f64
                const dataF64 = view.getFloat64(offset, false);
                offset = offset + 8;
                // str
                const dataStrLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataStrArray = new Uint8Array(view.buffer.slice(offset, offset + dataStrLength));
                const dataStr = textDecoder.decode(dataStrArray);
                offset = offset + dataStrLength;
                // bst
                const dataBstLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataBstArray = new Uint8Array(view.buffer.slice(offset, offset + dataBstLength));
                const dataBst = textDecoder.decode(dataBstArray);
                offset = offset + dataBstLength;
                // tuple
                // tuple binary
                const dataTupleBinary = view.buffer.slice(offset, offset + 6);
                offset = offset + 6;
                // tuple tuple
                // tuple tuple u8
                const dataTupleSubU8 = view.getUint8(offset, false);
                offset = offset + 1;
                // tuple tuple str
                const dataTupleSubStrLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataTupleSubStrArray = new Uint8Array(view.buffer.slice(offset, offset + dataTupleSubStrLength));
                const dataTupleSubStr = textDecoder.decode(dataTupleSubStrArray);
                offset = offset + dataTupleSubStrLength;
                // object
                const dataTupleSub = {"u8": dataTupleSubU8, "str": dataTupleSubStr};
                // tuple list
                const dataTupleList = [];
                let dataTupleListLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataTupleListLength >= 0) {
                    // 
                    // tuple list i16
                    const dataTupleListDataI16 = view.getInt16(offset, false);
                    offset = offset + 2;
                    // tuple list bst
                    const dataTupleListDataBstLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    const dataTupleListDataBstArray = new Uint8Array(view.buffer.slice(offset, offset + dataTupleListDataBstLength));
                    const dataTupleListDataBst = textDecoder.decode(dataTupleListDataBstArray);
                    offset = offset + dataTupleListDataBstLength;
                    // object
                    const dataTupleListData = {"i16": dataTupleListDataI16, "bst": dataTupleListDataBst};
                    // add
                    dataTupleList.push(dataTupleListData);
                }
                // 
                const dataTupleSingle = [];
                let dataTupleSingleLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataTupleSingleLength >= 0) {
                    // bool
                    const dataTupleSingleData = view.getUint8(offset, false) !== 0;
                    offset = offset + 1;
                    // add
                    dataTupleSingle.push(dataTupleSingleData);
                }
                // object
                const dataTuple = {"binary": dataTupleBinary, "sub": dataTupleSub, "list": dataTupleList, "single": dataTupleSingle};
                // list
                const dataIndexList = [];
                let dataIndexListLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataIndexListLength >= 0) {
                    // 
                    // tuple binary
                    const dataIndexListDataBinary = view.buffer.slice(offset, offset + 6);
                    offset = offset + 6;
                    // tuple tuple
                    // tuple tuple u8
                    const dataIndexListDataSubU8 = view.getUint8(offset, false);
                    offset = offset + 1;
                    // tuple tuple str
                    const dataIndexListDataSubStrLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    const dataIndexListDataSubStrArray = new Uint8Array(view.buffer.slice(offset, offset + dataIndexListDataSubStrLength));
                    const dataIndexListDataSubStr = textDecoder.decode(dataIndexListDataSubStrArray);
                    offset = offset + dataIndexListDataSubStrLength;
                    // object
                    const dataIndexListDataSub = {"u8": dataIndexListDataSubU8, "str": dataIndexListDataSubStr};
                    // tuple list
                    const dataIndexListDataList = [];
                    let dataIndexListDataListLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    while (--dataIndexListDataListLength >= 0) {
                        // 
                        // tuple list i16
                        const dataIndexListDataListDataI16 = view.getInt16(offset, false);
                        offset = offset + 2;
                        // tuple list bst
                        const dataIndexListDataListDataBstLength = view.getUint16(offset, false);
                        offset = offset + 2;
                        const dataIndexListDataListDataBstArray = new Uint8Array(view.buffer.slice(offset, offset + dataIndexListDataListDataBstLength));
                        const dataIndexListDataListDataBst = textDecoder.decode(dataIndexListDataListDataBstArray);
                        offset = offset + dataIndexListDataListDataBstLength;
                        // object
                        const dataIndexListDataListData = {"i16": dataIndexListDataListDataI16, "bst": dataIndexListDataListDataBst};
                        // add
                        dataIndexListDataList.push(dataIndexListDataListData);
                    }
                    // 
                    const dataIndexListDataSingle = [];
                    let dataIndexListDataSingleLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    while (--dataIndexListDataSingleLength >= 0) {
                        // bool
                        const dataIndexListDataSingleData = view.getUint8(offset, false) !== 0;
                        offset = offset + 1;
                        // add
                        dataIndexListDataSingle.push(dataIndexListDataSingleData);
                    }
                    // object
                    const dataIndexListData = {"binary": dataIndexListDataBinary, "sub": dataIndexListDataSub, "list": dataIndexListDataList, "single": dataIndexListDataSingle};
                    // add
                    dataIndexList.push(dataIndexListData);
                }
                // 
                const dataKeyList = {};
                let dataKeyListLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataKeyListLength >= 0) {
                    // 
                    // binary
                    const dataKeyListDataBinary = view.buffer.slice(offset, offset + 6);
                    offset = offset + 6;
                    // boolean
                    const dataKeyListDataBoolean = view.getUint8(offset, false) !== 0;
                    offset = offset + 1;
                    // u8
                    const dataKeyListDataU8 = view.getUint8(offset, false);
                    offset = offset + 1;
                    // u16
                    const dataKeyListDataU16 = view.getUint16(offset, false);
                    offset = offset + 2;
                    // u32
                    const dataKeyListDataU32 = view.getUint32(offset, false);
                    offset = offset + 4;
                    // u64
                    const dataKeyListDataU64 = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // i8
                    const dataKeyListDataI8 = view.getInt8(offset, false);
                    offset = offset + 1;
                    // i16
                    const dataKeyListDataI16 = view.getInt16(offset, false);
                    offset = offset + 2;
                    // i32
                    const dataKeyListDataI32 = view.getInt32(offset, false);
                    offset = offset + 4;
                    // i64
                    const dataKeyListDataI64 = view.getBigInt64(offset, false);
                    offset = offset + 8;
                    // f32
                    const dataKeyListDataF32 = view.getFloat32(offset, false);
                    offset = offset + 4;
                    // f64
                    const dataKeyListDataF64 = view.getFloat64(offset, false);
                    offset = offset + 8;
                    // str
                    const dataKeyListDataStrLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    const dataKeyListDataStrArray = new Uint8Array(view.buffer.slice(offset, offset + dataKeyListDataStrLength));
                    const dataKeyListDataStr = textDecoder.decode(dataKeyListDataStrArray);
                    offset = offset + dataKeyListDataStrLength;
                    // bst
                    const dataKeyListDataBstLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    const dataKeyListDataBstArray = new Uint8Array(view.buffer.slice(offset, offset + dataKeyListDataBstLength));
                    const dataKeyListDataBst = textDecoder.decode(dataKeyListDataBstArray);
                    offset = offset + dataKeyListDataBstLength;
                    // object
                    const dataKeyListData = {"binary": dataKeyListDataBinary, "boolean": dataKeyListDataBoolean, "u8": dataKeyListDataU8, "u16": dataKeyListDataU16, "u32": dataKeyListDataU32, "u64": dataKeyListDataU64, "i8": dataKeyListDataI8, "i16": dataKeyListDataI16, "i32": dataKeyListDataI32, "i64": dataKeyListDataI64, "f32": dataKeyListDataF32, "f64": dataKeyListDataF64, "str": dataKeyListDataStr, "bst": dataKeyListDataBst};
                    // add
                    dataKeyList[dataKeyListDataU8] = dataKeyListData;
                }
                // object
                const data = {"binary": dataBinary, "boolean": dataBoolean, "u8": dataU8, "u16": dataU16, "u32": dataU32, "u64": dataU64, "i8": dataI8, "i16": dataI16, "i32": dataI32, "i64": dataI64, "f32": dataF32, "f64": dataF64, "str": dataStr, "bst": dataBst, "tuple": dataTuple, "indexList": dataIndexList, "keyList": dataKeyList};
                return {"protocol": 65535, "data": data};
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }
}