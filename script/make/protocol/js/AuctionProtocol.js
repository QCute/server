export class AuctionQueryRequest {
    /** @type {number} protocol **/
    protocol = 16101;
    /**
     * @type {{
     * }} data
    **/
    data;
}

export class AuctionQueryResponse {
    /** @type {number} protocol **/
    protocol = 16101;
    /**
     * @type {Array<{
     *     auctionNo: BigInt;                                                                       // 拍品编号
     *     auctionId: number;                                                                       // 拍品ID
     *     number: number;                                                                          // 数量
     *     type: number;                                                                            // 拍卖类型(1:全服/2:公会)
     *     endTime: number;                                                                         // 结束时间
     *     nowPrice: number;                                                                        // 当前价格
     *     nextPrice: number;                                                                       // 下次出价的价格
     * }>} data
    **/
    data;
}

export class AuctionBidRequest {
    /** @type {number} protocol **/
    protocol = 16102;
    /**
     * @type {{
     *     auctionNo: BigInt;                                                                       // 拍品编号
     *     nextPrice: number;                                                                       // 新的价格
     * }} data
    **/
    data;
}

export class AuctionBidResponse {
    /** @type {number} protocol **/
    protocol = 16102;
    /**
     * @type {{
     *     result: string;                                                                          // 结果
     *     newPrice: number;                                                                        // 新的价格
     *     auction: {
     *         auctionNo: BigInt;                                                                   // 拍品编号
     *         auctionId: number;                                                                   // 拍品ID
     *         type: number;                                                                        // 拍卖类型(1:全服/2:公会)
     *         endTime: number;                                                                     // 结束时间
     *         nowPrice: number;                                                                    // 当前价格
     *         nextPrice: number;                                                                   // 下次出价的价格
     *     };                                                                                       // 拍品
     * }} data
    **/
    data;
}

export default class AuctionProtocol {
    static encode(textEncoder, view, offset, protocol, data) {
        switch (protocol) {
            case 16101: {

                return new DataView(view.buffer.slice(0, offset));
            }
            case 16102: {

                // extend
                while (view.byteLength < offset + 8) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 拍品编号
                view.setBigUint64(offset, data.auctionNo, false);
                offset = offset + 8;
                // extend
                while (view.byteLength < offset + 4) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 新的价格
                view.setUint32(offset, data.nextPrice, false);
                offset = offset + 4;
                return new DataView(view.buffer.slice(0, offset));
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }

    static decode(textDecoder, view, offset, protocol) {
        switch (protocol) {
            case 16101: {
                // 拍品列表
                const data = [];
                let dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataLength >= 0) {
                    // 拍品
                    // 拍品编号
                    const dataDataAuctionNo = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 拍品ID
                    const dataDataAuctionId = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 数量
                    const dataDataNumber = view.getUint16(offset, false);
                    offset = offset + 2;
                    // 拍卖类型(1:全服/2:公会)
                    const dataDataType = view.getUint8(offset, false);
                    offset = offset + 1;
                    // 结束时间
                    const dataDataEndTime = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 当前价格
                    const dataDataNowPrice = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 下次出价的价格
                    const dataDataNextPrice = view.getUint32(offset, false);
                    offset = offset + 4;
                    // object
                    const dataData = {"auctionNo": dataDataAuctionNo, "auctionId": dataDataAuctionId, "number": dataDataNumber, "type": dataDataType, "endTime": dataDataEndTime, "nowPrice": dataDataNowPrice, "nextPrice": dataDataNextPrice};
                    // add
                    data.push(dataData);
                }
                return {"protocol": 16101, "data": data};
            }
            case 16102: {
                // 
                // 结果
                const dataResultLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataResultArray = new Uint8Array(view.buffer.slice(offset, offset + dataResultLength));
                const dataResult = textDecoder.decode(dataResultArray);
                offset = offset + dataResultLength;
                // 新的价格
                const dataNewPrice = view.getUint32(offset, false);
                offset = offset + 4;
                // 拍品
                // 拍品编号
                const dataAuctionAuctionNo = view.getBigUint64(offset, false);
                offset = offset + 8;
                // 拍品ID
                const dataAuctionAuctionId = view.getUint32(offset, false);
                offset = offset + 4;
                // 拍卖类型(1:全服/2:公会)
                const dataAuctionType = view.getUint8(offset, false);
                offset = offset + 1;
                // 结束时间
                const dataAuctionEndTime = view.getUint32(offset, false);
                offset = offset + 4;
                // 当前价格
                const dataAuctionNowPrice = view.getUint32(offset, false);
                offset = offset + 4;
                // 下次出价的价格
                const dataAuctionNextPrice = view.getUint32(offset, false);
                offset = offset + 4;
                // object
                const dataAuction = {"auctionNo": dataAuctionAuctionNo, "auctionId": dataAuctionAuctionId, "type": dataAuctionType, "endTime": dataAuctionEndTime, "nowPrice": dataAuctionNowPrice, "nextPrice": dataAuctionNextPrice};
                // object
                const data = {"result": dataResult, "newPrice": dataNewPrice, "auction": dataAuction};
                return {"protocol": 16102, "data": data};
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }
}