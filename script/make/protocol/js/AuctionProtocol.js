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
                view.setBigUint64(offset, data["auctionNo"], false);
                offset = offset + 8;
                // extend
                while (view.byteLength < offset + 4) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 新的价格
                view.setUint32(offset, data["nextPrice"], false);
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
                    const auctionNo = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 拍品ID
                    const auctionId = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 数量
                    const number = view.getUint16(offset, false);
                    offset = offset + 2;
                    // 拍卖类型(1:全服/2:公会)
                    const type = view.getUint8(offset, false);
                    offset = offset + 1;
                    // 结束时间
                    const endTime = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 当前价格
                    const nowPrice = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 下次出价的价格
                    const nextPrice = view.getUint32(offset, false);
                    offset = offset + 4;
                    // object
                    const auction = {"auctionNo": auctionNo, "auctionId": auctionId, "number": number, "type": type, "endTime": endTime, "nowPrice": nowPrice, "nextPrice": nextPrice};
                    // add
                    data.push(auction);
                }
                return data;
            }
            case 16102: {
                // 
                // 结果
                const resultLength = view.getUint16(offset, false);
                offset = offset + 2;
                const resultArray = new Uint8Array(view.buffer.slice(offset, offset + resultLength));
                const result = textDecoder.decode(resultArray);
                offset = offset + resultLength;
                // 新的价格
                const newPrice = view.getUint32(offset, false);
                offset = offset + 4;
                // 拍品
                // 拍品编号
                const auctionAuctionNo = view.getBigUint64(offset, false);
                offset = offset + 8;
                // 拍品ID
                const auctionAuctionId = view.getUint32(offset, false);
                offset = offset + 4;
                // 拍卖类型(1:全服/2:公会)
                const auctionType = view.getUint8(offset, false);
                offset = offset + 1;
                // 结束时间
                const auctionEndTime = view.getUint32(offset, false);
                offset = offset + 4;
                // 当前价格
                const auctionNowPrice = view.getUint32(offset, false);
                offset = offset + 4;
                // 下次出价的价格
                const auctionNextPrice = view.getUint32(offset, false);
                offset = offset + 4;
                // object
                const auction = {"auctionNo": auctionAuctionNo, "auctionId": auctionAuctionId, "type": auctionType, "endTime": auctionEndTime, "nowPrice": auctionNowPrice, "nextPrice": auctionNextPrice};
                // object
                const data = {"result": result, "newPrice": newPrice, "auction": auction};
                return data;
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }
}