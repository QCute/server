export default class DailyProtocol {
    static encode(textEncoder, view, offset, protocol, data) {
        switch (protocol) {
            case 12301: {
                return new DataView(view.buffer.slice(0, offset));
            }
            case 12302: {
                return new DataView(view.buffer.slice(0, offset));
            }
            case 12303: {
                // extend
                while (view.byteLength < offset + 4) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 日常ID
                view.setUint32(offset, data, false);
                offset = offset + 4;
                return new DataView(view.buffer.slice(0, offset));
            }
            case 12304: {
                // extend
                while (view.byteLength < offset + 4) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 阶段ID
                view.setUint32(offset, data, false);
                offset = offset + 4;
                return new DataView(view.buffer.slice(0, offset));
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }

    static decode(textDecoder, view, offset, protocol) {
        switch (protocol) {
            case 12301: {
                // 
                const data = [];
                let dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataLength >= 0) {
                    // 
                    // 统计类型
                    const type = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 今日数量
                    const todayNumber = view.getUint32(offset, false);
                    offset = offset + 4;
                    // object
                    const count = {"type": type, "todayNumber": todayNumber};
                    // add
                    data.push(count);
                }
                return data;
            }
            case 12302: {
                // 
                // 
                // 日常ID
                const dailyDailyId = view.getUint32(offset, false);
                offset = offset + 4;
                // 是否领取奖励
                const dailyIsAward = view.getUint8(offset, false);
                offset = offset + 1;
                // object
                const daily = {"dailyId": dailyDailyId, "isAward": dailyIsAward};
                // 
                // 奖励阶段ID
                const dailyActiveStageId = view.getUint32(offset, false);
                offset = offset + 4;
                // 活跃度
                const dailyActiveScore = view.getUint32(offset, false);
                offset = offset + 4;
                // object
                const dailyActive = {"stageId": dailyActiveStageId, "score": dailyActiveScore};
                // object
                const data = {"daily": daily, "dailyActive": dailyActive};
                return data;
            }
            case 12303: {
                // 结果
                const dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataArray = new Uint8Array(view.buffer.slice(offset, offset + dataLength));
                const data = textDecoder.decode(dataArray);
                offset = offset + dataLength;
                return data;
            }
            case 12304: {
                // 结果
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