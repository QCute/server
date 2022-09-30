export function encodeDailyProtocol(textEncoder, view, offset, protocol, data) {
    switch (protocol) {
        case 12303: {
            // extend
            while (view.byteLength < offset + 4) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            // 日常ID
            view.setUint32(offset, data["dailyId"], false);
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
            view.setUint32(offset, data["stageId"], false);
            offset = offset + 4;
            return new DataView(view.buffer.slice(0, offset));
        }
        default:throw("unknown protocol define: " + protocol)
    }
}

export function decodeDailyProtocol(textDecoder, view, offset, protocol) {
    switch (protocol) {
        case 12301: {
            // 统计列表
            const list = [];
            let listLength = view.getUint16(offset, false);
            offset = offset + 2;
            while (--listLength >= 0) {
                // 统计类型
                const type = view.getUint32(offset, false);
                offset = offset + 4;
                // 今日数量
                const todayNumber = view.getUint32(offset, false);
                offset = offset + 4;
                // add
                list.push({type, todayNumber});
            }
            return {list};
        }
        case 12302: {
            // 日常列表
            const list = [];
            let listLength = view.getUint16(offset, false);
            offset = offset + 2;
            while (--listLength >= 0) {
                // 日常ID
                const dailyId = view.getUint32(offset, false);
                offset = offset + 4;
                // 是否领取奖励
                const isAward = view.getUint8(offset, false);
                offset = offset + 1;
                // add
                list.push({dailyId, isAward});
            }
            // 奖励阶段ID
            const stageId = view.getUint32(offset, false);
            offset = offset + 4;
            // 活跃度
            const score = view.getUint32(offset, false);
            offset = offset + 4;
            return {list, stageId, score};
        }
        case 12303: {
            // 结果
            const resultLength = view.getUint16(offset, false);
            offset = offset + 2;
            const resultArray = new Uint8Array(view.buffer.slice(offset, offset + resultLength));
            const result = textDecoder.decode(resultArray);
            offset = offset + resultLength;
            return {result};
        }
        case 12304: {
            // 结果
            const resultLength = view.getUint16(offset, false);
            offset = offset + 2;
            const resultArray = new Uint8Array(view.buffer.slice(offset, offset + resultLength));
            const result = textDecoder.decode(resultArray);
            offset = offset + resultLength;
            return {result};
        }
        default:throw("unknown protocol define: " + protocol)
    }
}