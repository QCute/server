export function encodeAchievementProtocol(textEncoder, view, offset, protocol, data) {
    switch (protocol) {
        case 12203: {
            // extend
            while (view.byteLength < offset + 4) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            // 成就ID
            view.setUint32(offset, data["achievementId"], false);
            offset = offset + 4;
            return new DataView(view.buffer.slice(0, offset));
        }
        default:throw("unknown protocol define: " + protocol)
    }
}

export function decodeAchievementProtocol(textDecoder, view, offset, protocol) {
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
                // 总数
                const totalNumber = view.getUint32(offset, false);
                offset = offset + 4;
                // add
                list.push({type, totalNumber});
            }
            return {list};
        }
        case 12202: {
            // 成就列表
            const list = [];
            let listLength = view.getUint16(offset, false);
            offset = offset + 2;
            while (--listLength >= 0) {
                // 成就ID
                const achievementId = view.getUint32(offset, false);
                offset = offset + 4;
                // 成就类型
                const type = view.getUint32(offset, false);
                offset = offset + 4;
                // add
                list.push({achievementId, type});
            }
            return {list};
        }
        case 12203: {
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