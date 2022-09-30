export function encodeTaskProtocol(textEncoder, view, offset, protocol, data) {
    switch (protocol) {
        case 11202: {
            // extend
            while (view.byteLength < offset + 4) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            // 任务ID
            view.setUint32(offset, data["taskId"], false);
            offset = offset + 4;
            return new DataView(view.buffer.slice(0, offset));
        }
        case 11203: {
            // extend
            while (view.byteLength < offset + 4) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            // 任务ID
            view.setUint32(offset, data["taskId"], false);
            offset = offset + 4;
            return new DataView(view.buffer.slice(0, offset));
        }
        default:throw("unknown protocol define: " + protocol)
    }
}

export function decodeTaskProtocol(textDecoder, view, offset, protocol) {
    switch (protocol) {
        case 11201: {
            // 任务列表
            const list = [];
            let listLength = view.getUint16(offset, false);
            offset = offset + 2;
            while (--listLength >= 0) {
                // 任务ID
                const taskId = view.getUint32(offset, false);
                offset = offset + 4;
                // 当前数量
                const number = view.getUint16(offset, false);
                offset = offset + 2;
                // 是否领取奖励
                const isAward = view.getUint8(offset, false);
                offset = offset + 1;
                // add
                list.push({taskId, number, isAward});
            }
            return {list};
        }
        case 11202: {
            // 结果
            const resultLength = view.getUint16(offset, false);
            offset = offset + 2;
            const resultArray = new Uint8Array(view.buffer.slice(offset, offset + resultLength));
            const result = textDecoder.decode(resultArray);
            offset = offset + resultLength;
            // 任务ID
            const taskId = view.getUint32(offset, false);
            offset = offset + 4;
            // 当前数量
            const number = view.getUint16(offset, false);
            offset = offset + 2;
            // 是否领取奖励
            const isAward = view.getUint8(offset, false);
            offset = offset + 1;
            return {result, taskId, number, isAward};
        }
        case 11203: {
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