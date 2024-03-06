export default class TaskProtocol {
    static encode(textEncoder, view, offset, protocol, data) {
        switch (protocol) {
            case 11201: {
                return new DataView(view.buffer.slice(0, offset));
            }
            case 11202: {
                // extend
                while (view.byteLength < offset + 4) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 任务ID
                view.setUint32(offset, data, false);
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
                view.setUint32(offset, data, false);
                offset = offset + 4;
                return new DataView(view.buffer.slice(0, offset));
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }

    static decode(textDecoder, view, offset, protocol) {
        switch (protocol) {
            case 11201: {
                // 任务列表
                const data = [];
                let dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataLength >= 0) {
                    // 
                    // 任务ID
                    const taskId = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 当前数量
                    const number = view.getUint16(offset, false);
                    offset = offset + 2;
                    // 是否领取奖励
                    const isAward = view.getUint8(offset, false);
                    offset = offset + 1;
                    // object
                    const task = {"taskId": taskId, "number": number, "isAward": isAward};
                    // add
                    data.push(task);
                }
                return data;
            }
            case 11202: {
                // 
                // 结果
                const resultLength = view.getUint16(offset, false);
                offset = offset + 2;
                const resultArray = new Uint8Array(view.buffer.slice(offset, offset + resultLength));
                const result = textDecoder.decode(resultArray);
                offset = offset + resultLength;
                // 
                // 任务ID
                const taskTaskId = view.getUint32(offset, false);
                offset = offset + 4;
                // 当前数量
                const taskNumber = view.getUint16(offset, false);
                offset = offset + 2;
                // 是否领取奖励
                const taskIsAward = view.getUint8(offset, false);
                offset = offset + 1;
                // object
                const task = {"taskId": taskTaskId, "number": taskNumber, "isAward": taskIsAward};
                // object
                const data = {"result": result, "task": task};
                return data;
            }
            case 11203: {
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