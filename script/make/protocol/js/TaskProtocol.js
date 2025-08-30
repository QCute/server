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
                    const dataDataTaskId = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 当前数量
                    const dataDataNumber = view.getUint16(offset, false);
                    offset = offset + 2;
                    // 是否领取奖励
                    const dataDataIsAward = view.getUint8(offset, false);
                    offset = offset + 1;
                    // object
                    const dataData = {"taskId": dataDataTaskId, "number": dataDataNumber, "isAward": dataDataIsAward};
                    // add
                    data.push(dataData);
                }
                return data;
            }
            case 11202: {
                // 
                // 结果
                const dataResultLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataResultArray = new Uint8Array(view.buffer.slice(offset, offset + dataResultLength));
                const dataResult = textDecoder.decode(dataResultArray);
                offset = offset + dataResultLength;
                // 
                // 任务ID
                const dataTaskTaskId = view.getUint32(offset, false);
                offset = offset + 4;
                // 当前数量
                const dataTaskNumber = view.getUint16(offset, false);
                offset = offset + 2;
                // 是否领取奖励
                const dataTaskIsAward = view.getUint8(offset, false);
                offset = offset + 1;
                // object
                const dataTask = {"taskId": dataTaskTaskId, "number": dataTaskNumber, "isAward": dataTaskIsAward};
                // object
                const data = {"result": dataResult, "task": dataTask};
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