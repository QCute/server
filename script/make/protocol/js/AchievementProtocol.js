export default class AchievementProtocol {
    static encode(textEncoder, view, offset, protocol, data) {
        switch (protocol) {
            case 12201: {

                return new DataView(view.buffer.slice(0, offset));
            }
            case 12202: {

                return new DataView(view.buffer.slice(0, offset));
            }
            case 12203: {
                // extend
                while (view.byteLength < offset + 4) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 成就ID
                view.setUint32(offset, data, false);
                offset = offset + 4;
                return new DataView(view.buffer.slice(0, offset));
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }

    static decode(textDecoder, view, offset, protocol) {
        switch (protocol) {
            case 12201: {
                // 统计列表
                const data = [];
                let dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataLength >= 0) {
                    // 
                    // 统计类型
                    const dataDataType = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 总数
                    const dataDataTotalNumber = view.getUint32(offset, false);
                    offset = offset + 4;
                    // object
                    const dataData = {"type": dataDataType, "totalNumber": dataDataTotalNumber};
                    // add
                    data.push(dataData);
                }
                return data;
            }
            case 12202: {
                // 成就列表
                const data = [];
                let dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataLength >= 0) {
                    // 
                    // 成就ID
                    const dataDataAchievementId = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 成就类型
                    const dataDataType = view.getUint32(offset, false);
                    offset = offset + 4;
                    // object
                    const dataData = {"achievementId": dataDataAchievementId, "type": dataDataType};
                    // add
                    data.push(dataData);
                }
                return data;
            }
            case 12203: {
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