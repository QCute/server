export default class SkillProtocol {
    static encode(textEncoder, view, offset, protocol, data) {
        switch (protocol) {
            case 11701: {

                return new DataView(view.buffer.slice(0, offset));
            }
            case 11702: {
                // extend
                while (view.byteLength < offset + 4) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 技能ID
                view.setUint32(offset, data, false);
                offset = offset + 4;
                return new DataView(view.buffer.slice(0, offset));
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }

    static decode(textDecoder, view, offset, protocol) {
        switch (protocol) {
            case 11701: {
                // 技能列表
                const data = [];
                let dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataLength >= 0) {
                    // 
                    // 技能ID
                    const dataDataSkillId = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 技能等级
                    const dataDataLevel = view.getUint16(offset, false);
                    offset = offset + 2;
                    // object
                    const dataData = {"skillId": dataDataSkillId, "level": dataDataLevel};
                    // add
                    data.push(dataData);
                }
                return data;
            }
            case 11702: {
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