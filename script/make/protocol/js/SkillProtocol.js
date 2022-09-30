export function encodeSkillProtocol(textEncoder, view, offset, protocol, data) {
    switch (protocol) {
        case 11702: {
            // extend
            while (view.byteLength < offset + 4) {
                const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                view = extendView;
            }
            // 技能ID
            view.setUint32(offset, data["skillId"], false);
            offset = offset + 4;
            return new DataView(view.buffer.slice(0, offset));
        }
        default:throw("unknown protocol define: " + protocol)
    }
}

export function decodeSkillProtocol(textDecoder, view, offset, protocol) {
    switch (protocol) {
        case 11701: {
            // 技能列表
            const list = [];
            let listLength = view.getUint16(offset, false);
            offset = offset + 2;
            while (--listLength >= 0) {
                // 技能ID
                const skillId = view.getUint32(offset, false);
                offset = offset + 4;
                // 技能等级
                const level = view.getUint16(offset, false);
                offset = offset + 2;
                // add
                list.push({skillId, level});
            }
            return {list};
        }
        case 11702: {
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