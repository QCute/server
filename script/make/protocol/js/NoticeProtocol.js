export default class NoticeProtocol {
    static encode(textEncoder, view, offset, protocol, data) {
        switch (protocol) {
            case 50001: {

                return new DataView(view.buffer.slice(0, offset));
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }

    static decode(textDecoder, view, offset, protocol) {
        switch (protocol) {
            case 50001: {
                // 公告列表
                const data = [];
                let dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataLength >= 0) {
                    // 
                    // 公告ID
                    const dataDataNoticeId = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 收到时间
                    const dataDataReceiveTime = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 读取时间
                    const dataDataReadTime = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 标题
                    const dataDataTitleLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    const dataDataTitleArray = new Uint8Array(view.buffer.slice(offset, offset + dataDataTitleLength));
                    const dataDataTitle = textDecoder.decode(dataDataTitleArray);
                    offset = offset + dataDataTitleLength;
                    // 内容
                    const dataDataContentLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    const dataDataContentArray = new Uint8Array(view.buffer.slice(offset, offset + dataDataContentLength));
                    const dataDataContent = textDecoder.decode(dataDataContentArray);
                    offset = offset + dataDataContentLength;
                    // object
                    const dataData = {"noticeId": dataDataNoticeId, "receiveTime": dataDataReceiveTime, "readTime": dataDataReadTime, "title": dataDataTitle, "content": dataDataContent};
                    // add
                    data.push(dataData);
                }
                return data;
            }
            case 50002: {
                // 
                // 范围
                const dataScope = view.getUint8(offset, false);
                offset = offset + 1;
                // 类型
                const dataType = view.getUint8(offset, false);
                offset = offset + 1;
                // 标题
                const dataTitleLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataTitleArray = new Uint8Array(view.buffer.slice(offset, offset + dataTitleLength));
                const dataTitle = textDecoder.decode(dataTitleArray);
                offset = offset + dataTitleLength;
                // 消息
                const dataMsgLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataMsgArray = new Uint8Array(view.buffer.slice(offset, offset + dataMsgLength));
                const dataMsg = textDecoder.decode(dataMsgArray);
                offset = offset + dataMsgLength;
                // object
                const data = {"scope": dataScope, "type": dataType, "title": dataTitle, "msg": dataMsg};
                return data;
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }
}