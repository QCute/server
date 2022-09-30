export function encodeNoticeProtocol(textEncoder, view, offset, protocol, data) {
    switch (protocol) {

        default:throw("unknown protocol define: " + protocol)
    }
}

export function decodeNoticeProtocol(textDecoder, view, offset, protocol) {
    switch (protocol) {
        case 50001: {
            // 公告列表
            const noticeList = [];
            let noticeListLength = view.getUint16(offset, false);
            offset = offset + 2;
            while (--noticeListLength >= 0) {
                // 公告ID
                const noticeId = view.getBigUint64(offset, false);
                offset = offset + 8;
                // 收到时间
                const receiveTime = view.getUint32(offset, false);
                offset = offset + 4;
                // 读取时间
                const readTime = view.getUint32(offset, false);
                offset = offset + 4;
                // 标题
                const titleLength = view.getUint16(offset, false);
                offset = offset + 2;
                const titleArray = new Uint8Array(view.buffer.slice(offset, offset + titleLength));
                const title = textDecoder.decode(titleArray);
                offset = offset + titleLength;
                // 内容
                const contentLength = view.getUint16(offset, false);
                offset = offset + 2;
                const contentArray = new Uint8Array(view.buffer.slice(offset, offset + contentLength));
                const content = textDecoder.decode(contentArray);
                offset = offset + contentLength;
                // add
                noticeList.push({noticeId, receiveTime, readTime, title, content});
            }
            return {noticeList};
        }
        case 50002: {
            // 范围
            const scope = view.getUint8(offset, false);
            offset = offset + 1;
            // 类型
            const type = view.getUint8(offset, false);
            offset = offset + 1;
            // 标题
            const titleLength = view.getUint16(offset, false);
            offset = offset + 2;
            const titleArray = new Uint8Array(view.buffer.slice(offset, offset + titleLength));
            const title = textDecoder.decode(titleArray);
            offset = offset + titleLength;
            // 消息
            const msgLength = view.getUint16(offset, false);
            offset = offset + 2;
            const msgArray = new Uint8Array(view.buffer.slice(offset, offset + msgLength));
            const msg = textDecoder.decode(msgArray);
            offset = offset + msgLength;
            return {scope, type, title, msg};
        }
        default:throw("unknown protocol define: " + protocol)
    }
}