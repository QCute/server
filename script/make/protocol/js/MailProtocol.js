export class MailQueryRequest {
    /** @type {number} protocol **/
    protocol = 11401;
    /**
     * @type {{
     * }} data
    **/
    data;
}

export class MailQueryResponse {
    /** @type {number} protocol **/
    protocol = 11401;
    /**
     * @type {Array<{
     *     mailId: BigInt;                                                                          // 邮件ID
     *     receiveTime: number;                                                                     // 接收时间
     *     expireTime: number;                                                                      // 有效时间
     *     readTime: number;                                                                        // 读取时间
     *     receiveAttachmentTime: number;                                                           // 领取附件时间
     *     title: string;                                                                           // 标题
     *     content: string;                                                                         // 内容
     *     attachment: Array<{
     *         itemId: number;                                                                      // 物品ID
     *         number: number;                                                                      // 数量
     *     }>;                                                                                      // 附件列表
     * }>} data
    **/
    data;
}

export class MailReadRequest {
    /** @type {number} protocol **/
    protocol = 11402;
    /**
     * @type {BigInt} data
    **/
    data;
}

export class MailReadResponse {
    /** @type {number} protocol **/
    protocol = 11402;
    /**
     * @type {string} data
    **/
    data;
}

export class MailReceiveAttachmentRequest {
    /** @type {number} protocol **/
    protocol = 11403;
    /**
     * @type {BigInt} data
    **/
    data;
}

export class MailReceiveAttachmentResponse {
    /** @type {number} protocol **/
    protocol = 11403;
    /**
     * @type {string} data
    **/
    data;
}

export class MailDeleteRequest {
    /** @type {number} protocol **/
    protocol = 11404;
    /**
     * @type {BigInt} data
    **/
    data;
}

export class MailDeleteResponse {
    /** @type {number} protocol **/
    protocol = 11404;
    /**
     * @type {string} data
    **/
    data;
}

export default class MailProtocol {
    static encode(textEncoder, view, offset, protocol, data) {
        switch (protocol) {
            case 11401: {

                return new DataView(view.buffer.slice(0, offset));
            }
            case 11402: {
                // extend
                while (view.byteLength < offset + 8) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 邮件ID
                view.setBigUint64(offset, data, false);
                offset = offset + 8;
                return new DataView(view.buffer.slice(0, offset));
            }
            case 11403: {
                // extend
                while (view.byteLength < offset + 8) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 邮件ID
                view.setBigUint64(offset, data, false);
                offset = offset + 8;
                return new DataView(view.buffer.slice(0, offset));
            }
            case 11404: {
                // extend
                while (view.byteLength < offset + 8) {
                    const extendView = new DataView(new ArrayBuffer(view.byteLength * 2));
                    (new Uint8Array(extendView.buffer)).set(new Uint8Array(view.buffer));
                    view = extendView;
                }
                // 邮件ID
                view.setBigUint64(offset, data, false);
                offset = offset + 8;
                return new DataView(view.buffer.slice(0, offset));
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }

    static decode(textDecoder, view, offset, protocol) {
        switch (protocol) {
            case 11401: {
                // 
                const data = [];
                let dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                while (--dataLength >= 0) {
                    // 
                    // 邮件ID
                    const dataDataMailId = view.getBigUint64(offset, false);
                    offset = offset + 8;
                    // 接收时间
                    const dataDataReceiveTime = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 有效时间
                    const dataDataExpireTime = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 读取时间
                    const dataDataReadTime = view.getUint32(offset, false);
                    offset = offset + 4;
                    // 领取附件时间
                    const dataDataReceiveAttachmentTime = view.getUint32(offset, false);
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
                    // 附件列表
                    const dataDataAttachment = [];
                    let dataDataAttachmentLength = view.getUint16(offset, false);
                    offset = offset + 2;
                    while (--dataDataAttachmentLength >= 0) {
                        // 
                        // 物品ID
                        const dataDataAttachmentDataItemId = view.getUint32(offset, false);
                        offset = offset + 4;
                        // 数量
                        const dataDataAttachmentDataNumber = view.getUint16(offset, false);
                        offset = offset + 2;
                        // object
                        const dataDataAttachmentData = {"itemId": dataDataAttachmentDataItemId, "number": dataDataAttachmentDataNumber};
                        // add
                        dataDataAttachment.push(dataDataAttachmentData);
                    }
                    // object
                    const dataData = {"mailId": dataDataMailId, "receiveTime": dataDataReceiveTime, "expireTime": dataDataExpireTime, "readTime": dataDataReadTime, "receiveAttachmentTime": dataDataReceiveAttachmentTime, "title": dataDataTitle, "content": dataDataContent, "attachment": dataDataAttachment};
                    // add
                    data.push(dataData);
                }
                return {"protocol": 11401, "data": data};
            }
            case 11402: {
                // 结果
                const dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataArray = new Uint8Array(view.buffer.slice(offset, offset + dataLength));
                const data = textDecoder.decode(dataArray);
                offset = offset + dataLength;
                return {"protocol": 11402, "data": data};
            }
            case 11403: {
                // 结果
                const dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataArray = new Uint8Array(view.buffer.slice(offset, offset + dataLength));
                const data = textDecoder.decode(dataArray);
                offset = offset + dataLength;
                return {"protocol": 11403, "data": data};
            }
            case 11404: {
                // 结果
                const dataLength = view.getUint16(offset, false);
                offset = offset + 2;
                const dataArray = new Uint8Array(view.buffer.slice(offset, offset + dataLength));
                const data = textDecoder.decode(dataArray);
                offset = offset + dataLength;
                return {"protocol": 11404, "data": data};
            }
            default: throw("unknown protocol define: " + protocol)
        }
    }
}