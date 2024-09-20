import Encoder from "../Encoder.js";
import Decoder from "../Decoder.js";

const packet = {
    protocol: 65535,
    data: {
        "binary": new Uint8Array([97, 98, 99, 100, 101, 102]).buffer,
        "boolean": true,

        "u8": 1,
        "u16": 2,
        "u32": 3,
        "u64": BigInt(4),

        "i8": 4,
        "i16": 3,
        "i32": 2,
        "i64": BigInt(1),

        "f32": 1.23,
        "f64": 4.56,

        "str": "一23",
        "bst": "1二三",

        "tuple": {
            "tupleBinary": new Uint8Array([97, 98, 99, 100, 101, 102]).buffer,
            "tupleSubTuple": {
                "tupleSubTupleU8": 95,
                "tupleSubTupleStr": "xyz",
            },
            "tupleSubList": [
                {"tupleSubListI16": 456, "tupleSubListBst": "wow"},
                {"tupleSubListI16": 369, "tupleSubListBst": "oops"},
            ],
            "tupleSubListSingle": [true, false, false, true, false],
        },

        "indexList": [
            {
                "listBinary": new Uint8Array([97, 98, 99, 100, 101, 102]).buffer,
                "listSubTuple": {
                    "listSubTupleU8": 108,
                    "listSubTupleStr": "qwe",
                },
                "listSubList": [
                    {"listSubListI16": 456, "listSubListBst": "wow"},
                    {"listSubListI16": 369, "listSubListBst": "oops"},
                ],
                "listSubListSingle": [true, false, false, true, false],
            }
        ],

        "keyList": {
            1: {
                "listBinary": new Uint8Array([97, 98, 99, 100, 101, 102]).buffer,
                "listBoolean": false,
            
                "listU8": 1,
                "listU16": 2,
                "listU32": 3,
                "listU64": BigInt(4),
            
                "listI8": 4,
                "listI16": 3,
                "listI32": 2,
                "listI64": BigInt(1),
            
                "listF32": 1.23,
                "listF64": 4.56,
            
                "listStr": "一23",
                "listBst": "1二三",
            }
        }
    }
};

function dump(buffer) {
    buffer = new Uint8Array(buffer);
    let string = '';
    for(let i = 0; i < buffer.length; i++) {
        string = string + buffer[i] + ', '
    }
    return '[' + string + ']';
}

function testEncoderDecoder() {
    console.log(packet);
    BigInt.prototype.toJSON = function() { return this.toString(); }
    let encoder = new Encoder();
    let buffer = encoder.encode(packet.protocol, packet.data);
    console.log(dump(buffer));
    let array = new Uint8Array(buffer.byteLength * 2);
    array.set(new Uint8Array(buffer));
    array.set(new Uint8Array(buffer), buffer.byteLength);
    let decoder = new Decoder();
    for (let i = 0; i < array.buffer.byteLength; i += 10) {
        let result = decoder.appendData(array.buffer.slice(i, i + 10 || undefined)).decode();
        if(result) {
            console.log(result);
            console.assert(JSON.stringify(packet) == JSON.stringify(result));
        }
    }
}

testEncoderDecoder()

function testNetworkEncoderDecoder(url) {
    const socket = new WebSocket(url || 'ws://127.0.0.1:33333');
    const encoder = new Encoder();
    socket.onopen = () => {
        let buffer = encoder.encode(packet.protocol, packet.data);
        socket.send(buffer);
    }
    const decoder = new Decoder();
    socket.onmessage = (event) => {
        decoder.appendData(event.data);
        do {
            let packet = decoder.decode();
            if (!packet) break;
            console.log(packet);
        } while(true)
    }
}

testNetworkEncoderDecoder('ws://127.0.0.1:33333')
