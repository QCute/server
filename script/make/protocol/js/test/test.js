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
            "binary": new Uint8Array([97, 98, 99, 100, 101, 102]).buffer,
            "sub": {
                "u8": 95,
                "str": "xyz",
            },
            "list": [
                {"i16": 456, "bst": "wow"},
                {"i16": 369, "bst": "oops"},
            ],
            "single": [true, false, false, true, false],
        },

        "indexList": [
            {
                "binary": new Uint8Array([97, 98, 99, 100, 101, 102]).buffer,
                "sub": {
                    "u8": 108,
                    "str": "qwe",
                },
                "list": [
                    {"i16": 456, "bst": "wow"},
                    {"i16": 369, "bst": "oops"},
                ],
                "single": [true, false, false, true, false],
            }
        ],

        "keyList": {
            1: {
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
