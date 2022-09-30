import Reader from "../Reader.js";
import Writer from "../Writer.js";

const data = {
    protocol: 65535,
    content: {
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

        "indexList": [
            {
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

function testReaderWriter() {
    console.log(data);
    BigInt.prototype.toJSON = function() { return this.toString(); }
    let writer = new Writer();
    let buffer = writer.write(data.protocol, data.content);
    let array = new Uint8Array(buffer.byteLength * 2);
    array.set(new Uint8Array(buffer));
    array.set(new Uint8Array(buffer), buffer.byteLength);
    let reader = new Reader();
    for (let i = 0; i < buffer.byteLength * 2; i += 10) {
        let result = reader.read(array.buffer.slice(i, i + 10 || undefined));
        if(result) {
            console.log(result);
            console.assert(JSON.stringify(data) == JSON.stringify(result));
        }
    }
}

testReaderWriter()
