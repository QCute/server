-- get current source
local source = debug.getinfo(1).source
local path = string.sub(source, 2, string.len(source) - (string.find(string.reverse(source), "/") or 0))
-- add package path
-- current path
package.path = package.path .. ";" .. "./" .. path .. "/?.lua"
-- parent path
package.path = package.path .. ";" .. "./" .. path .. "/../?.lua";
-- protocol reader/writer
require("../Reader")
require("../Writer")
-- protocol and data
local packet = {
    protocol = 65535,
    data = {
        binary = "\97\98\99\100\101\102",
        boolean = true,

        u8 = 1,
        u16 = 2,
        u32 = 3,
        u64 = 4,

        i8 = 4,
        i16 = 3,
        i32 = 2,
        i64 = 1,

        f32 = 1.23,
        f64 = 4.56,

        str = "一23",
        bst = "1二三",

        tuple = {
            binary = "\97\98\99\100\101\102",
            sub = {
                u8 = 95,
                str = "xyz",
            },
            list = {
                {i16 = 456, bst = "wow"},
                {i16 = 369, bst = "oops"},
            },
            single = {true, false, false, true, false},
        },

        indexList = {
            {
                binary = "\97\98\99\100\101\102",
                sub = {
                    u8 = 108,
                    str = "qwe",
                },
                list = {
                    {i16 = 456, bst = "wow"},
                    {i16 = 369, bst = "oops"},
                },
                single = {true, false, false, true, false},
            }
        },

        keyList = {
            [1] = {
                binary = "\97\98\99\100\101\102",
                boolean = true,
        
                u8 = 1,
                u16 = 2,
                u32 = 3,
                u64 = 4,
        
                i8 = 4,
                i16 = 3,
                i32 = 2,
                i64 = 1,
        
                f32 = 1.23,
                f64 = 4.56,
        
                str = "一23",
                bst = "1二三",
            }
        }
    }
}

-- byte dump function
function dump(data)
    local string = '';
    for i = 1, data:len() do
        string = string .. string.byte(data, i, i) .. ', '
    end
    return '{ ' .. string .. ' }'
end

-- table stringify function
function stringify(data)
    if type(data) == 'boolean' then
        return tostring(data)
    elseif type(data) == 'number' then
        return data
    elseif type(data) == 'string' then
        return '"' .. data .. '"'
    elseif type(data) == 'table' then
        local string = ''
        for key, value in pairs(data) do
            string = string .. key .. ' = ' .. stringify(value) .. ', '
        end
        return '{ ' .. string .. ' }'
    end
end

function testReaderWriter()
    print(stringify(packet))
    print()
    local buffer = Writer:write(packet.protocol, packet.data)
    print(dump(buffer))
    print()
    local len = string.len(buffer)
    local array = table.concat({buffer, buffer})
    local len = string.len(array)
    for i = 0, len do
        local slice = string.sub(array, i * 10 + 1, (i + 1) * 10 or nil);
        local result = Reader:appendData(slice):read()
        if result then
            print(stringify(result))
            print(string.format("assert: %s", stringify(result) == stringify(packet)))
            print()
        end
    end
end

testReaderWriter()

-- test function
function testNetworkReaderWriter()
    local socket = require("socket")
    local host = "127.0.0.1"
    local sock = assert(socket.connect(host, 33333))
    local buffer = Writer:write(packet.protocol, packet.data)
    print(dump(buffer))
    sock:send(buffer)
    repeat
        local chunk, status, partial = sock:receive(1024)
        Reader:appendData(chunk or partial)
        while(true) do
            local result = Reader:read()
            if result == nil then
                break
            end
            print(stringify(result))
        end
    until status ~= "closed"
    sock:close()  -- 关闭 TCP 连接
end

testNetworkReaderWriter()
