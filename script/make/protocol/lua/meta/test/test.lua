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
local data = {
    protocol = 65535,
    content = {
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

        indexList = {
            {
                listBinary = "\97\98\99\100\101\102",
                listBoolean = false,
            
                listU8 = 1,
                listU16 = 2,
                listU32 = 3,
                listU64 = 4,
            
                listI8 = 4,
                listI16 = 3,
                listI32 = 2,
                listI64 = 1,
            
                listF32 = 1.23,
                listF64 = 4.56,
            
                listStr = "一23",
                listBst = "1二三",
            }
        },

        keyList = {
            [1] = {
                listBinary = "\97\98\99\100\101\102",
                listBoolean = false,
            
                listU8 = 1,
                listU16 = 2,
                listU32 = 3,
                listU64 = 4,
            
                listI8 = 4,
                listI16 = 3,
                listI32 = 2,
                listI64 = 1,
            
                listF32 = 1.23,
                listF64 = 4.56,
            
                listStr = "一23",
                listBst = "1二三",
            }
        }
    }
}

-- table stringify function
function stringify(data)
    local string = ''
    for key, value in pairs(data) do
        if type(value) == 'number' then
            string = string .. key .. ' = ' .. value .. ', '
        elseif type(value) == 'string' then
            string = string .. key .. ' = "' .. value .. '", '
        elseif type(value) == 'table' then
            string = string .. key .. ' = ' .. stringify(value) .. ', '
        end
    end
    return '{ ' .. string .. ' }'
end

function testReaderWriter()
    print(stringify(data))
    print()
    local buffer = Writer:write(data.protocol, data.content)
    local len = string.len(buffer)
    local db = table.concat({buffer, buffer})
    for i = 0, len * 2 do
        local s = string.sub(db, i * 10 + 1, (i + 1) * 10 or nil);
        local result = Reader:read(s)
        if result then
            print(stringify(result))
            assert(stringify(result) == stringify(data))
        end
    end
end

testReaderWriter()
