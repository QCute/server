function encodeTestProtocol(offset, protocol, data)
    local switch = {
        [65535] = function()
            local offset = offset
            local table = {}
            -- binary
            table[offset] = data["binary"]
            offset = offset + 1
            -- boolean
            table[offset] = string.pack(">I1", data["boolean"] ~= 0 and 1 or 0)
            offset = offset + 1
            -- u8
            table[offset] = string.pack(">I1", data["u8"])
            offset = offset + 1
            -- u16
            table[offset] = string.pack(">I2", data["u16"])
            offset = offset + 1
            -- u32
            table[offset] = string.pack(">I4", data["u32"])
            offset = offset + 1
            -- u64
            table[offset] = string.pack(">I8", data["u64"])
            offset = offset + 1
            -- i8
            table[offset] = string.pack(">i1", data["i8"])
            offset = offset + 1
            -- i16
            table[offset] = string.pack(">i2", data["i16"])
            offset = offset + 1
            -- i32
            table[offset] = string.pack(">i4", data["i32"])
            offset = offset + 1
            -- i64
            table[offset] = string.pack(">i8", data["i64"])
            offset = offset + 1
            -- f32
            table[offset] = string.pack(">f", data["f32"])
            offset = offset + 1
            -- f64
            table[offset] = string.pack(">d", data["f64"])
            offset = offset + 1
            -- str
            table[offset] = string.pack(">s2", data["str"])
            offset = offset + 1
            -- bst
            table[offset] = string.pack(">s2", data["bst"])
            offset = offset + 1
            -- list
            local indexListTable = data["indexList"]
            table[offset] = string.pack(">I2", #indexListTable)
            offset = offset + 1
            for indexListIndex = 1, #indexListTable do
                -- list_binary
                table[offset] = indexListTable[indexListIndex]["listBinary"]
                offset = offset + 1
                -- list_boolean
                table[offset] = string.pack(">I1", indexListTable[indexListIndex]["listBoolean"] ~= 0 and 1 or 0)
                offset = offset + 1
                -- list_u8
                table[offset] = string.pack(">I1", indexListTable[indexListIndex]["listU8"])
                offset = offset + 1
                -- list_u16
                table[offset] = string.pack(">I2", indexListTable[indexListIndex]["listU16"])
                offset = offset + 1
                -- list_u32
                table[offset] = string.pack(">I4", indexListTable[indexListIndex]["listU32"])
                offset = offset + 1
                -- list_u64
                table[offset] = string.pack(">I8", indexListTable[indexListIndex]["listU64"])
                offset = offset + 1
                -- list_i8
                table[offset] = string.pack(">i1", indexListTable[indexListIndex]["listI8"])
                offset = offset + 1
                -- list_i16
                table[offset] = string.pack(">i2", indexListTable[indexListIndex]["listI16"])
                offset = offset + 1
                -- list_i32
                table[offset] = string.pack(">i4", indexListTable[indexListIndex]["listI32"])
                offset = offset + 1
                -- list_i64
                table[offset] = string.pack(">i8", indexListTable[indexListIndex]["listI64"])
                offset = offset + 1
                -- list_f32
                table[offset] = string.pack(">f", indexListTable[indexListIndex]["listF32"])
                offset = offset + 1
                -- list_f64
                table[offset] = string.pack(">d", indexListTable[indexListIndex]["listF64"])
                offset = offset + 1
                -- list_str
                table[offset] = string.pack(">s2", indexListTable[indexListIndex]["listStr"])
                offset = offset + 1
                -- list_bst
                table[offset] = string.pack(">s2", indexListTable[indexListIndex]["listBst"])
                offset = offset + 1
            end
            -- key_list
            local keyListTable = data["keyList"]
            table[offset] = string.pack(">I2", #keyListTable)
            offset = offset + 1
            for _, keyListItemData in pairs(keyListTable) do
                -- list_binary
                table[offset] = keyListItemData["listBinary"]
                offset = offset + 1
                -- list_boolean
                table[offset] = string.pack(">I1", keyListItemData["listBoolean"] ~= 0 and 1 or 0)
                offset = offset + 1
                -- list_u8
                table[offset] = string.pack(">I1", keyListItemData["listU8"])
                offset = offset + 1
                -- list_u16
                table[offset] = string.pack(">I2", keyListItemData["listU16"])
                offset = offset + 1
                -- list_u32
                table[offset] = string.pack(">I4", keyListItemData["listU32"])
                offset = offset + 1
                -- list_u64
                table[offset] = string.pack(">I8", keyListItemData["listU64"])
                offset = offset + 1
                -- list_i8
                table[offset] = string.pack(">i1", keyListItemData["listI8"])
                offset = offset + 1
                -- list_i16
                table[offset] = string.pack(">i2", keyListItemData["listI16"])
                offset = offset + 1
                -- list_i32
                table[offset] = string.pack(">i4", keyListItemData["listI32"])
                offset = offset + 1
                -- list_i64
                table[offset] = string.pack(">i8", keyListItemData["listI64"])
                offset = offset + 1
                -- list_f32
                table[offset] = string.pack(">f", keyListItemData["listF32"])
                offset = offset + 1
                -- list_f64
                table[offset] = string.pack(">d", keyListItemData["listF64"])
                offset = offset + 1
                -- list_str
                table[offset] = string.pack(">s2", keyListItemData["listStr"])
                offset = offset + 1
                -- list_bst
                table[offset] = string.pack(">s2", keyListItemData["listBst"])
                offset = offset + 1
            end
            return table
        end
    }
    local method = switch[protocol]
    if method then
        return method()
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function decodeTestProtocol(offset, protocol, data)
    local switch = {
        [65535] = function()
            local offset = offset
            -- binary
            local binary = string.unpack("c6", data, offset)
            offset = offset + 6
            -- boolean
            local boolean = string.unpack(">I1", data, offset) ~= 0
            offset = offset + 1
            -- u8
            local u8 = string.unpack(">I1", data, offset)
            offset = offset + 1
            -- u16
            local u16 = string.unpack(">I2", data, offset)
            offset = offset + 2
            -- u32
            local u32 = string.unpack(">I4", data, offset)
            offset = offset + 4
            -- u64
            local u64 = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- i8
            local i8 = string.unpack(">i1", data, offset)
            offset = offset + 1
            -- i16
            local i16 = string.unpack(">i2", data, offset)
            offset = offset + 2
            -- i32
            local i32 = string.unpack(">i4", data, offset)
            offset = offset + 4
            -- i64
            local i64 = string.unpack(">i8", data, offset)
            offset = offset + 8
            -- f32
            local f32 = string.unpack(">f", data, offset)
            offset = offset + 4
            -- f64
            local f64 = string.unpack(">d", data, offset)
            offset = offset + 8
            -- str
            local str = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(str)
            -- bst
            local bst = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(bst)
            -- list
            local indexList = {}
            local indexListLength = string.unpack(">I2", data, offset)
            offset = offset + 2
            for indexListIndex = 1, indexListLength do
                -- list_binary
                local listBinary = string.unpack("c6", data, offset)
                offset = offset + 6
                -- list_boolean
                local listBoolean = string.unpack(">I1", data, offset) ~= 0
                offset = offset + 1
                -- list_u8
                local listU8 = string.unpack(">I1", data, offset)
                offset = offset + 1
                -- list_u16
                local listU16 = string.unpack(">I2", data, offset)
                offset = offset + 2
                -- list_u32
                local listU32 = string.unpack(">I4", data, offset)
                offset = offset + 4
                -- list_u64
                local listU64 = string.unpack(">I8", data, offset)
                offset = offset + 8
                -- list_i8
                local listI8 = string.unpack(">i1", data, offset)
                offset = offset + 1
                -- list_i16
                local listI16 = string.unpack(">i2", data, offset)
                offset = offset + 2
                -- list_i32
                local listI32 = string.unpack(">i4", data, offset)
                offset = offset + 4
                -- list_i64
                local listI64 = string.unpack(">i8", data, offset)
                offset = offset + 8
                -- list_f32
                local listF32 = string.unpack(">f", data, offset)
                offset = offset + 4
                -- list_f64
                local listF64 = string.unpack(">d", data, offset)
                offset = offset + 8
                -- list_str
                local listStr = string.unpack(">s2", data, offset)
                offset = offset + 2 + string.len(listStr)
                -- list_bst
                local listBst = string.unpack(">s2", data, offset)
                offset = offset + 2 + string.len(listBst)
                indexList[indexListIndex] = {listBinary = listBinary, listBoolean = listBoolean, listU8 = listU8, listU16 = listU16, listU32 = listU32, listU64 = listU64, listI8 = listI8, listI16 = listI16, listI32 = listI32, listI64 = listI64, listF32 = listF32, listF64 = listF64, listStr = listStr, listBst = listBst}
            end
            -- key_list
            local keyList = {}
            local keyListLength = string.unpack(">I2", data, offset)
            offset = offset + 2
            for keyListIndex = 1, keyListLength do
                -- list_binary
                local listBinary = string.unpack("c6", data, offset)
                offset = offset + 6
                -- list_boolean
                local listBoolean = string.unpack(">I1", data, offset) ~= 0
                offset = offset + 1
                -- list_u8
                local listU8 = string.unpack(">I1", data, offset)
                offset = offset + 1
                -- list_u16
                local listU16 = string.unpack(">I2", data, offset)
                offset = offset + 2
                -- list_u32
                local listU32 = string.unpack(">I4", data, offset)
                offset = offset + 4
                -- list_u64
                local listU64 = string.unpack(">I8", data, offset)
                offset = offset + 8
                -- list_i8
                local listI8 = string.unpack(">i1", data, offset)
                offset = offset + 1
                -- list_i16
                local listI16 = string.unpack(">i2", data, offset)
                offset = offset + 2
                -- list_i32
                local listI32 = string.unpack(">i4", data, offset)
                offset = offset + 4
                -- list_i64
                local listI64 = string.unpack(">i8", data, offset)
                offset = offset + 8
                -- list_f32
                local listF32 = string.unpack(">f", data, offset)
                offset = offset + 4
                -- list_f64
                local listF64 = string.unpack(">d", data, offset)
                offset = offset + 8
                -- list_str
                local listStr = string.unpack(">s2", data, offset)
                offset = offset + 2 + string.len(listStr)
                -- list_bst
                local listBst = string.unpack(">s2", data, offset)
                offset = offset + 2 + string.len(listBst)
                keyList[listU8] = {listBinary = listBinary, listBoolean = listBoolean, listU8 = listU8, listU16 = listU16, listU32 = listU32, listU64 = listU64, listI8 = listI8, listI16 = listI16, listI32 = listI32, listI64 = listI64, listF32 = listF32, listF64 = listF64, listStr = listStr, listBst = listBst}
            end
            return {binary = binary, boolean = boolean, u8 = u8, u16 = u16, u32 = u32, u64 = u64, i8 = i8, i16 = i16, i32 = i32, i64 = i64, f32 = f32, f64 = f64, str = str, bst = bst, indexList = indexList, keyList = keyList}
        end
    }
    local method = switch[protocol]
    if method then
        return method()
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end