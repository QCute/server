TestProtocol = {}

function TestProtocol.encode(offset, protocol, data)
    if protocol == 65535 then
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
        -- tuple_binary
        table[offset] = data["tuple"]["tupleBinary"]
        offset = offset + 1
        -- tuple_sub_tuple_u8
        table[offset] = string.pack(">I1", data["tuple"]["tupleSubTuple"]["tupleSubTupleU8"])
        offset = offset + 1
        -- tuple_sub_tuple_str
        table[offset] = string.pack(">s2", data["tuple"]["tupleSubTuple"]["tupleSubTupleStr"])
        offset = offset + 1
        -- tuple_sub_list
        local tupleSubListTable = data["tuple"]["tupleSubList"]
        table[offset] = string.pack(">I2", #tupleSubListTable)
        offset = offset + 1
        for tupleSubListIndex = 1, #tupleSubListTable do
            local tupleSubListItemData = tupleSubListTable[tupleSubListIndex]
            -- tuple_sub_list_i16
            table[offset] = string.pack(">i2", tupleSubListItemData["tupleSubListI16"])
            offset = offset + 1
            -- tuple_sub_list_bst
            table[offset] = string.pack(">s2", tupleSubListItemData["tupleSubListBst"])
            offset = offset + 1
        end
        -- tuple_sub_list_single
        local tupleSubListSingleTable = data["tuple"]["tupleSubListSingle"]
        table[offset] = string.pack(">I2", #tupleSubListSingleTable)
        offset = offset + 1
        for tupleSubListSingleIndex = 1, #tupleSubListSingleTable do
            local tupleSubListSingleItemData = tupleSubListSingleTable[tupleSubListSingleIndex]
            -- tuple_sub_list_single_bool
            table[offset] = string.pack(">I1", tupleSubListSingleItemData ~= 0 and 1 or 0)
            offset = offset + 1
        end
        -- list
        local indexListTable = data["indexList"]
        table[offset] = string.pack(">I2", #indexListTable)
        offset = offset + 1
        for indexListIndex = 1, #indexListTable do
            local indexListItemData = indexListTable[indexListIndex]
            -- list_binary
            table[offset] = indexListItemData["listBinary"]
            offset = offset + 1
            -- list_sub_tuple_u8
            table[offset] = string.pack(">I1", indexListItemData["listSubTuple"]["listSubTupleU8"])
            offset = offset + 1
            -- list_sub_tuple_str
            table[offset] = string.pack(">s2", indexListItemData["listSubTuple"]["listSubTupleStr"])
            offset = offset + 1
            -- list_sub_list
            local listSubListTable = indexListItemData["listSubList"]
            table[offset] = string.pack(">I2", #listSubListTable)
            offset = offset + 1
            for listSubListIndex = 1, #listSubListTable do
                local listSubListItemData = listSubListTable[listSubListIndex]
                -- list_sub_list_i16
                table[offset] = string.pack(">i2", listSubListItemData["listSubListI16"])
                offset = offset + 1
                -- list_sub_list_bst
                table[offset] = string.pack(">s2", listSubListItemData["listSubListBst"])
                offset = offset + 1
            end
            -- list_sub_list_single
            local listSubListSingleTable = indexListItemData["listSubListSingle"]
            table[offset] = string.pack(">I2", #listSubListSingleTable)
            offset = offset + 1
            for listSubListSingleIndex = 1, #listSubListSingleTable do
                local listSubListSingleItemData = listSubListSingleTable[listSubListSingleIndex]
                -- list_sub_list_single_bool
                table[offset] = string.pack(">I1", listSubListSingleItemData ~= 0 and 1 or 0)
                offset = offset + 1
            end
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
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function TestProtocol.decode(offset, protocol, data)
    if protocol == 65535 then
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
        -- tuple
        -- tuple_binary
        local tupleBinary = string.unpack("c6", data, offset)
        offset = offset + 6
        -- tuple_sub_tuple
        -- tuple_sub_tuple_u8
        local tupleSubTupleU8 = string.unpack(">I1", data, offset)
        offset = offset + 1
        -- tuple_sub_tuple_str
        local tupleSubTupleStr = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(tupleSubTupleStr)
        -- object
        local tupleSubTuple = {tupleSubTupleU8 = tupleSubTupleU8, tupleSubTupleStr = tupleSubTupleStr}
        -- tuple_sub_list
        local tupleSubList = {}
        local tupleSubListLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for tupleSubListIndex = 1, tupleSubListLength do
            -- tuple_sub_list_i16
            local tupleSubListI16 = string.unpack(">i2", data, offset)
            offset = offset + 2
            -- tuple_sub_list_bst
            local tupleSubListBst = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(tupleSubListBst)
            tupleSubList[tupleSubListIndex] = {tupleSubListI16 = tupleSubListI16, tupleSubListBst = tupleSubListBst}
        end
        -- tuple_sub_list_single
        local tupleSubListSingle = {}
        local tupleSubListSingleLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for tupleSubListSingleIndex = 1, tupleSubListSingleLength do
            -- tuple_sub_list_single_bool
            local tupleSubListSingleBool = string.unpack(">I1", data, offset) ~= 0
            offset = offset + 1
            tupleSubListSingle[tupleSubListSingleIndex] = tupleSubListSingleBool
        end
        -- object
        local tuple = {tupleBinary = tupleBinary, tupleSubTuple = tupleSubTuple, tupleSubList = tupleSubList, tupleSubListSingle = tupleSubListSingle}
        -- list
        local indexList = {}
        local indexListLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for indexListIndex = 1, indexListLength do
            -- list_binary
            local listBinary = string.unpack("c6", data, offset)
            offset = offset + 6
            -- list_sub_tuple
            -- list_sub_tuple_u8
            local listSubTupleU8 = string.unpack(">I1", data, offset)
            offset = offset + 1
            -- list_sub_tuple_str
            local listSubTupleStr = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(listSubTupleStr)
            -- object
            local listSubTuple = {listSubTupleU8 = listSubTupleU8, listSubTupleStr = listSubTupleStr}
            -- list_sub_list
            local listSubList = {}
            local listSubListLength = string.unpack(">I2", data, offset)
            offset = offset + 2
            for listSubListIndex = 1, listSubListLength do
                -- list_sub_list_i16
                local listSubListI16 = string.unpack(">i2", data, offset)
                offset = offset + 2
                -- list_sub_list_bst
                local listSubListBst = string.unpack(">s2", data, offset)
                offset = offset + 2 + string.len(listSubListBst)
                listSubList[listSubListIndex] = {listSubListI16 = listSubListI16, listSubListBst = listSubListBst}
            end
            -- list_sub_list_single
            local listSubListSingle = {}
            local listSubListSingleLength = string.unpack(">I2", data, offset)
            offset = offset + 2
            for listSubListSingleIndex = 1, listSubListSingleLength do
                -- list_sub_list_single_bool
                local listSubListSingleBool = string.unpack(">I1", data, offset) ~= 0
                offset = offset + 1
                listSubListSingle[listSubListSingleIndex] = listSubListSingleBool
            end
            indexList[indexListIndex] = {listBinary = listBinary, listSubTuple = listSubTuple, listSubList = listSubList, listSubListSingle = listSubListSingle}
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
        return {binary = binary, boolean = boolean, u8 = u8, u16 = u16, u32 = u32, u64 = u64, i8 = i8, i16 = i16, i32 = i32, i64 = i64, f32 = f32, f64 = f64, str = str, bst = bst, tuple = tuple, indexList = indexList, keyList = keyList}
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end