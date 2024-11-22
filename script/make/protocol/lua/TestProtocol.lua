TestProtocol = {}

function TestProtocol.encode(offset, protocol, data)
    if protocol == 65532 then
        local offset = offset
        local table = {}
        -- single i16
        table[offset] = string.pack(">i2", data)
        offset = offset + 1
        return table
    elseif protocol == 65533 then
        local offset = offset
        local table = {}
        -- single list
        table[offset] = string.pack(">I2", #data)
        offset = offset + 1
        for dataIndex = 1, #data do
            local dataItem = data[dataIndex]
            -- single u32
            table[offset] = string.pack(">I4", dataItem)
            offset = offset + 1
        end
        return table
    elseif protocol == 65534 then
        local offset = offset
        local table = {}
        -- key single list
        table[offset] = string.pack(">I2", #data)
        offset = offset + 1
        for _, dataItem in pairs(data) do
            -- key single u32
            table[offset] = string.pack(">I4", dataItem)
            offset = offset + 1
        end
        return table
    elseif protocol == 65535 then
        local offset = offset
        local table = {}
        -- binary
        table[offset] = data["binary"]
        offset = offset + 1
        -- bool
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
        -- i16
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
        -- convert
        local tuple = data["tuple"];
        -- tuple binary
        table[offset] = tuple["binary"]
        offset = offset + 1
        -- convert
        local tupleSub = tuple["sub"];
        -- tuple tuple u8
        table[offset] = string.pack(">I1", tupleSub["u8"])
        offset = offset + 1
        -- tuple tuple str
        table[offset] = string.pack(">s2", tupleSub["str"])
        offset = offset + 1
        -- tuple list
        local tupleList = tuple["list"]
        table[offset] = string.pack(">I2", #tupleList)
        offset = offset + 1
        for tupleListIndex = 1, #tupleList do
            local tupleListItem = tupleList[tupleListIndex]
            -- tuple list i16
            table[offset] = string.pack(">i2", tupleListItem["i16"])
            offset = offset + 1
            -- tuple list bst
            table[offset] = string.pack(">s2", tupleListItem["bst"])
            offset = offset + 1
        end
        -- 
        local tupleSingle = tuple["single"]
        table[offset] = string.pack(">I2", #tupleSingle)
        offset = offset + 1
        for tupleSingleIndex = 1, #tupleSingle do
            local tupleSingleItem = tupleSingle[tupleSingleIndex]
            -- bool
            table[offset] = string.pack(">I1", tupleSingleItem ~= 0 and 1 or 0)
            offset = offset + 1
        end
        -- list
        local indexList = data["indexList"]
        table[offset] = string.pack(">I2", #indexList)
        offset = offset + 1
        for indexListIndex = 1, #indexList do
            local indexListItem = indexList[indexListIndex]
            -- tuple binary
            table[offset] = indexListItem["binary"]
            offset = offset + 1
            -- convert
            local indexListItemSub = indexListItem["sub"];
            -- tuple tuple u8
            table[offset] = string.pack(">I1", indexListItemSub["u8"])
            offset = offset + 1
            -- tuple tuple str
            table[offset] = string.pack(">s2", indexListItemSub["str"])
            offset = offset + 1
            -- tuple list
            local indexListItemList = indexListItem["list"]
            table[offset] = string.pack(">I2", #indexListItemList)
            offset = offset + 1
            for indexListItemListIndex = 1, #indexListItemList do
                local indexListItemListItem = indexListItemList[indexListItemListIndex]
                -- tuple list i16
                table[offset] = string.pack(">i2", indexListItemListItem["i16"])
                offset = offset + 1
                -- tuple list bst
                table[offset] = string.pack(">s2", indexListItemListItem["bst"])
                offset = offset + 1
            end
            -- 
            local indexListItemSingle = indexListItem["single"]
            table[offset] = string.pack(">I2", #indexListItemSingle)
            offset = offset + 1
            for indexListItemSingleIndex = 1, #indexListItemSingle do
                local indexListItemSingleItem = indexListItemSingle[indexListItemSingleIndex]
                -- bool
                table[offset] = string.pack(">I1", indexListItemSingleItem ~= 0 and 1 or 0)
                offset = offset + 1
            end
        end
        -- 
        local keyList = data["keyList"]
        table[offset] = string.pack(">I2", #keyList)
        offset = offset + 1
        for _, keyListItem in pairs(keyList) do
            -- binary
            table[offset] = keyListItem["binary"]
            offset = offset + 1
            -- bool
            table[offset] = string.pack(">I1", keyListItem["boolean"] ~= 0 and 1 or 0)
            offset = offset + 1
            -- u8
            table[offset] = string.pack(">I1", keyListItem["u8"])
            offset = offset + 1
            -- u16
            table[offset] = string.pack(">I2", keyListItem["u16"])
            offset = offset + 1
            -- u32
            table[offset] = string.pack(">I4", keyListItem["u32"])
            offset = offset + 1
            -- u64
            table[offset] = string.pack(">I8", keyListItem["u64"])
            offset = offset + 1
            -- i8
            table[offset] = string.pack(">i1", keyListItem["i8"])
            offset = offset + 1
            -- i16
            table[offset] = string.pack(">i2", keyListItem["i16"])
            offset = offset + 1
            -- i32
            table[offset] = string.pack(">i4", keyListItem["i32"])
            offset = offset + 1
            -- i64
            table[offset] = string.pack(">i8", keyListItem["i64"])
            offset = offset + 1
            -- f32
            table[offset] = string.pack(">f", keyListItem["f32"])
            offset = offset + 1
            -- f64
            table[offset] = string.pack(">d", keyListItem["f64"])
            offset = offset + 1
            -- str
            table[offset] = string.pack(">s2", keyListItem["str"])
            offset = offset + 1
            -- bst
            table[offset] = string.pack(">s2", keyListItem["bst"])
            offset = offset + 1
        end
        return table
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function TestProtocol.decode(offset, protocol, data)
    if protocol == 65532 then
        local offset = offset
        -- single i16
        local data = string.unpack(">i2", data, offset)
        offset = offset + 2
        return data
    elseif protocol == 65533 then
        local offset = offset
        -- single list
        local data = {}
        local dataLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- single u32
            local item = string.unpack(">I4", data, offset)
            offset = offset + 4
            data[dataIndex] = item
        end
        return data
    elseif protocol == 65534 then
        local offset = offset
        -- key single list
        local data = {}
        local dataLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- key single u32
            local item = string.unpack(">I4", data, offset)
            offset = offset + 4
            data[u32] = item
        end
        return data
    elseif protocol == 65535 then
        local offset = offset
        -- 
        -- binary
        local binary = string.unpack("c6", data, offset)
        offset = offset + 6
        -- bool
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
        -- i16
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
        -- tuple binary
        local tupleBinary = string.unpack("c6", data, offset)
        offset = offset + 6
        -- tuple tuple
        -- tuple tuple u8
        local tupleSubU8 = string.unpack(">I1", data, offset)
        offset = offset + 1
        -- tuple tuple str
        local tupleSubStr = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(tupleSubStr)
        -- object
        local tupleSub = {u8 = tupleSubU8, str = tupleSubStr}
        -- tuple list
        local tupleList = {}
        local tupleListLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for tupleListIndex = 1, tupleListLength do
            -- 
            -- tuple list i16
            local tupleListI16 = string.unpack(">i2", data, offset)
            offset = offset + 2
            -- tuple list bst
            local tupleListBst = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(tupleListBst)
            -- object
            local tupleListItem = {i16 = tupleListI16, bst = tupleListBst}
            tupleList[tupleListIndex] = tupleListItem
        end
        -- 
        local tupleSingle = {}
        local tupleSingleLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for tupleSingleIndex = 1, tupleSingleLength do
            -- bool
            local tupleSingleItem = string.unpack(">I1", data, offset) ~= 0
            offset = offset + 1
            tupleSingle[tupleSingleIndex] = tupleSingleItem
        end
        -- object
        local tuple = {binary = tupleBinary, sub = tupleSub, list = tupleList, single = tupleSingle}
        -- list
        local indexList = {}
        local indexListLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for indexListIndex = 1, indexListLength do
            -- 
            -- tuple binary
            local indexListBinary = string.unpack("c6", data, offset)
            offset = offset + 6
            -- tuple tuple
            -- tuple tuple u8
            local indexListSubU8 = string.unpack(">I1", data, offset)
            offset = offset + 1
            -- tuple tuple str
            local indexListSubStr = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(indexListSubStr)
            -- object
            local indexListSub = {u8 = indexListSubU8, str = indexListSubStr}
            -- tuple list
            local indexListList = {}
            local indexListListLength = string.unpack(">I2", data, offset)
            offset = offset + 2
            for indexListListIndex = 1, indexListListLength do
                -- 
                -- tuple list i16
                local indexListListI16 = string.unpack(">i2", data, offset)
                offset = offset + 2
                -- tuple list bst
                local indexListListBst = string.unpack(">s2", data, offset)
                offset = offset + 2 + string.len(indexListListBst)
                -- object
                local indexListListItem = {i16 = indexListListI16, bst = indexListListBst}
                indexListList[indexListListIndex] = indexListListItem
            end
            -- 
            local indexListSingle = {}
            local indexListSingleLength = string.unpack(">I2", data, offset)
            offset = offset + 2
            for indexListSingleIndex = 1, indexListSingleLength do
                -- bool
                local indexListSingleItem = string.unpack(">I1", data, offset) ~= 0
                offset = offset + 1
                indexListSingle[indexListSingleIndex] = indexListSingleItem
            end
            -- object
            local indexListItem = {binary = indexListBinary, sub = indexListSub, list = indexListList, single = indexListSingle}
            indexList[indexListIndex] = indexListItem
        end
        -- 
        local keyList = {}
        local keyListLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for keyListIndex = 1, keyListLength do
            -- 
            -- binary
            local keyListBinary = string.unpack("c6", data, offset)
            offset = offset + 6
            -- boolean
            local keyListBoolean = string.unpack(">I1", data, offset) ~= 0
            offset = offset + 1
            -- u8
            local keyListU8 = string.unpack(">I1", data, offset)
            offset = offset + 1
            -- u16
            local keyListU16 = string.unpack(">I2", data, offset)
            offset = offset + 2
            -- u32
            local keyListU32 = string.unpack(">I4", data, offset)
            offset = offset + 4
            -- u64
            local keyListU64 = string.unpack(">I8", data, offset)
            offset = offset + 8
            -- i8
            local keyListI8 = string.unpack(">i1", data, offset)
            offset = offset + 1
            -- i16
            local keyListI16 = string.unpack(">i2", data, offset)
            offset = offset + 2
            -- i32
            local keyListI32 = string.unpack(">i4", data, offset)
            offset = offset + 4
            -- i64
            local keyListI64 = string.unpack(">i8", data, offset)
            offset = offset + 8
            -- f32
            local keyListF32 = string.unpack(">f", data, offset)
            offset = offset + 4
            -- f64
            local keyListF64 = string.unpack(">d", data, offset)
            offset = offset + 8
            -- str
            local keyListStr = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(keyListStr)
            -- bst
            local keyListBst = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(keyListBst)
            -- object
            local keyListItem = {binary = keyListBinary, boolean = keyListBoolean, u8 = keyListU8, u16 = keyListU16, u32 = keyListU32, u64 = keyListU64, i8 = keyListI8, i16 = keyListI16, i32 = keyListI32, i64 = keyListI64, f32 = keyListF32, f64 = keyListF64, str = keyListStr, bst = keyListBst}
            keyList[u8] = keyListItem
        end
        -- object
        local data = {binary = binary, boolean = boolean, u8 = u8, u16 = u16, u32 = u32, u64 = u64, i8 = i8, i16 = i16, i32 = i32, i64 = i64, f32 = f32, f64 = f64, str = str, bst = bst, tuple = tuple, indexList = indexList, keyList = keyList}
        return data
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end