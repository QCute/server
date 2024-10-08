TestProtocol = {}

function TestProtocol.encode(offset, protocol, data)
    if protocol == 65535 then
        local offset = offset
        local table = {}
        -- binary
        table[offset] = data["data"]["binary"]
        offset = offset + 1
        -- bool
        table[offset] = string.pack(">I1", data["data"]["bool"] ~= 0 and 1 or 0)
        offset = offset + 1
        -- u8
        table[offset] = string.pack(">I1", data["data"]["u8"])
        offset = offset + 1
        -- u16
        table[offset] = string.pack(">I2", data["data"]["u16"])
        offset = offset + 1
        -- u32
        table[offset] = string.pack(">I4", data["data"]["u32"])
        offset = offset + 1
        -- u64
        table[offset] = string.pack(">I8", data["data"]["u64"])
        offset = offset + 1
        -- i8
        table[offset] = string.pack(">i1", data["data"]["i8"])
        offset = offset + 1
        -- i16
        table[offset] = string.pack(">i2", data["data"]["i16"])
        offset = offset + 1
        -- i32
        table[offset] = string.pack(">i4", data["data"]["i32"])
        offset = offset + 1
        -- i16
        table[offset] = string.pack(">i8", data["data"]["i64"])
        offset = offset + 1
        -- f32
        table[offset] = string.pack(">f", data["data"]["f32"])
        offset = offset + 1
        -- f64
        table[offset] = string.pack(">d", data["data"]["f64"])
        offset = offset + 1
        -- str
        table[offset] = string.pack(">s2", data["data"]["str"])
        offset = offset + 1
        -- bst
        table[offset] = string.pack(">s2", data["data"]["bst"])
        offset = offset + 1
        -- tuple binary
        table[offset] = data["data"]["tuple"]["binary"]
        offset = offset + 1
        -- tuple tuple u8
        table[offset] = string.pack(">I1", data["data"]["tuple"]["sub"]["u8"])
        offset = offset + 1
        -- tuple tuple str
        table[offset] = string.pack(">s2", data["data"]["tuple"]["sub"]["str"])
        offset = offset + 1
        -- tuple list
        local listTable = data["data"]["tuple"]["list"]
        table[offset] = string.pack(">I2", #listTable)
        offset = offset + 1
        for listIndex = 1, #listTable do
            local listItemData = listTable[listIndex]
            -- tuple list i16
            table[offset] = string.pack(">i2", listItemData["i16"])
            offset = offset + 1
            -- tuple list bst
            table[offset] = string.pack(">s2", listItemData["bst"])
            offset = offset + 1
        end
        -- 
        local singleTable = data["data"]["tuple"]["single"]
        table[offset] = string.pack(">I2", #singleTable)
        offset = offset + 1
        for singleIndex = 1, #singleTable do
            local singleItemData = singleTable[singleIndex]
            -- bool
            table[offset] = string.pack(">I1", singleItemData ~= 0 and 1 or 0)
            offset = offset + 1
        end
        -- list
        local indexListTable = data["data"]["indexList"]
        table[offset] = string.pack(">I2", #indexListTable)
        offset = offset + 1
        for indexListIndex = 1, #indexListTable do
            local indexListItemData = indexListTable[indexListIndex]
            -- tuple binary
            table[offset] = indexListItemData["binary"]
            offset = offset + 1
            -- tuple tuple u8
            table[offset] = string.pack(">I1", indexListItemData["sub"]["u8"])
            offset = offset + 1
            -- tuple tuple str
            table[offset] = string.pack(">s2", indexListItemData["sub"]["str"])
            offset = offset + 1
            -- tuple list
            local listTable = indexListItemData["list"]
            table[offset] = string.pack(">I2", #listTable)
            offset = offset + 1
            for listIndex = 1, #listTable do
                local listItemData = listTable[listIndex]
                -- tuple list i16
                table[offset] = string.pack(">i2", listItemData["i16"])
                offset = offset + 1
                -- tuple list bst
                table[offset] = string.pack(">s2", listItemData["bst"])
                offset = offset + 1
            end
            -- 
            local singleTable = indexListItemData["single"]
            table[offset] = string.pack(">I2", #singleTable)
            offset = offset + 1
            for singleIndex = 1, #singleTable do
                local singleItemData = singleTable[singleIndex]
                -- bool
                table[offset] = string.pack(">I1", singleItemData ~= 0 and 1 or 0)
                offset = offset + 1
            end
        end
        -- 
        local keyListTable = data["data"]["keyList"]
        table[offset] = string.pack(">I2", #keyListTable)
        offset = offset + 1
        for _, keyListItemData in pairs(keyListTable) do
            -- binary
            table[offset] = keyListItemData["binary"]
            offset = offset + 1
            -- bool
            table[offset] = string.pack(">I1", keyListItemData["bool"] ~= 0 and 1 or 0)
            offset = offset + 1
            -- u8
            table[offset] = string.pack(">I1", keyListItemData["u8"])
            offset = offset + 1
            -- u16
            table[offset] = string.pack(">I2", keyListItemData["u16"])
            offset = offset + 1
            -- u32
            table[offset] = string.pack(">I4", keyListItemData["u32"])
            offset = offset + 1
            -- u64
            table[offset] = string.pack(">I8", keyListItemData["u64"])
            offset = offset + 1
            -- i8
            table[offset] = string.pack(">i1", keyListItemData["i8"])
            offset = offset + 1
            -- i16
            table[offset] = string.pack(">i2", keyListItemData["i16"])
            offset = offset + 1
            -- i32
            table[offset] = string.pack(">i4", keyListItemData["i32"])
            offset = offset + 1
            -- i64
            table[offset] = string.pack(">i8", keyListItemData["i64"])
            offset = offset + 1
            -- f32
            table[offset] = string.pack(">f", keyListItemData["f32"])
            offset = offset + 1
            -- f64
            table[offset] = string.pack(">d", keyListItemData["f64"])
            offset = offset + 1
            -- str
            table[offset] = string.pack(">s2", keyListItemData["str"])
            offset = offset + 1
            -- bst
            table[offset] = string.pack(">s2", keyListItemData["bst"])
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
        -- 
        -- binary
        local binary = string.unpack("c6", data, offset)
        offset = offset + 6
        -- bool
        local bool = string.unpack(">I1", data, offset) ~= 0
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
        local binary = string.unpack("c6", data, offset)
        offset = offset + 6
        -- tuple tuple
        -- tuple tuple u8
        local u8 = string.unpack(">I1", data, offset)
        offset = offset + 1
        -- tuple tuple str
        local str = string.unpack(">s2", data, offset)
        offset = offset + 2 + string.len(str)
        -- object
        local sub = {u8 = u8, str = str}
        -- tuple list
        local list = {}
        local listLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for listIndex = 1, listLength do
            -- 
            -- tuple list i16
            local i16 = string.unpack(">i2", data, offset)
            offset = offset + 2
            -- tuple list bst
            local bst = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(bst)
            -- object
            local listItem = {i16 = i16, bst = bst}
            list[listIndex] = listItem
        end
        -- 
        local single = {}
        local singleLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for singleIndex = 1, singleLength do
            -- bool
            local singleItem = string.unpack(">I1", data, offset) ~= 0
            offset = offset + 1
            single[singleIndex] = singleItem
        end
        -- object
        local tuple = {binary = binary, sub = sub, list = list, single = single}
        -- list
        local indexList = {}
        local indexListLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for indexListIndex = 1, indexListLength do
            -- 
            -- tuple binary
            local binary = string.unpack("c6", data, offset)
            offset = offset + 6
            -- tuple tuple
            -- tuple tuple u8
            local u8 = string.unpack(">I1", data, offset)
            offset = offset + 1
            -- tuple tuple str
            local str = string.unpack(">s2", data, offset)
            offset = offset + 2 + string.len(str)
            -- object
            local sub = {u8 = u8, str = str}
            -- tuple list
            local list = {}
            local listLength = string.unpack(">I2", data, offset)
            offset = offset + 2
            for listIndex = 1, listLength do
                -- 
                -- tuple list i16
                local i16 = string.unpack(">i2", data, offset)
                offset = offset + 2
                -- tuple list bst
                local bst = string.unpack(">s2", data, offset)
                offset = offset + 2 + string.len(bst)
                -- object
                local listItem = {i16 = i16, bst = bst}
                list[listIndex] = listItem
            end
            -- 
            local single = {}
            local singleLength = string.unpack(">I2", data, offset)
            offset = offset + 2
            for singleIndex = 1, singleLength do
                -- bool
                local singleItem = string.unpack(">I1", data, offset) ~= 0
                offset = offset + 1
                single[singleIndex] = singleItem
            end
            -- object
            local indexListItem = {binary = binary, sub = sub, list = list, single = single}
            indexList[indexListIndex] = indexListItem
        end
        -- 
        local keyList = {}
        local keyListLength = string.unpack(">I2", data, offset)
        offset = offset + 2
        for keyListIndex = 1, keyListLength do
            -- 
            -- binary
            local binary = string.unpack("c6", data, offset)
            offset = offset + 6
            -- bool
            local bool = string.unpack(">I1", data, offset) ~= 0
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
            -- object
            local keyListItem = {binary = binary, bool = bool, u8 = u8, u16 = u16, u32 = u32, u64 = u64, i8 = i8, i16 = i16, i32 = i32, i64 = i64, f32 = f32, f64 = f64, str = str, bst = bst}
            keyList[u8] = keyListItem
        end
        -- object
        local data = {binary = binary, bool = bool, u8 = u8, u16 = u16, u32 = u32, u64 = u64, i8 = i8, i16 = i16, i32 = i32, i64 = i64, f32 = f32, f64 = f64, str = str, bst = bst, tuple = tuple, indexList = indexList, keyList = keyList}
        return data
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end