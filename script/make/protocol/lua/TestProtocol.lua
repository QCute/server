--- @class TestTestSingleProtocolRequest
--- @field protocol number 65531
--- @field data integer

--- @class TestTestSingleProtocolRequest
--- @field protocol number 65531
--- @field data integer

--- @class TestTestListProtocolRequest
--- @field protocol number 65532
--- @field data integer[]

--- @class TestTestListProtocolRequest
--- @field protocol number 65532
--- @field data integer[]

--- @class TestTestListTupleProtocolRequest
--- @field protocol number 65533
--- @field data {
---     id: integer,                                                                                -- single u64
--- }[]

--- @class TestTestListTupleProtocolRequest
--- @field protocol number 65533
--- @field data {
---     name: string,                                                                               -- single ast
--- }[]

--- @class TestTestLsProtocolRequest
--- @field protocol number 65534
--- @field data {
---     id: integer,                                                                                -- single u32
---     theItemData: {
---         itemId: integer,                                                                        -- 
---         type: integer,                                                                          -- single u16
---     },                                                                                          -- 
---     name: {
---         ls: {
---             ix: integer,                                                                        -- 
---             nx: string,                                                                         -- 
---             rx: integer[],                                                                      -- 
---             sx: {
---                 ii: integer,                                                                    -- a list ii u32
---                 nn: string,                                                                     -- a list nn bst
---             }[],                                                                                -- 
---         }[],                                                                                    -- 
---         lss: integer[],                                                                         -- 
---     },                                                                                          -- 
--- }

--- @class TestTestLsProtocolRequest
--- @field protocol number 65534
--- @field data {
---     name: integer,                                                                              -- single u32
--- }[]

--- @class TestTestProtocolRequest
--- @field protocol number 65535
--- @field data {
---     binary: string,                                                                             -- binary
---     boolean: boolean,                                                                           -- bool
---     u8: integer,                                                                                -- u8
---     u16: integer,                                                                               -- u16
---     u32: integer,                                                                               -- u32
---     u64: integer,                                                                               -- u64
---     i8: integer,                                                                                -- i8
---     i16: integer,                                                                               -- i16
---     i32: integer,                                                                               -- i32
---     i64: integer,                                                                               -- i16
---     f32: number,                                                                                -- f32
---     f64: number,                                                                                -- f64
---     str: string,                                                                                -- str
---     bst: string,                                                                                -- bst
---     tuple: {
---         binary: string,                                                                         -- tuple binary
---         sub: {
---             u8: integer,                                                                        -- tuple tuple u8
---             str: string,                                                                        -- tuple tuple str
---         },                                                                                      -- tuple tuple
---         list: {
---             i16: integer,                                                                       -- tuple list i16
---             bst: string,                                                                        -- tuple list bst
---         }[],                                                                                    -- tuple list
---         single: boolean[],                                                                      -- 
---     },                                                                                          -- tuple
---     indexList: {
---         binary: string,                                                                         -- tuple binary
---         sub: {
---             u8: integer,                                                                        -- tuple tuple u8
---             str: string,                                                                        -- tuple tuple str
---         },                                                                                      -- tuple tuple
---         list: {
---             i16: integer,                                                                       -- tuple list i16
---             bst: string,                                                                        -- tuple list bst
---         }[],                                                                                    -- tuple list
---         single: boolean[],                                                                      -- 
---     }[],                                                                                        -- list
---     keyList: table<boolean|integer|number|string, {
---         binary: string,                                                                         -- binary
---         boolean: boolean,                                                                       -- bool
---         u8: integer,                                                                            -- u8
---         u16: integer,                                                                           -- u16
---         u32: integer,                                                                           -- u32
---         u64: integer,                                                                           -- u64
---         i8: integer,                                                                            -- i8
---         i16: integer,                                                                           -- i16
---         i32: integer,                                                                           -- i32
---         i64: integer,                                                                           -- i64
---         f32: number,                                                                            -- f32
---         f64: number,                                                                            -- f64
---         str: string,                                                                            -- str
---         bst: string,                                                                            -- bst
---     }>,                                                                                         -- 
--- }

--- @class TestTestProtocolRequest
--- @field protocol number 65535
--- @field data {
---     binary: string,                                                                             -- binary
---     boolean: boolean,                                                                           -- bool
---     u8: integer,                                                                                -- u8
---     u16: integer,                                                                               -- u16
---     u32: integer,                                                                               -- u32
---     u64: integer,                                                                               -- u64
---     i8: integer,                                                                                -- i8
---     i16: integer,                                                                               -- i16
---     i32: integer,                                                                               -- i32
---     i64: integer,                                                                               -- i16
---     f32: number,                                                                                -- f32
---     f64: number,                                                                                -- f64
---     str: string,                                                                                -- str
---     bst: string,                                                                                -- bst
---     tuple: {
---         binary: string,                                                                         -- tuple binary
---         sub: {
---             u8: integer,                                                                        -- tuple tuple u8
---             str: string,                                                                        -- tuple tuple str
---         },                                                                                      -- tuple tuple
---         list: {
---             i16: integer,                                                                       -- tuple list i16
---             bst: string,                                                                        -- tuple list bst
---         }[],                                                                                    -- tuple list
---         single: boolean[],                                                                      -- 
---     },                                                                                          -- tuple
---     indexList: {
---         binary: string,                                                                         -- tuple binary
---         sub: {
---             u8: integer,                                                                        -- tuple tuple u8
---             str: string,                                                                        -- tuple tuple str
---         },                                                                                      -- tuple tuple
---         list: {
---             i16: integer,                                                                       -- tuple list i16
---             bst: string,                                                                        -- tuple list bst
---         }[],                                                                                    -- tuple list
---         single: boolean[],                                                                      -- 
---     }[],                                                                                        -- list
---     keyList: table<boolean|integer|number|string, {
---         binary: string,                                                                         -- binary
---         boolean: boolean,                                                                       -- boolean
---         u8: integer,                                                                            -- u8
---         u16: integer,                                                                           -- u16
---         u32: integer,                                                                           -- u32
---         u64: integer,                                                                           -- u64
---         i8: integer,                                                                            -- i8
---         i16: integer,                                                                           -- i16
---         i32: integer,                                                                           -- i32
---         i64: integer,                                                                           -- i64
---         f32: number,                                                                            -- f32
---         f64: number,                                                                            -- f64
---         str: string,                                                                            -- str
---         bst: string,                                                                            -- bst
---     }>,                                                                                         -- 
--- }

TestProtocol = {}

function TestProtocol.encode(offset, protocol, data)
    if protocol == 65531 then
        local table = {}
        -- single i16
        table[offset] = string.pack(">i2", data)
        offset = offset + 1
        return table
    elseif protocol == 65532 then
        local table = {}

        -- single list
        table[offset] = string.pack(">I2", #data)
        offset = offset + 1
        for dataIndex = 1, #data do
            local dataData = data[dataIndex]
            -- single u32
            table[offset] = string.pack(">I4", dataData)
            offset = offset + 1
        end
        return table
    elseif protocol == 65533 then
        local table = {}

        -- single list
        table[offset] = string.pack(">I2", #data)
        offset = offset + 1
        for dataIndex = 1, #data do
            local dataData = data[dataIndex]

            -- single u64
            table[offset] = string.pack(">I8", dataData.id)
            offset = offset + 1
        end
        return table
    elseif protocol == 65534 then
        local table = {}

        -- single u32
        table[offset] = string.pack(">I4", data.id)
        offset = offset + 1
        local dataTheItemData = data.theItemData;
        -- 
        table[offset] = string.pack(">I4", dataTheItemData.itemId)
        offset = offset + 1
        -- single u16
        table[offset] = string.pack(">I2", dataTheItemData.type)
        offset = offset + 1
        local dataName = data.name;
        local dataNameLs = dataName.ls;
        -- 
        table[offset] = string.pack(">I2", #dataNameLs)
        offset = offset + 1
        for dataNameLsIndex = 1, #dataNameLs do
            local dataNameLsData = dataNameLs[dataNameLsIndex]

            -- 
            table[offset] = string.pack(">I4", dataNameLsData.ix)
            offset = offset + 1
            -- 
            table[offset] = string.pack(">s2", dataNameLsData.nx)
            offset = offset + 1
            local dataNameLsDataRx = dataNameLsData.rx;
            -- 
            table[offset] = string.pack(">I2", #dataNameLsDataRx)
            offset = offset + 1
            for dataNameLsDataRxIndex = 1, #dataNameLsDataRx do
                local dataNameLsDataRxData = dataNameLsDataRx[dataNameLsDataRxIndex]
                -- a list u32
                table[offset] = string.pack(">I4", dataNameLsDataRxData)
                offset = offset + 1
            end
            local dataNameLsDataSx = dataNameLsData.sx;
            -- 
            table[offset] = string.pack(">I2", #dataNameLsDataSx)
            offset = offset + 1
            for dataNameLsDataSxIndex = 1, #dataNameLsDataSx do
                local dataNameLsDataSxData = dataNameLsDataSx[dataNameLsDataSxIndex]

                -- a list ii u32
                table[offset] = string.pack(">I4", dataNameLsDataSxData.ii)
                offset = offset + 1
                -- a list nn bst
                table[offset] = string.pack(">s2", dataNameLsDataSxData.nn)
                offset = offset + 1
            end
        end
        local dataNameLss = dataName.lss;
        -- 
        table[offset] = string.pack(">I2", #dataNameLss)
        offset = offset + 1
        for dataNameLssIndex = 1, #dataNameLss do
            local dataNameLssData = dataNameLss[dataNameLssIndex]
            -- single u8
            table[offset] = string.pack(">I1", dataNameLssData)
            offset = offset + 1
        end
        return table
    elseif protocol == 65535 then
        local table = {}

        -- binary
        table[offset] = data.binary
        offset = offset + 1
        -- bool
        table[offset] = string.pack(">I1", data.boolean ~= 0 and 1 or 0)
        offset = offset + 1
        -- u8
        table[offset] = string.pack(">I1", data.u8)
        offset = offset + 1
        -- u16
        table[offset] = string.pack(">I2", data.u16)
        offset = offset + 1
        -- u32
        table[offset] = string.pack(">I4", data.u32)
        offset = offset + 1
        -- u64
        table[offset] = string.pack(">I8", data.u64)
        offset = offset + 1
        -- i8
        table[offset] = string.pack(">i1", data.i8)
        offset = offset + 1
        -- i16
        table[offset] = string.pack(">i2", data.i16)
        offset = offset + 1
        -- i32
        table[offset] = string.pack(">i4", data.i32)
        offset = offset + 1
        -- i16
        table[offset] = string.pack(">i8", data.i64)
        offset = offset + 1
        -- f32
        table[offset] = string.pack(">f", data.f32)
        offset = offset + 1
        -- f64
        table[offset] = string.pack(">d", data.f64)
        offset = offset + 1
        -- str
        table[offset] = string.pack(">s2", data.str)
        offset = offset + 1
        -- bst
        table[offset] = string.pack(">s2", data.bst)
        offset = offset + 1
        local dataTuple = data.tuple;
        -- tuple binary
        table[offset] = dataTuple.binary
        offset = offset + 1
        local dataTupleSub = dataTuple.sub;
        -- tuple tuple u8
        table[offset] = string.pack(">I1", dataTupleSub.u8)
        offset = offset + 1
        -- tuple tuple str
        table[offset] = string.pack(">s2", dataTupleSub.str)
        offset = offset + 1
        local dataTupleList = dataTuple.list;
        -- tuple list
        table[offset] = string.pack(">I2", #dataTupleList)
        offset = offset + 1
        for dataTupleListIndex = 1, #dataTupleList do
            local dataTupleListData = dataTupleList[dataTupleListIndex]

            -- tuple list i16
            table[offset] = string.pack(">i2", dataTupleListData.i16)
            offset = offset + 1
            -- tuple list bst
            table[offset] = string.pack(">s2", dataTupleListData.bst)
            offset = offset + 1
        end
        local dataTupleSingle = dataTuple.single;
        -- 
        table[offset] = string.pack(">I2", #dataTupleSingle)
        offset = offset + 1
        for dataTupleSingleIndex = 1, #dataTupleSingle do
            local dataTupleSingleData = dataTupleSingle[dataTupleSingleIndex]
            -- bool
            table[offset] = string.pack(">I1", dataTupleSingleData ~= 0 and 1 or 0)
            offset = offset + 1
        end
        local dataIndexList = data.indexList;
        -- list
        table[offset] = string.pack(">I2", #dataIndexList)
        offset = offset + 1
        for dataIndexListIndex = 1, #dataIndexList do
            local dataIndexListData = dataIndexList[dataIndexListIndex]

            -- tuple binary
            table[offset] = dataIndexListData.binary
            offset = offset + 1
            local dataIndexListDataSub = dataIndexListData.sub;
            -- tuple tuple u8
            table[offset] = string.pack(">I1", dataIndexListDataSub.u8)
            offset = offset + 1
            -- tuple tuple str
            table[offset] = string.pack(">s2", dataIndexListDataSub.str)
            offset = offset + 1
            local dataIndexListDataList = dataIndexListData.list;
            -- tuple list
            table[offset] = string.pack(">I2", #dataIndexListDataList)
            offset = offset + 1
            for dataIndexListDataListIndex = 1, #dataIndexListDataList do
                local dataIndexListDataListData = dataIndexListDataList[dataIndexListDataListIndex]

                -- tuple list i16
                table[offset] = string.pack(">i2", dataIndexListDataListData.i16)
                offset = offset + 1
                -- tuple list bst
                table[offset] = string.pack(">s2", dataIndexListDataListData.bst)
                offset = offset + 1
            end
            local dataIndexListDataSingle = dataIndexListData.single;
            -- 
            table[offset] = string.pack(">I2", #dataIndexListDataSingle)
            offset = offset + 1
            for dataIndexListDataSingleIndex = 1, #dataIndexListDataSingle do
                local dataIndexListDataSingleData = dataIndexListDataSingle[dataIndexListDataSingleIndex]
                -- bool
                table[offset] = string.pack(">I1", dataIndexListDataSingleData ~= 0 and 1 or 0)
                offset = offset + 1
            end
        end
        local dataKeyList = data.keyList;
        -- 
        table[offset] = string.pack(">I2", #dataKeyList)
        offset = offset + 1
        for _, dataKeyListData in pairs(dataKeyList) do

            -- binary
            table[offset] = dataKeyListData.binary
            offset = offset + 1
            -- bool
            table[offset] = string.pack(">I1", dataKeyListData.boolean ~= 0 and 1 or 0)
            offset = offset + 1
            -- u8
            table[offset] = string.pack(">I1", dataKeyListData.u8)
            offset = offset + 1
            -- u16
            table[offset] = string.pack(">I2", dataKeyListData.u16)
            offset = offset + 1
            -- u32
            table[offset] = string.pack(">I4", dataKeyListData.u32)
            offset = offset + 1
            -- u64
            table[offset] = string.pack(">I8", dataKeyListData.u64)
            offset = offset + 1
            -- i8
            table[offset] = string.pack(">i1", dataKeyListData.i8)
            offset = offset + 1
            -- i16
            table[offset] = string.pack(">i2", dataKeyListData.i16)
            offset = offset + 1
            -- i32
            table[offset] = string.pack(">i4", dataKeyListData.i32)
            offset = offset + 1
            -- i64
            table[offset] = string.pack(">i8", dataKeyListData.i64)
            offset = offset + 1
            -- f32
            table[offset] = string.pack(">f", dataKeyListData.f32)
            offset = offset + 1
            -- f64
            table[offset] = string.pack(">d", dataKeyListData.f64)
            offset = offset + 1
            -- str
            table[offset] = string.pack(">s2", dataKeyListData.str)
            offset = offset + 1
            -- bst
            table[offset] = string.pack(">s2", dataKeyListData.bst)
            offset = offset + 1
        end
        return table
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end

function TestProtocol.decode(offset, protocol, bytes)
    if protocol == 65531 then
        -- single i16
        local data = string.unpack(">i2", bytes, offset)
        offset = offset + 2
        return {protocol = 65531, data = data}
    elseif protocol == 65532 then
        -- single list
        local data = {}
        local dataLength = string.unpack(">I2", bytes, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- single u32
            local dataData = string.unpack(">I4", bytes, offset)
            offset = offset + 4
            data[dataIndex] = dataData
        end
        return {protocol = 65532, data = data}
    elseif protocol == 65533 then
        -- single list
        local data = {}
        local dataLength = string.unpack(">I2", bytes, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
            -- single ast
            local dataDataName = string.unpack(">s2", bytes, offset)
            offset = offset + 2 + string.len(dataDataName)
            -- object
            local dataData = {name = dataDataName}
            data[dataIndex] = dataData
        end
        return {protocol = 65533, data = data}
    elseif protocol == 65534 then
        -- single list
        local data = {}
        local dataLength = string.unpack(">I2", bytes, offset)
        offset = offset + 2
        for dataIndex = 1, dataLength do
            -- 
            -- single u32
            local dataDataName = string.unpack(">I4", bytes, offset)
            offset = offset + 4
            -- object
            local dataData = {name = dataDataName}
            data[dataIndex] = dataData
        end
        return {protocol = 65534, data = data}
    elseif protocol == 65535 then
        -- 
        -- binary
        local dataBinary = string.unpack("c6", bytes, offset)
        offset = offset + 6
        -- bool
        local dataBoolean = string.unpack(">I1", bytes, offset) ~= 0
        offset = offset + 1
        -- u8
        local dataU8 = string.unpack(">I1", bytes, offset)
        offset = offset + 1
        -- u16
        local dataU16 = string.unpack(">I2", bytes, offset)
        offset = offset + 2
        -- u32
        local dataU32 = string.unpack(">I4", bytes, offset)
        offset = offset + 4
        -- u64
        local dataU64 = string.unpack(">I8", bytes, offset)
        offset = offset + 8
        -- i8
        local dataI8 = string.unpack(">i1", bytes, offset)
        offset = offset + 1
        -- i16
        local dataI16 = string.unpack(">i2", bytes, offset)
        offset = offset + 2
        -- i32
        local dataI32 = string.unpack(">i4", bytes, offset)
        offset = offset + 4
        -- i16
        local dataI64 = string.unpack(">i8", bytes, offset)
        offset = offset + 8
        -- f32
        local dataF32 = string.unpack(">f", bytes, offset)
        offset = offset + 4
        -- f64
        local dataF64 = string.unpack(">d", bytes, offset)
        offset = offset + 8
        -- str
        local dataStr = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(dataStr)
        -- bst
        local dataBst = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(dataBst)
        -- tuple
        -- tuple binary
        local dataTupleBinary = string.unpack("c6", bytes, offset)
        offset = offset + 6
        -- tuple tuple
        -- tuple tuple u8
        local dataTupleSubU8 = string.unpack(">I1", bytes, offset)
        offset = offset + 1
        -- tuple tuple str
        local dataTupleSubStr = string.unpack(">s2", bytes, offset)
        offset = offset + 2 + string.len(dataTupleSubStr)
        -- object
        local dataTupleSub = {u8 = dataTupleSubU8, str = dataTupleSubStr}
        -- tuple list
        local dataTupleList = {}
        local dataTupleListLength = string.unpack(">I2", bytes, offset)
        offset = offset + 2
        for dataTupleListIndex = 1, dataTupleListLength do
            -- 
            -- tuple list i16
            local dataTupleListDataI16 = string.unpack(">i2", bytes, offset)
            offset = offset + 2
            -- tuple list bst
            local dataTupleListDataBst = string.unpack(">s2", bytes, offset)
            offset = offset + 2 + string.len(dataTupleListDataBst)
            -- object
            local dataTupleListData = {i16 = dataTupleListDataI16, bst = dataTupleListDataBst}
            dataTupleList[dataTupleListIndex] = dataTupleListData
        end
        -- 
        local dataTupleSingle = {}
        local dataTupleSingleLength = string.unpack(">I2", bytes, offset)
        offset = offset + 2
        for dataTupleSingleIndex = 1, dataTupleSingleLength do
            -- bool
            local dataTupleSingleData = string.unpack(">I1", bytes, offset) ~= 0
            offset = offset + 1
            dataTupleSingle[dataTupleSingleIndex] = dataTupleSingleData
        end
        -- object
        local dataTuple = {binary = dataTupleBinary, sub = dataTupleSub, list = dataTupleList, single = dataTupleSingle}
        -- list
        local dataIndexList = {}
        local dataIndexListLength = string.unpack(">I2", bytes, offset)
        offset = offset + 2
        for dataIndexListIndex = 1, dataIndexListLength do
            -- 
            -- tuple binary
            local dataIndexListDataBinary = string.unpack("c6", bytes, offset)
            offset = offset + 6
            -- tuple tuple
            -- tuple tuple u8
            local dataIndexListDataSubU8 = string.unpack(">I1", bytes, offset)
            offset = offset + 1
            -- tuple tuple str
            local dataIndexListDataSubStr = string.unpack(">s2", bytes, offset)
            offset = offset + 2 + string.len(dataIndexListDataSubStr)
            -- object
            local dataIndexListDataSub = {u8 = dataIndexListDataSubU8, str = dataIndexListDataSubStr}
            -- tuple list
            local dataIndexListDataList = {}
            local dataIndexListDataListLength = string.unpack(">I2", bytes, offset)
            offset = offset + 2
            for dataIndexListDataListIndex = 1, dataIndexListDataListLength do
                -- 
                -- tuple list i16
                local dataIndexListDataListDataI16 = string.unpack(">i2", bytes, offset)
                offset = offset + 2
                -- tuple list bst
                local dataIndexListDataListDataBst = string.unpack(">s2", bytes, offset)
                offset = offset + 2 + string.len(dataIndexListDataListDataBst)
                -- object
                local dataIndexListDataListData = {i16 = dataIndexListDataListDataI16, bst = dataIndexListDataListDataBst}
                dataIndexListDataList[dataIndexListDataListIndex] = dataIndexListDataListData
            end
            -- 
            local dataIndexListDataSingle = {}
            local dataIndexListDataSingleLength = string.unpack(">I2", bytes, offset)
            offset = offset + 2
            for dataIndexListDataSingleIndex = 1, dataIndexListDataSingleLength do
                -- bool
                local dataIndexListDataSingleData = string.unpack(">I1", bytes, offset) ~= 0
                offset = offset + 1
                dataIndexListDataSingle[dataIndexListDataSingleIndex] = dataIndexListDataSingleData
            end
            -- object
            local dataIndexListData = {binary = dataIndexListDataBinary, sub = dataIndexListDataSub, list = dataIndexListDataList, single = dataIndexListDataSingle}
            dataIndexList[dataIndexListIndex] = dataIndexListData
        end
        -- 
        local dataKeyList = {}
        local dataKeyListLength = string.unpack(">I2", bytes, offset)
        offset = offset + 2
        for dataKeyListIndex = 1, dataKeyListLength do
            -- 
            -- binary
            local dataKeyListDataBinary = string.unpack("c6", bytes, offset)
            offset = offset + 6
            -- boolean
            local dataKeyListDataBoolean = string.unpack(">I1", bytes, offset) ~= 0
            offset = offset + 1
            -- u8
            local dataKeyListDataU8 = string.unpack(">I1", bytes, offset)
            offset = offset + 1
            -- u16
            local dataKeyListDataU16 = string.unpack(">I2", bytes, offset)
            offset = offset + 2
            -- u32
            local dataKeyListDataU32 = string.unpack(">I4", bytes, offset)
            offset = offset + 4
            -- u64
            local dataKeyListDataU64 = string.unpack(">I8", bytes, offset)
            offset = offset + 8
            -- i8
            local dataKeyListDataI8 = string.unpack(">i1", bytes, offset)
            offset = offset + 1
            -- i16
            local dataKeyListDataI16 = string.unpack(">i2", bytes, offset)
            offset = offset + 2
            -- i32
            local dataKeyListDataI32 = string.unpack(">i4", bytes, offset)
            offset = offset + 4
            -- i64
            local dataKeyListDataI64 = string.unpack(">i8", bytes, offset)
            offset = offset + 8
            -- f32
            local dataKeyListDataF32 = string.unpack(">f", bytes, offset)
            offset = offset + 4
            -- f64
            local dataKeyListDataF64 = string.unpack(">d", bytes, offset)
            offset = offset + 8
            -- str
            local dataKeyListDataStr = string.unpack(">s2", bytes, offset)
            offset = offset + 2 + string.len(dataKeyListDataStr)
            -- bst
            local dataKeyListDataBst = string.unpack(">s2", bytes, offset)
            offset = offset + 2 + string.len(dataKeyListDataBst)
            -- object
            local dataKeyListData = {binary = dataKeyListDataBinary, boolean = dataKeyListDataBoolean, u8 = dataKeyListDataU8, u16 = dataKeyListDataU16, u32 = dataKeyListDataU32, u64 = dataKeyListDataU64, i8 = dataKeyListDataI8, i16 = dataKeyListDataI16, i32 = dataKeyListDataI32, i64 = dataKeyListDataI64, f32 = dataKeyListDataF32, f64 = dataKeyListDataF64, str = dataKeyListDataStr, bst = dataKeyListDataBst}
            dataKeyList[dataKeyListDataU8] = dataKeyListData
        end
        -- object
        local data = {binary = dataBinary, boolean = dataBoolean, u8 = dataU8, u16 = dataU16, u32 = dataU32, u64 = dataU64, i8 = dataI8, i16 = dataI16, i32 = dataI32, i64 = dataI64, f32 = dataF32, f64 = dataF64, str = dataStr, bst = dataBst, tuple = dataTuple, indexList = dataIndexList, keyList = dataKeyList}
        return {protocol = 65535, data = data}
    else
        error(string.format('unknown protocol define: %d', protocol))
    end
end