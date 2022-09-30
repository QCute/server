require("./ProtocolRouter")

Decoder = {buffer = {"", ""}}

--- This function returns `table`.
--- @param buffer string
--- @return table
function Decoder:decode(buffer)
    self.buffer[2] = buffer or ""
    local data = table.concat(self.buffer)
    self.buffer[1] = data
    self.buffer[2] = ""
    local length = string.len(data)
    -- @tag protocol content length 2 bytes(without header 4 byte), protocol 2 bytes
    if length >= 4 then
        local packageLength = string.unpack(">I2", data)
        if length >= 4 + packageLength then
            local protocol = string.unpack(">I2", data, 3)
            local packageData = string.sub(data, 5, 4 + packageLength)
            -- save
            self.buffer[1] = string.sub(data, 5 + packageLength)
            local content = decodeProtocol(1, protocol, packageData)
            return { protocol = protocol, content = content }
        end
    end
end
