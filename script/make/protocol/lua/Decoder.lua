require("./ProtocolRouter")

Decoder = {length = 0, buffer = {"", ""}}

--- This function returns `self`.
--- @param buffer string
--- @return self
function Decoder:appendData(buffer)
    self.buffer[2] = buffer or ""
    buffer = table.concat(self.buffer)
    self.buffer[1] = buffer
    self.buffer[2] = ""
    self.length = string.len(buffer)
    return self
end

--- This function returns `table|nil`.
--- @return table|nil packet
function Decoder:decode()
    -- @tag protocol data length 2 bytes(without header 4 byte), protocol 2 bytes
    if self.length >= 4 then
        local packetLength = string.unpack(">I2", self.buffer[1])
        if self.length >= 4 + packetLength then
            local protocol = string.unpack(">I2", self.buffer[1], 3)
            local packetData = string.sub(self.buffer[1], 5, 4 + packetLength)
            -- save
            self.buffer[1] = string.sub(self.buffer[1], 5 + packetLength)
            local data = ProtocolRouter.decode(1, protocol, packetData)
            return { protocol = protocol, data = data }
        end
    end
end
