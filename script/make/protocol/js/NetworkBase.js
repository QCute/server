import Encoder from "../Encoder.js";
import Decoder from "../Decoder.js";

export default class NetworkBase {

    /** @var {Encoder} */
    encoder = new Encoder();

    /** @var {Decoder} */
    decoder = new Decoder();

    constructor() {
    }

    sendPacket(request) {
        this.socket.send(this.encoder.encode(request.protocol, request));
    }

    /** 
     * Handle socket message
     * @param {ArrayBuffer} data 
     */
    onMessage(data) {
        this.decoder.appendData(data);
        do {
            const packet = this.decoder.decode();
            if (!packet) break;
            this.handle(packet.protocol, packet.data)
        } while(true)
    }

    /**
     * Handle network packet.
     * 
     * @param {number} protocol
     * @param {object} data
     */
    handle(protocol, data) {
        switch(protocol) {
            case 65535: this.test.handleAccountQuery(data);
        }
    }
}


class Network extends NetworkBase {
    test: Test;

    
}

class TestRequest {
    id;
    name;
}

class TestResponse {

}



-module(account_handler).
-export([handle/3]).

handle(State, 10000, []) ->
    account:heartbeat(State);

handle(State, 10001, [ServerId, AccountName]) ->
    account:query(State, ServerId, AccountName);

handle(State, 10002, [RoleName, ServerId, AccountName, Sex, Classes, Channel, DeviceId, Mac, DeviceType]) ->
    account:create(State, RoleName, ServerId, AccountName, Sex, Classes, Channel, DeviceId, Mac, DeviceType);

handle(State, 10003, [RoleId, RoleName, ServerId, AccountName]) ->
    account:login(State, RoleId, RoleName, ServerId, AccountName);

handle(State, 10004, []) ->
    account:logout(State);

handle(State, Protocol, Data) ->
    account:handle_packet(State, Protocol, Data).

