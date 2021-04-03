import { WebSerial } from "./webSerial";
import CRC32 from "crc-32";

const MAX_RAW_DATA: number = 1024;
const MAX_SLIP_DATA: number = 2051;

enum SlipDecodeState {
  Decoding,
  EscReceived,
  ClearingInvalidPacket,
}

class Slip {
  static readonly END: number = 0xc0;
  static readonly ESC: number = 0xdb;
  static readonly ESC_END: number = 0xdc;
  static readonly ESC_ESC: number = 0xdd;

  decode_state: SlipDecodeState;
  decoded_data: number[] = [];

  constructor() {
    this.decode_state = SlipDecodeState.Decoding;
  }

  encode(bytes: Uint8Array): Uint8Array {
    let encode_bytes = new Uint8Array(MAX_SLIP_DATA);
    let idx = 0;
    for (const byte of bytes) {
      if (byte == Slip.END) {
        encode_bytes[idx] = Slip.ESC;
        encode_bytes[idx + 1] = Slip.ESC_END;
        idx += 2;
      } else if (byte == Slip.ESC) {
        encode_bytes[idx] = Slip.ESC;
        encode_bytes[idx + 1] = Slip.ESC_ESC;
        idx += 2;
      } else {
        encode_bytes[idx] = byte;
        idx += 1;
      }
    }
    encode_bytes[idx] = Slip.END;
    idx += 1;

    return encode_bytes.slice(0, idx);
  }

  decode_byte(byte: number): [boolean, SlipDecodeState, number[]] {
    let finished: boolean = false;
    if (byte > 0xff || byte < 0) {
      console.log("invalid argument");
      return [false, this.decode_state, this.decoded_data];
    }

    switch (this.decode_state) {
      case SlipDecodeState.Decoding:
        if (byte == Slip.END) {
          finished = true;
        } else if (byte == Slip.ESC) {
          this.decode_state = SlipDecodeState.EscReceived;
        } else {
          this.decoded_data.push(byte);
        }
        break;
      case SlipDecodeState.EscReceived:
        if (byte == Slip.ESC_END) {
          this.decoded_data.push(Slip.END);
          this.decode_state = SlipDecodeState.Decoding;
        } else if (byte == Slip.ESC_ESC) {
          this.decoded_data.push(Slip.ESC);
          this.decode_state = SlipDecodeState.Decoding;
        } else {
          this.decode_state = SlipDecodeState.ClearingInvalidPacket;
        }
        break;
      case SlipDecodeState.ClearingInvalidPacket:
        if (byte == Slip.END) {
          this.decode_state = SlipDecodeState.Decoding;
        }
        break;
    }

    if (finished) {
      let res = this.decoded_data.slice();
      this.decoded_data = [];
      return [finished, this.decode_state, res];
    }

    return [finished, this.decode_state, this.decoded_data];
  }
}

enum OPCODE {
  VERSION = 0x00,
  CREATE = 0x01,
  SRN = 0x02,
  CRC = 0x03,
  EXECUTE = 0x04,
  SELECT = 0x06,
  MTU = 0x07,
  WRITE = 0x08,
  PING = 0x09,
  HW_VER = 0x0a,
  SW_VER = 0x0b,
  ABORT = 0x0c,
  RESPONSE = 0x60,
}

enum RES_CODE {
  INVALID_CODE = 0x00,
  SUCCESS = 0x01,
  NOTSUPPORTED = 0x02,
  INVALID_PARAMETER = 0x03,
  INSUFFICIENT_RESOURCES = 0x04,
  INVALID_OBJECT = 0x05,
  INVALID_SIGNATURE = 0x06,
  UNSUPPORTED_TYPE = 0x07,
  OPERATION_NOTPERMITTED = 0x08,
  OPERATION_FAILED = 0x0a,
  EXTENDED_ERROR = 0x0b,
}
type ObjectResponse = { max_size: number; offset: number; crc: number };

class DfuBootloader {
  slip = new Slip();

  private received: boolean = false;
  private sending_opcode: OPCODE | null = null;
  private command_result: { success: boolean; data: number[] } = {
    success: false,
    data: [],
  };

  private sleep(ms: number) {
    return new Promise((resolve: any) => setTimeout(resolve, ms));
  }

  async waitReceive(timeout: number = 500) {
    while (this.received == false && timeout > 0) {
      await this.sleep(1);
      timeout--;
    }

    if (timeout == 0) {
      throw new Error("DFU command timeout");
    }

    if (this.received == true) {
      if (this.command_result.success) {
        this.received = false;
      } else {
        this.received = false;
        throw new Error("Failed to receive command response.");
      }
    }
  }

  async getProtocolVersion(): Promise<number> {
    this.sending_opcode = OPCODE.VERSION;
    await this.send(new Uint8Array([this.sending_opcode]));
    await this.waitReceive();

    if (this.command_result) {
      return this.command_result.data[0];
    } else {
      console.error("Get Protocol Ver:Failed");
      return Promise.reject();
    }
  }

  async setPacketReceiptNotification(prn: number) {
    this.sending_opcode = OPCODE.SRN;
    await this.send(new Uint8Array([this.sending_opcode, prn]));
    await this.waitReceive();

    if (!this.command_result.success) {
      return Promise.reject("Failed to set Packet Receipt Notification.");
    }
  }

  async getMtu(): Promise<number> {
    this.sending_opcode = OPCODE.MTU;
    await this.send(new Uint8Array([this.sending_opcode]));
    await this.waitReceive();

    let data = new Uint8Array(this.command_result.data);
    let u16data = new Uint16Array(data.buffer);

    if (this.command_result.success) {
      return u16data[0];
    } else {
      console.error("Get mtu:Failed");
      return Promise.reject(new Error("Failed to get MTU"));
    }
  }

  private async createObject(type: 1 | 2, size: number) {
    this.sending_opcode = OPCODE.CREATE;

    await this.send(
      new Uint8Array([
        this.sending_opcode,
        type,
        size & 0xff,
        (size >> 8) & 0xff,
        (size >> 16) & 0xff,
        (size >> 24) & 0xff,
      ])
    );
    await this.waitReceive();

    if (!this.command_result.success) {
      return Promise.reject(new Error("Failed to create Object"));
    }
  }
  private async createCommand(size: number) {
    return this.createObject(1, size);
  }
  private async createData(size: number) {
    return this.createObject(2, size);
  }
  private async selectObject(type: 1 | 2): Promise<ObjectResponse> {
    this.sending_opcode = OPCODE.SELECT;

    await this.send(new Uint8Array([this.sending_opcode, type]));
    await this.waitReceive();

    let data = new Uint8Array(this.command_result.data);
    let i32data = new Int32Array(data.buffer);

    return new Promise((resolve, reject) => {
      if (this.command_result.success) {
        let response: ObjectResponse = {
          max_size: i32data[0],
          offset: i32data[1],
          crc: i32data[2],
        };
        console.log("select object:", response);
        resolve(response);
      } else {
        reject();
      }
    });
  }

  private async selectCommand(): Promise<ObjectResponse> {
    return this.selectObject(1);
  }

  private async selectData(): Promise<ObjectResponse> {
    return this.selectObject(2);
  }

  private async requestChecksum(): Promise<ObjectResponse | null> {
    this.sending_opcode = OPCODE.CRC;

    await this.send(new Uint8Array([this.sending_opcode]));
    await this.waitReceive();

    if (this.command_result.success) {
      let data = new Uint8Array(this.command_result.data);
      let i32data = new Int32Array(data.buffer);

      return new Promise((resolve) => {
        resolve({ max_size: 0, offset: i32data[0], crc: i32data[1] });
      });
    } else {
      return new Promise((resolve) => {
        resolve(null);
      });
    }
  }

  private async requestExecute(): Promise<void> {
    this.sending_opcode = OPCODE.EXECUTE;

    await this.send(new Uint8Array([this.sending_opcode]));
    await this.waitReceive();

    return new Promise((resolve, reject) => {
      if (this.command_result.success) {
        resolve();
      } else {
        reject();
      }
    });
  }

  private async streamData(
    data: Uint8Array,
    crc: number,
    offset: number
  ): Promise<number> {
    let idx = 0;
    let len = data.length;
    let transmit_data: Uint8Array;

    for (idx = 0; idx < len; idx += MAX_RAW_DATA) {
      transmit_data = data.slice(idx, idx + MAX_RAW_DATA);
      let send_packet = new Uint8Array(1 + transmit_data.length);
      this.sending_opcode = OPCODE.WRITE;
      send_packet.set([OPCODE.WRITE]);
      send_packet.set(transmit_data, 1);
      await this.send(send_packet);

      crc = CRC32.buf(transmit_data, crc);
      offset += transmit_data.length;

      // await this.waitReceive();
    }

    let crc_response = await this.requestChecksum();
    if (!crc_response) {
      return Promise.reject(new Error("Failed to get checksum."));
    }
    console.log("crc_response", crc_response);

    if (crc != crc_response.crc) {
      console.error(
        `CRC Error: Expect: 0x${crc.toString(
          16
        )}, Received: 0x${crc_response.crc.toString(16)}`
      );
      return Promise.reject(new Error("CRC Error."));
    }

    if (offset != crc_response.offset) {
      return Promise.reject(new Error("Offset error. Packet lossed."));
    }

    return crc;
  }

  private decodeReceivePacket(msg: Uint8Array): [boolean, number[] | null] {
    let decoded: number[];
    let done: boolean = false;
    let decode_state: SlipDecodeState = SlipDecodeState.Decoding;
    for (const byte of msg) {
      [done, decode_state, decoded] = this.slip.decode_byte(byte);
      if (done) {
        console.log("SLIP packet decoded");
        return [true, decoded];
      }

      if (decode_state == SlipDecodeState.ClearingInvalidPacket) {
        console.log("Invalid SLIP packet");
        return [true, null];
      }
    }

    return [false, null];
  }

  private onReceivePacket(msg: Uint8Array) {
    let [done, decoded] = this.decodeReceivePacket(msg);

    if (done) {
      if (decoded) {
        this.messageParaser(decoded.slice());
        this.received = true;
      } else {
        // invalid packet
      }
    }
  }

  private messageParaser(msg: number[]) {
    this.command_result.success = false;
    this.command_result.data = [];

    if (msg[0] != OPCODE.RESPONSE) {
      // error
      console.error("invalid response: not response packet");
    }

    if (msg[1] != this.sending_opcode) {
      // error
      console.error(
        `invalid response: different opcode. expect ${this.sending_opcode}, received ${msg[1]}`
      );
    }

    if (msg[2] == RES_CODE.SUCCESS) {
      console.log("dfu command successed", this.sending_opcode);
      this.command_result.success = true;
      this.command_result.data = msg.slice(3);
    } else if (msg[2] == RES_CODE.EXTENDED_ERROR) {
      console.error("extend error:", msg[3]);
    } else {
      // error
      console.error("invalid response: oeperation failed:", msg[2]);
    }
  }

  constructor(private serial: WebSerial) {
    serial.setReceiveCallback(this.onReceivePacket.bind(this));
  }

  async checkIntegrity(): Promise<boolean> {
    await this.setPacketReceiptNotification(0);
    let mtu = await this.getMtu();
    if (mtu != MAX_SLIP_DATA) {
      console.error(`Wrong MTU:${mtu} Expected:${MAX_SLIP_DATA}`);
      return false;
    }
    return true;
  }

  async send(msg: Uint8Array) {
    let packet = this.slip.encode(msg);
    console.log(`serial send ${packet.byteLength}byte`);
    if (!this.serial.connected) {
      return Promise.reject(new Error("Serial port is not opend."));
    }
    await this.serial.write(packet);
  }

  async sendInitpacket(packet: Uint8Array) {
    let tryToResume = async (response: ObjectResponse) => {
      console.log("try to resume init packet");
      if (response.offset == 0 || response.offset > packet.length) {
        console.log("Nothing to resume");
        return false;
      }

      let expexted_crc = CRC32.buf(packet.slice(0, response.offset));

      if (expexted_crc != response.crc) {
        console.log("invalid crc");
        return false;
      }

      if (packet.length > response.offset) {
        console.log("resume init packet");
        await this.streamData(
          packet.slice(response.offset),
          expexted_crc,
          response.offset
        );
      }

      await this.requestExecute();

      return true;
    };

    let response: ObjectResponse = Object.assign(
      {},
      await this.selectCommand()
    );
    console.log("Select Command:", response);

    if (!response) {
      return Promise.reject(new Error("Invalid response."));
    }

    if (response.max_size < packet.length) {
      return Promise.reject(new Error("Too long init packet."));
    }

    if (response && (await tryToResume(response))) {
      // scccess
      return;
    }

    console.log("send intial packet from head");
    await this.createCommand(packet.length);
    await this.streamData(packet, 0, 0);
    await this.requestExecute();
  }

  async sendFirmware(
    firm: Uint8Array,
    onProgress?: (progress: number) => void
  ) {
    let tryToResume = async (response: ObjectResponse) => {
      console.log("try to resume firmware packet");
      if (response.offset == 0) {
        console.log("Nothing to resume");
        return false;
      }

      let expexted_crc = CRC32.buf(firm.slice(0, response.offset));
      let emptyBytesInPage = response.offset % response.max_size;

      if (expexted_crc != response.crc) {
        console.log("invalid crc");
        response.offset -= emptyBytesInPage
          ? emptyBytesInPage
          : response.max_size;
        response.crc = CRC32.buf(firm.slice(0, response.offset));
        return false;
      }

      if (emptyBytesInPage != 0 && response.offset != firm.length) {
        console.log("resume firmware image");
        console.log("send a chunk of firmware image");

        let transmit_data = firm.slice(
          response.offset,
          response.offset + response.max_size - emptyBytesInPage
        );
        response.crc = await this.streamData(
          transmit_data,
          response.crc,
          response.offset
        );
      }

      await this.requestExecute();

      return false;
    };

    let response = Object.assign({}, await this.selectData());

    if (!response) {
      return Promise.reject(new Error("Invalid response."));
    }

    console.log("Select Data:", response);

    // if (response.max_size > MAX_RAW_DATA) {
    //   Promise.reject(
    //     new Error(`Invalid Max data size:${response.max_size}>${MAX_RAW_DATA}`)
    //   );
    // }

    // send first chunk
    await tryToResume(response);

    let idx: number = response.offset;
    let len: number = firm.length;
    console.log(`write ${len} bytes, strat from ${idx}`);
    for (idx; idx < len; idx += response.max_size) {
      console.log(
        `send a chunk of firmware image [${idx}:${
          idx + response.max_size
        }]/${len} => ${Math.floor((idx / len) * 100)}%`
      );
      let transmit_data = firm.slice(idx, idx + response.max_size);

      await this.createData(transmit_data.length);
      response.crc = await this.streamData(transmit_data, response.crc, idx);
      await this.requestExecute();
      if (onProgress) {
        onProgress(Math.floor((idx / len) * 100));
      }
    }
    if (onProgress) {
      onProgress(Math.floor((idx / len) * 100));
    }
  }
}

// let dfu = new DfuBootloader();

// window.getBootloaderVersion = function getBootloaderVersion() {
//     dfu.getVersion();
// };

// async function loadFirmwareImage(firmName) {
//     let file = await fetch(firmName)
//         .then((response) => {
//             return response.arrayBuffer();
//         })
//         .then((array) => {
//             console.log(array);
//             return array;
//         });
//
//     return file;
// }

export { DfuBootloader };
