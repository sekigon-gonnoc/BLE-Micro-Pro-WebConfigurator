import { WebSerial } from "./webSerial";

const SOH = 0x01;
const STX = 0x02;
const ETX = 0x03;
const EOT = 0x04;
const ACK = 0x06;
const NAK = 0x15;
const CAN = 0x18;
const EOF = 0x1a;

class Xmodem {
  xmodem_state: "INIT" | "ACTIVE" | "COMPLETE" = "INIT";
  current_packet = 0;
  readonly payload_length = 128;

  constructor(
    private serial: WebSerial,
    private file: Uint8Array,
  ) {
    serial.setReceiveCallback(this.onReceivePacket.bind(this));
  }

  getProgress(): number {
    return Math.min(
      ((this.current_packet * this.payload_length) / this.file.length) * 100.0,
      100,
    );
  }

  private makePacket(): Uint8Array {
    console.log(`Create packet ${this.current_packet}`)
    const start = this.current_packet * this.payload_length;
    const end = (this.current_packet + 1) * this.payload_length;
    if (end <= this.file.length) {
      return Uint8Array.from([
        SOH,
        (this.current_packet + 1) & 0xff,
        (~(this.current_packet + 1)) & 0xff,
        ...this.file.slice(start, end),
        0,
        0,
      ]);
    } else {
      return Uint8Array.from([
        SOH,
        (this.current_packet + 1) & 0xff,
        (~this.current_packet + 1) & 0xff,
        ...this.file.slice(start, end),
        ...Array(end - this.file.length).fill(EOF),
        0,
        0,
      ]);
    }
  }

  private async onReceivePacket(msg: Uint8Array) {
    for (const d of msg) {
      console.log(d.toString(16));
      switch (d) {
        case "C".charCodeAt(0):
          if (this.xmodem_state === "INIT") {
            this.xmodem_state = "ACTIVE";
            await this.serial.write(this.makePacket());
          }
          return;
        case ACK:
          if (this.xmodem_state === "ACTIVE") {
            this.current_packet += 1;

            if (this.current_packet * this.payload_length > this.file.length) {
              this.xmodem_state = "COMPLETE";
              await this.serial.write(Uint8Array.from([EOT]));
            } else {
              await this.serial.write(this.makePacket());
            }
          } else if (this.xmodem_state === "COMPLETE") {
            await this.serial.close();
          }
          return;
        case NAK:
          if (this.xmodem_state === "ACTIVE") {
            await this.serial.write(this.makePacket());
          }
          return;
      }
    }
  }
}

export {Xmodem}