import { WebSerial, SerialPortFilter } from "./webSerial";
import { DfuBootloader } from "./dfu";
import { sleep } from "./sleep";

export class BleMicroPro {
  private static bleMicroPro: BleMicroPro;
  private isBusy: boolean;
  protected port: WebSerial;
  protected filters: SerialPortFilter[];
  protected defaultBaudrate: number = 115200;

  protected constructor(port: WebSerial) {
    this.port = port;
    this.filters = null;
  }

  static getInstance(port: WebSerial) {
    if (!this.bleMicroPro) {
      this.bleMicroPro = new BleMicroPro(port);
    }

    return this.bleMicroPro;
  }

  private open = async (): Promise<boolean> => {
    if (!this.port.connected) {
      await this.port.open(
        () => {},
        this.defaultBaudrate,
        this.filters
          ? {
              filters: this.filters,
            }
          : null,
      );
      this.port.startReadLoop();
    }

    return this.port.connected;
  };

  private close = async () => {
    this.port.setReceiveCallback(() => {});
    await this.port.close();
  };

  public extractJson = (recv: string): string => {
    return recv.slice(recv.indexOf("{"), recv.lastIndexOf("}") + 1);
  };

  public extractMessage = (recv: string): string => {
    return recv.replace(/.*bmp@.*/gm, "").trim();
  };

  public sendCommand = async (
    command: string,
    timeoutMs: number = 500,
    extractor: (recv: string) => string = this.extractJson,
  ): Promise<string> => {
    const isOpen = await this.open();

    if (!isOpen) {
      return;
    }

    let recv = "";
    this.port.setReceiveCallback((msg) => {
      recv = recv.concat(new TextDecoder().decode(msg));
    });

    try {
      this.port.writeString(`\n${command}\n`);
      await sleep(timeoutMs);

      console.log(recv);
      recv = extractor(recv);
      console.log(recv);
    } catch (e) {
      console.error(e);
    } finally {
      this.port.setReceiveCallback(() => {});
    }

    return recv;
  };

  public getVersion = async (): Promise<string> => {
    const version = await this.sendCommand("version", 500, this.extractMessage);
    return version;
  };

  public startAdvertise = async (id?: number) => {
    if (id != null) {
      console.log(`adv ${id}`);
      await this.sendCommand(`adv ${id}`);
    } else {
      console.log(`adv`);
      await this.sendCommand(`adv`);
    }
  };

  public getBondingList = async (): Promise<any | null> => {
    const bonds = await this.sendCommand(`show`);
    try {
      return JSON.parse(bonds);
    } catch (e) {
      console.error(e);
      await this.close();
      return null;
    }
  };

  public deleteBonding = async (id: number) => {
    console.log(`del ${id}`);
    await this.sendCommand(`del ${id}`);
  };

  public isBmp = async (): Promise<boolean> => {
    try {
      const response = await this.sendCommand("", 500, (str) => str);

      if (response.includes("bmp@")) {
        return true;
      } else {
        return false;
      }
    } catch (e) {
      console.error(e);
      return false;
    }
  };

  private activateBootloader = async (): Promise<
    "dfuFound" | "dfuActivate" | "comError"
  > => {
    if (!this.port.connected) {
      try {
        await this.port.open(
          null,
          57600,
          this.filters != null
            ? {
                filters: this.filters.concat({
                  usbVendorId: 0x1915,
                  usbProductId: 0x521f,
                }),
              }
            : null,
        );
        this.port.startReadLoop();
      } catch (e) {
        console.error(e);
        await this.close();
        return "comError";
      }
    }

    if ((await this.isBmp()) == true) {
      try {
        console.log("send dfu wake up command");
        await this.port.writeString("\x03\ndfu\n\xc0");
        await this.close();
        return "dfuActivate";
      } catch (e) {
        console.error(e);
        await this.close();
        return "comError";
      }
    }

    const dfu = new DfuBootloader(this.port);
    let dfuFound = false;

    try {
      console.log("check dfu");
      dfuFound = await dfu.checkIntegrity();
    } catch (e) {
      console.error(e);
      dfuFound = false;
      await this.close();
    }

    if (dfuFound) {
      console.log("dfu found");
    }

    return "dfuFound";
  };

  public updateFirmware = async (
    firmware: Uint8Array,
    initPacket: Uint8Array,
    onProgress: (progress: number) => void,
  ): Promise<"Success" | "DfuActivate" | { error: string }> => {
    const blActivation = await this.activateBootloader();

    if (blActivation === "comError") {
      return { error: "Access to Invalid port" };
    } else if (blActivation === "dfuActivate") {
      return "DfuActivate";
    }

    const dfu = new DfuBootloader(this.port);

    try {
      await dfu.sendInitpacket(initPacket);
    } catch (e) {
      await this.close();
      console.error(e);
      return { error: `Failed to send init packet: ${e}` };
    }

    try {
      await dfu.sendFirmware(firmware, onProgress);
      await this.close();
      return "Success";
    } catch (e) {
      await this.close();
      console.error(e);
      return { error: `Failed to send firmware image: ${e}` };
    }
  };

  public tryLock = async (func: () => Promise<any>) => {
    const lock = this.lock();
    if (lock != null) {
      try {
        await func();
      } finally {
        this.release(lock);
      }
    }
  };

  public lock = (): any => {
    if (this.isBusy) {
      return null;
    } else {
      this.isBusy = true;
      return this;
    }
  };

  public release = (lock: any): boolean => {
    if (lock === this) {
      this.isBusy = false;
      return true;
    }
    return false;
  };
}
