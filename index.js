import { WebSerial } from "./src/webSerial";
import { DfuBootloader } from "./src/dfu";
import { Xmodem } from "./src/xmodem";
import "bootstrap/dist/css/bootstrap.min.css";
import { keyboards } from "./src/keyboards";
import { Elm } from "./src/App.elm";

const app = Elm.App.init({
  node: document.getElementById("main"),
  flags: {
    revision: import.meta.env.VITE_REVISION,
    webSerialEnabled: navigator.serial ? true : false,
    keyboards: keyboards,
    bootloaders: [
      "ble_micro_pro_bootloader_0_11_2",
      "ble_micro_pro_bootloader_1_0_0_rc",
    ],
    applications: [
      "ble_micro_pro_default_0_11_3",
      "ble_micro_pro_vial_1_0_1_rc",
      "ble_micro_pro_safemode_0_11_3",
      "crkbd_ecwl_bmp_default_0_11_2",
      "kugel_default_0_11_2",
      "toybox_bmp_default_0_11_2",
    ],
  },
});

const serial = new WebSerial(128, 5);
let serialReceivedStr = "";

function sleep(ms) {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

function notifyUpdateResult(progress, message) {
  app.ports.updateResult.send({ progress: progress, message: message });
}

function notifyUpdateProgress(progress) {
  notifyUpdateResult(progress, "");
}

function notifyBootloaderWakeup() {
  notifyUpdateResult(-1, "");
}

function notifyUpdateError(message) {
  notifyUpdateResult(-2, message);
}

app.ports.updateFirmware.subscribe(async (command) => {
  const dfu = new DfuBootloader(serial);
  console.log(command);

  if (serial.connected) {
    try {
      console.log("close previous port");
      await serial.close();
    } catch (e) {}
  }

  try {
    await serial.open();
  } catch (e) {
    console.error(e);
    notifyUpdateError(e.message);
    return;
  }
  serial.startReadLoop();

  let firmName = `${command.type}/${command.name}`;

  if (command.type == "application" && command.disableMsc == true) {
    firmName = firmName.replace("default", "no_msc");
  } else if (command.type == "bootloader" && command.disableMsc == true) {
    firmName = firmName + "_no_msc";
  }

  const dat = await fetch(`${firmName}.dat`);
  const bin = await fetch(`${firmName}.bin`);

  if (!(dat.ok && bin.ok)) {
    console.error("failed to load file");
    notifyUpdateError(`File ${firmName} not found.`);
    return;
  }

  console.log("target firmware is found");

  let is_dfu = false;
  try {
    console.log("send dfu wake up command");
    await serial.writeString("\x03\ndfu\n\xc0");
  } catch (e) {
    console.error(e);

    notifyUpdateError(e.message);

    return;
  }

  await sleep(100);

  try {
    console.log("check dfu");
    is_dfu = await dfu.checkIntegrity();
    if (!is_dfu) {
      console.error("dfu not found");
      return;
    }
  } catch (e) {
    console.error(e);
    notifyBootloaderWakeup();
    return;
  }

  console.log("dfu found");

  try {
    const initPacket = await dat.arrayBuffer();
    await dfu.sendInitpacket(new Uint8Array(initPacket));

    const firmImage = await bin.arrayBuffer();
    await dfu.sendFirmware(new Uint8Array(firmImage), (progress) => {
      notifyUpdateProgress(progress);
    });
  } catch (e) {
    console.error(e);
    notifyUpdateError(e.message);

    return;
  }

  console.log("update completed");
  await serial.close();
});

app.ports.updateConfig.subscribe(async (setup) => {
  console.log(setup);

  if (!setup.keyboard) {
    loadUserFile(".bin", async (fileBuffer) => {
      if (
        fileBuffer[0] != 0xae ||
        fileBuffer[1] != 0xfa ||
        fileBuffer[2] != 0x5a ||
        fileBuffer[3] != 0xb0
      ) {
        console.log("File header does not match");
        notifyUpdateError(`Invalid config file. `);
        return;
      }
      assignSetup(fileBuffer, setup);
      await transferFileByXmodem(fileBuffer);
    });
  } else {
    const fileName = setup.isSplit
      ? setup.isSlave
        ? setup.useLpme
          ? `config/${setup.keyboard}_lpme_config.bin`
          : `config/${setup.keyboard}_master_config.bin`
        : `config/${setup.keyboard}_slave_config.bin`
      : `config/${setup.keyboard}_single_config.bin`;

    const file = await fetch(fileName);
    if (file.ok) {
      const fileBuffer = new Uint8Array(await file.arrayBuffer());
      assignSetup(fileBuffer, setup);
      console.log(fileBuffer);
      // await transferFileByXmodem(fileBuffer);
    } else {
      notifyUpdateError(`${file.status} ${file.statusText}. `);
    }
  }
});

function assignSetup(fileBuffer, setup) {
  fileBuffer.set([setup.debounce], 3913);
  fileBuffer.set([setup.periphInterval], 3982);
  fileBuffer.set([setup.periphInterval], 3984);
  fileBuffer.set([setup.centralInterval], 3988);
  fileBuffer.set([setup.centralInterval], 3990);
  fileBuffer.set([setup.autoSleep], 4022);
}

app.ports.updateEeprom.subscribe(async (setup) => {
  if (!setup.keyboard) {
    loadUserFile(".bin", async (fileBuffer) => {
      if (fileBuffer[0] != 0xe6 || fileBuffer[1] != 0xfe) {
        console.log("file header does not match");
        notifyUpdateError(`Invalid eeprom file. `);
        return;
      }
      await transferFileByXmodem(fileBuffer);
    });
  } else {
    const fileBuffer = new Uint8Array(
      await fetch(`${setup.keyboadrd}_default.bin`).then((res) =>
        res.arrayBuffer(),
      ),
    );
    await transferFileByXmodem(fileBuffer);
  }
});

async function loadUserFile(extension, callback) {
  const input = document.createElement("input");
  input.type = "file";
  input.accept = extension;
  input.addEventListener("change", () => {
    const file = input.files?.[0];
    if (file == null) return;
    const reader = new FileReader();

    reader.onload = async () => {
      const fileBuffer = new Uint8Array(await file.arrayBuffer());
      await callback(fileBuffer);
    };

    reader.readAsArrayBuffer(file);
  });
  input.click();
  input.remove();
}

async function transferFileByXmodem(data) {
  try {
    await serial.open();
    serial.startReadLoop();
  } catch (e) {
    console.error(e);
    notifyUpdateError(e.message);
    return;
  }

  let progress = 0;
  notifyUpdateProgress(0);

  await serial.writeString("xmodem\n");

  const xmodem = new Xmodem(serial, data);

  while (xmodem.getProgress() < 100.0) {
    await sleep(30);
    if (progress != Math.floor(xmodem.getProgress())) {
      progress = Math.floor(xmodem.getProgress());
      notifyUpdateProgress(progress);
    }
  }
}
