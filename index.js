import { WebSerial } from "./src/webSerial";
import { DfuBootloader } from "./src/dfu";
import "bootstrap/dist/css/bootstrap.min.css";
import { keyboards } from "./src/keyboards";

const { Elm } = require("./src/App.elm");

const app = Elm.App.init({
  node: document.getElementById("main"),
  flags: {
    revision: process.env.REVISION,
    webSerialEnabled: navigator.serial ? true : false,
    keyboards: keyboards,
    bootloaders: [
      "ble_micro_pro_bootloader_0_11_2",
      "ble_micro_pro_bootloader_1_0_1_rc",
    ],
    applications: [
      "ble_micro_pro_default_0_11_3",
      "ble_micro_pro_vial_1_0_0_rc",
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

  let json;
  if (!setup.uploaded) {
    try {
      let configName = `config/${setup.keyboard}/${setup.keyboard}`;

      if (setup.layout != "") {
        configName += `_${setup.layout}`;
      }

      if (setup.useLpme) {
        configName += "_lpme_left_config.json";
      } else if (setup.isSplit) {
        if (setup.isLeft) {
          configName += "_master_left_config.json";
        } else {
          configName += "_slave_right_config.json";
        }
      } else {
        configName += "_config.json";
      }

      const file = await fetch(`${configName}`);

      if (!file.ok) {
        console.error("failed to load file");
        notifyUpdateError(e.message);
        return;
      }

      json = await file.json();

      if (setup.isSplit) {
        if (setup.useLpme) {
          json.config.mode = "SINGLE";
        } else {
          if (setup.isSlave) {
            json.config.mode = "SPLIT_SLAVE";
          } else {
            json.config.mode = "SPLIT_MASTER";
          }
        }
      } else {
        json.config.mode = "SINGLE";
      }

      json.config.matrix.debounce = setup.debounce;
      json.config.matrix.is_left_hand = setup.isLeft ? 1 : 0;

      json.config.peripheral.max_interval = setup.periphInterval;
      json.config.peripheral.min_interval = setup.periphInterval;
      json.config.peripheral.slave_latency = Math.floor(
        500 / setup.periphInterval
      );

      json.config.central.max_interval = setup.centralInterval;
      json.config.central.min_interval = setup.centralInterval;

      json.config.keymap.locale = setup.isJis ? "JP" : "US";

      if (!json.config.reserved) {
        json.config.reserved = Array(8).fill(0);
      }
      json.config.reserved[2] = Math.floor(setup.autoSleep / 10);
    } catch (e) {
      console.error(e);
      notifyUpdateError(e.message);
      return;
    }
  }

  if (serial.connected) {
    try {
      await serial.close();
    } catch (e) {}
  }

  serialReceivedStr = "";
  serial.setReceiveCallback((array) => {
    let receivedPacket = String.fromCharCode.apply(null, array);
    serialReceivedStr += receivedPacket;
  });

  try {
    await serial.open();
  } catch (e) {
    console.error(e);
    notifyUpdateError(e.message);
    return;
  }
  serial.startReadLoop();

  if (setup.uploaded) {
    // send uploaded config
    try {
      await sendConfig("config", 0, setup.uploaded);
      notifyUpdateProgress(100);
    } catch (e) {
      console.error(e);
      notifyUpdateError(e.message);
    } finally {
      await serial.close();
    }
    return;
  }

  try {
    await sendConfig("config", 0, JSON.stringify(json));
    notifyUpdateProgress(100);

    const filename =
      setup.layout == ""
        ? `config/${setup.keyboard}/${setup.keyboard}_encoder.json`
        : `config/${setup.keyboard}/${setup.keyboard}_${setup.layout}_encoder.json`;

    const file = await fetch(filename);

    if (file.ok) {
      let j = await file
        .json()
        .then((j) => {
          j = JSON.stringify(j);
          return j;
        })
        .catch(() => {
          console.log("no file");
          return null;
        });

      if (j) {
        console.log(j);
        await sendConfig("encoder", 5, j);
        notifyUpdateProgress(100);
      } else {
        await serial.writeString(`\nremove 5\n`);
      }
    }
  } catch (e) {
    console.error(e);
    notifyUpdateError(e.message);
  } finally {
    await serial.close();
    return;
  }
});
async function sendConfig(key, fileId, configString) {
  serialReceivedStr = "";
  await serial.writeString(`\x03file ${key}\n`);

  let configBytes = new TextEncoder().encode(configString);

  for (let index = 0; index < configBytes.length; index += 64) {
    await serial.write(configBytes.slice(index, index + 64));
    await sleep(30);
    notifyUpdateProgress(Math.floor((index / configBytes.length) * 100));
  }

  await serial.writeString("\0");
  await serial.writeString(`\nupdate ${fileId}\n`);
  await sleep(100);
  if (
    !serialReceivedStr.includes("Failed") &&
    serialReceivedStr.includes("Write succeed")
  ) {
    return true;
  } else {
    return Promise.reject(new Error("Failed to Update"));
  }
}
