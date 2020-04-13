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
    bootloaders: ["ble_micro_pro_bootloader_0_5_0"],
    applications: ["ble_micro_pro_default_0_5_0", "kugel_default_0_5_0"]
  }
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
    await serial.writeString("\x03\ndfu\n\xc0");
  } catch (e) {
    console.error(e);

    notifyUpdateError(e.message);

    return;
  }

  await sleep(100);

  try {
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
      await sendConfig(setup.uploaded);
      notifyUpdateProgress(100);
    } catch (e) {
      console.error(e);
      notifyUpdateError(e.message);
    }
    return;
  }

  try {
    await sendConfig(JSON.stringify(json));
    notifyUpdateProgress(100);
  } catch (e) {
    console.error(e);
    notifyUpdateError(e.message);

    return;
  }
});
async function sendConfig(configString) {
  await serial.writeString("\x03file config\n");

  for (let index = 0; index < configString.length; index += 64) {
    await serial.writeString(configString.slice(index, index + 64));
    await sleep(30);
    notifyUpdateProgress(Math.floor((index / configString.length) * 100));
  }

  await serial.writeString("\0");
  await serial.writeString("\nupdate 0\n");
  await sleep(100);
  if (
    !serialReceivedStr.includes("Failed") &&
    serialReceivedStr.includes("Write succeed")
  ) {
    return true;
  } else {
    return Promis.reject(new Error("Failed to Update"));
  }
}
