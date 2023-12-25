const fs = require("fs");

const fileList = fs.readdirSync("public/config");

let keyboard_list = {};

for (const file of fileList) {
  if (!file.includes("_config.bin")) continue;

  const keyboard = file.split("_").slice(0, -2).join("_");

  if (keyboard_list[keyboard] === undefined) {
    keyboard_list[keyboard] = {
      name: keyboard,
      layout: [""],
      keymap: [],
      firmware: "ble_micro_pro",
      split: false,
      lpme: false,
    };
  }

  if (file.includes("lpme_config.bin")) {
    keyboard_list[keyboard].lpme = true;
    keyboard_list[keyboard].split = true;
  } else if (file.includes("master_config.bin")) {
    keyboard_list[keyboard].split = true;
  }

  if (fileList.findIndex((k) => k === `${keyboard}_default.bin`) >= 0) {
    keyboard_list[keyboard].keymap = [`${keyboard}_default.bin`];
  }
}

fs.writeFileSync("src/keyboards.json", JSON.stringify(keyboard_list, null, 2));
