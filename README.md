# BLE-Micro-Pro-WebConfigurator

## Build
```
yarn install
yarn start
```

## Add your keyboard
Put [your keyborad config](https://github.com/sekigon-gonnoc/BLE-Micro-Pro/blob/master/AboutDefaultFirmware/doc/define_new_keyboard.md#%E3%82%B3%E3%83%B3%E3%83%95%E3%82%A3%E3%82%B0%E3%83%95%E3%82%A1%E3%82%A4%E3%83%AB%E3%81%AE%E7%94%A8%E6%84%8F) files to `static/config/your_keyboad/`, then
```
python ./makeKeyboardList.py
yarn fmt
```