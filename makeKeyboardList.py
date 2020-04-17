#!/usr/bin/env python
# -*- coding:utf-8 -*-

import os
import re

CONFIG_DIR = './static/config'


def isExists(path):
    if not os.path.exists(path):
        print(f'{path} does not exist')


with open('./src/keyboards.js', mode='w') as f:
    f.write('export {keyboards};')
    f.write('let keyboards=[')
    for directory in os.listdir(CONFIG_DIR):
        layouts = set()
        if os.path.isdir(f'{CONFIG_DIR}/{directory}'):
            configs = os.listdir(f'{CONFIG_DIR}/{directory}')

            isSplit = any(map(lambda k: "master" in k, configs))
            isLpmeAvailable = any(map(lambda k: "lpme" in k, configs))

            for config in configs:
                config = config.replace(f'{directory}', '')
                layout = re.search(
                    r'^.*?(?=(_config|_master|_slave|_lpme))', config, flags=re.IGNORECASE)
                if layout != None:
                    layouts.add(layout.group(0).lstrip('_'))

            # print(f'{directory}:{isSplit}:{isLpmeAvailable}:{layouts}')
            if len(layouts) == 0:
                raise Exception(f"wrong file names in {directory}")

            for layout in sorted(layouts):
                if layout != '':
                    layout = '_' + layout

                if isSplit:
                    isExists(
                        f'{CONFIG_DIR}/{directory}/{directory}{layout}_master_left_config.json')
                    isExists(
                        f'{CONFIG_DIR}/{directory}/{directory}{layout}_slave_right_config.json')

                    if isLpmeAvailable:
                        isExists(
                            f'{CONFIG_DIR}/{directory}/{directory}{layout}_lpme_left_config.json')
                else:
                    isExists(
                        f'{CONFIG_DIR}/{directory}/{directory}{layout}_config.json')

            f.write(
                f'{{name:\'{directory}\',layout:{sorted(list(layouts))},keymap:[],split:{str(isSplit).lower()},lpme:{str(isLpmeAvailable).lower()}}},')

    f.write(']')
