#!/bin/bash
set -e -o pipefail
git clone https://github.com/gwatt/chez-exe.git
cd chez-exe
scheme --script gen-config.ss --bootpath "../ChezScheme/$TARGET_MACHINE/boot/$TARGET_MACHINE"
make -j $(getconf _NPROCESSORS_ONLN)
sudo make install
cd ..