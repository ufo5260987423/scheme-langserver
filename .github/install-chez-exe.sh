#!/bin/bash
set -e -o pipefail
cd chez-exe
scheme --script gen-config.ss --bootpath "../ChezScheme/$TARGET_MACHINE/boot/$TARGET_MACHINE"
make -j $(getconf _NPROCESSORS_ONLN)
sudo make install
cd ..