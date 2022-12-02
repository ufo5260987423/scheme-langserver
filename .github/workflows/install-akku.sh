#!/bin/bash
set -e -o pipefail

wget https://gitlab.com/akkuscm/akku/uploads/819fd1f988c6af5e7df0dfa70aa3d3fe/akku-1.1.0.tar.gz
tar -zxvf akku-1.1.0.tar.gz
cd akku-1.1.0
./configure
make -j $(getconf _NPROCESSORS_ONLN)
make install
cd ..