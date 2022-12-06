#!/bin/bash
set -e -o pipefail
bash .akku/env
compile-chez-program run.ss
