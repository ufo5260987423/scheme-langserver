#!/bin/sh
# Wrapper for scheme-langserver when launched by the MCP bridge.
# The bridge passes flags like --cache-path, --multi-thread, etc.; this
# wrapper only ensures we invoke scheme --script run.ss from the project
# root with the correct library paths.
cd /home/ufo/Documents/workspace/scheme-langserver
. .akku/bin/activate
exec /etc/profiles/per-user/ufo/bin/scheme --script run.ss "$@"
