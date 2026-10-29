#!/bin/sh
# Wrapper for scheme-langserver when launched by the MCP bridge.
# The bridge passes flags like --cache-path, --multi-thread, etc.; this
# wrapper only ensures we invoke scheme --script run.ss from the project
# root with the correct library paths.
# -a enable is appended by this project itself: the dogfood workspace wants
# automatic macro resolution on (it is opt-in since 2026-10-29, default off).
cd /home/ufo/Documents/workspace/scheme-langserver
. .akku/bin/activate
exec /etc/profiles/per-user/ufo/bin/scheme --script run.ss "$@" -a enable
