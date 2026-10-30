#!/bin/sh
# Restore the ./run wrapper after compile-chez-program overwrote it with the
# compiled binary. The magic-scheme VSCode extension launches <workspace>/run;
# the compiled binary currently cannot runtime-resolve speculative macro-derived
# references (see AGENTS.md known issues), so ./run must stay a wrapper that
# runs the server in script mode (scheme --script run.ss), which is verified
# working end to end.
#
# Usage (after `compile-chez-program --full-chez run.ss`):
#   sh restore-run-wrapper.sh
# The freshly compiled binary is preserved as run.bin (gitignored).

set -e
cd "$(dirname "$0")"

if [ -f run ] && [ ! -L run ] && head -c 4 run 2>/dev/null | grep -q 'MZ\|ELF'; then
  mv -f run run.bin
  echo "compiled binary preserved as run.bin"
elif [ -f run.bin ]; then
  echo "run.bin already present; replacing ./run with the wrapper"
else
  echo "warning: no compiled binary found at ./run or ./run.bin; creating the wrapper anyway"
fi

cat > run <<'EOF'
#!/bin/sh
# Workspace launcher preferred by the magic-scheme VSCode extension (it looks
# for <workspace>/run first). The compiled binary (run.bin) has a known issue
# (docs: compiled binaries create but cannot runtime-resolve speculative
# macro-derived references), so this wrapper runs the server in script mode,
# which is fully verified.
cd "$(dirname "$0")"
. .akku/bin/activate
exec scheme --script run.ss "$@"
EOF
chmod +x run
echo "./run wrapper restored (script mode; VSCode-ready)"
