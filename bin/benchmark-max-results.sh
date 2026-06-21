#!/usr/bin/env bash
# Benchmark type inference with different PRIVATE-MAX-RESULTS thresholds.
# Usage: source .akku/bin/activate && bash bin/benchmark-max-results.sh

set -e

LIBRARIES=(
  "(scheme-langserver util contain)"
  "(scheme-langserver util json)"
  "(scheme-langserver virtual-file-system file-node)"
  "(scheme-langserver analysis identifier reference)"
  "(scheme-langserver analysis type domain-specific-language interpreter)"
  "(scheme-langserver util binary-search)"
)

THRESHOLDS=(100 200 500 1000)
TIMEOUT_SECONDS=600
OUTPUT_DIR="/tmp/type-analysis-benchmark"
mkdir -p "$OUTPUT_DIR"

run_for_threshold() {
  local threshold="$1"
  echo "=============================================="
  echo "PRIVATE-MAX-RESULTS = $threshold"
  echo "=============================================="

  # Patch the source file
  sed -i "s/(define PRIVATE-MAX-RESULTS [0-9]*)/(define PRIVATE-MAX-RESULTS $threshold)/" \
    analysis/type/domain-specific-language/interpreter.sls

  # Clear compiled cache
  rm -rf .akku/libobj/scheme-langserver

  for lib in "${LIBRARIES[@]}"; do
    local safe_name
    safe_name=$(echo "$lib" | tr -d '()' | tr ' ' '-')
    local out_file="$OUTPUT_DIR/${safe_name}-${threshold}.txt"

    echo "--- $lib ---"
    local t0 t1 elapsed status
    t0=$(date +%s.%N)
    if timeout "$TIMEOUT_SECONDS" scheme --script bin/output-type-analysis.ss . "$lib" "$out_file" >/dev/null 2>&1; then
      status="ok"
    else
      status="timeout-or-error"
    fi
    t1=$(date +%s.%N)
    elapsed=$(awk "BEGIN {printf \"%.2f\", $t1 - $t0}")

    local lines=0
    if [ -f "$out_file" ]; then
      lines=$(wc -l < "$out_file")
    fi
    echo "status: $status  elapsed: ${elapsed}s  output lines: $lines"
  done
}

# Save original value
ORIGINAL=$(grep -oP '(?<=PRIVATE-MAX-RESULTS )[0-9]+' analysis/type/domain-specific-language/interpreter.sls)
echo "Original PRIVATE-MAX-RESULTS: $ORIGINAL"

# Run benchmarks
for t in "${THRESHOLDS[@]}"; do
  run_for_threshold "$t"
done

# Restore original
sed -i "s/(define PRIVATE-MAX-RESULTS [0-9]*)/(define PRIVATE-MAX-RESULTS $ORIGINAL)/" \
  analysis/type/domain-specific-language/interpreter.sls
echo "Restored PRIVATE-MAX-RESULTS to $ORIGINAL"

echo "Benchmark complete. Results in $OUTPUT_DIR"
