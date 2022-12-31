find  ./tests ! -path "./tests/parallel-log-debug.sps" ! -path "./tests/log-debug.sps" -name "*sps" -exec scheme --script {} \; 
