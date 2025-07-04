#!/usr/bin/env bash

# akku install
# bash .akku/env
compile-chez-program run.ss --static

find .akku/libobj/srfi -name "*so" > srfi.so.txt
# echo './run.so' >> srfi.so.txt
sed -i 's/^./"./g' srfi.so.txt
sed -i 's/so$/so"/g' srfi.so.txt

echo '(make-boot-file "srfi.boot" (quote ("scheme" "petite"))' > build.ss
cat srfi.so.txt >>build.ss
echo ")" >> build.ss


cat build.ss | scheme -q

rm build.ss
rm srfi.so.txt