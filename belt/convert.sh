set -e
for i in src/*.ml; do
COMPILE_TO_NATIVE=true ../bsb_modules/bs-platform/lib/bspp.exe $i | ../bsb_modules/bs-platform/lib/bsrefmt --print binary > $i.bin
COMPILE_TO_NATIVE=1 ../bsb_modules/bs-platform/lib/belt_bsppx.exe $i.bin $i.bin2
../bsb_modules/bs-platform/lib/bsrefmt --parse binary $i.bin2 --print ml > $(basename $i)
rm $i.bin $i.bin2
done
