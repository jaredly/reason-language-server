set -e
for i in *.ml; do
../bsb_modules/bs-platform/lib/bsrefmt $i --print binary > $i.bin
COMPILE_TO_NATIVE=1 ../bsb_modules/bs-platform/lib/belt_bsppx.exe $i.bin $i.bin2
../bsb_modules/bs-platform/lib/bsrefmt --parse binary $i.bin2 --print ml > $i
rm $i.bin $i.bin2
done
