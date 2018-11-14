set -e
rm -f types.lock.json
rm -f data.*.json
for i in {1..6}
do
  echo "include AllTypes.V$i" > src/Types.re
  sed -i -e "s/\"version\": [0-9]/\"version\": $i/" types.json
  npm start -s
  rm -rf lib
  npm run -s try
done