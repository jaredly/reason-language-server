#!/bin/bash
set -ex
esy
# We need to run the binary outside of the esy env.
cp $(esy x which RunTests.exe) RunTests.exe
./RunTests.exe