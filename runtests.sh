#!/bin/bash
set -ex
esy
# We need to run the binary outside of the esy env.
sh -c '$(esy x which RunTests.exe)'