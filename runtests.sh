#!/bin/bash
set -ex

esy
TARGET_BIN=$(esy echo '#{self.target_dir}/install/default/bin')
$TARGET_BIN/UtilTests.exe
$TARGET_BIN/ExamplesTests
$TARGET_BIN/AllTests
BASE=old_ocamls/407
(cd $BASE; esy)
$TARGET_BIN/AllTests -b $BASE