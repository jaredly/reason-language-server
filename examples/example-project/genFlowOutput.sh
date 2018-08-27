#!/usr/bin/env bash

THIS_SCRIPT_DIR="$(cd "$( dirname "$0" )" && pwd)"
FIRST=$(echo $2 | cut -d':' -f1)
SECOND=$(echo $2 | cut -d':' -f2)
ADD="Add $FIRST $SECOND"
WRITE="Write $THIS_SCRIPT_DIR/$FIRST"

echo "  " $ADD
echo "  " $WRITE
