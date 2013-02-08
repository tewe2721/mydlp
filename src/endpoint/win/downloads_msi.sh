#!/bin/bash
[ -f "latest.txt" ] || exit 1
EPVER=$(cat latest.txt)
MSIFN=mydlp_$(echo $EPVER|sed -s 's/\./_/g').msi
echo "Generating latest.msi for $EPVER"
mkdir -p msi; cd msi; wget --quiet -Nc http://ftp/ftp/pub/mydlp/endpoint/$MSIFN || exit 1
[ -f "$MSIFN" ] || exit 1
