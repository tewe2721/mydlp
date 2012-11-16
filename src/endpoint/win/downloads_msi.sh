#!/bin/bash
[ -f "latest.txt" ] || exit 1
EPVER=$(cat latest.txt)
MSIFN=mydlp_$(echo $EPVER|sed -s 's/\./_/g').msi
echo "Generating latest.msi for $EPVER"
[ -f "latest.msi" ] && mv -f latest.msi $MSIFN
wget --quiet -Nc http://ftp/ftp/pub/mydlp/endpoint/$MSIFN || exit 1
mv -f $MSIFN latest.msi || exit 1

