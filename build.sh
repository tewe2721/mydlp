#!/bin/bash

CURRENTVERSION=$(git describe |sed -s 's/-.*$//')
NEXTVERSION=""

function incrementVersion {
	local PRE=$(echo $CURRENTVERSION|sed -e 's/\.[0-9]$//')
	local LAST=$(echo $CURRENTVERSION|sed -e 's/^v[0-9]\.[0-9]\.//')
	NEXTVERSION="$PRE"".""$(( $LAST + 1 ))"
}

bash bootstrap.sh

dpkg-buildpackage || exit 1

if [ -n "$1" -a "$1" == "updateVC" ]; then
	incrementVersion
	if (git tag|grep -q $NEXTVERSION) ; then
		echo "This revision is tagged already."
	else
		git tag -a $NEXTVERSION -m "Tag for release $NEXTVERSION" || exit 1
		git push --tags git@github.com:mydlp/mydlp.git || exit 1
	fi
fi

true

