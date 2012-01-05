#!/bin/bash

CURRENTVERSION=$(git describe |sed -s 's/-.*$//')
NEXTVERSION=""

function incrementVersion {
	local PRE=$(echo $CURRENTVERSION|sed -e 's/\.[0-9]$//')
	local LAST=$(echo $CURRENTVERSION|sed -e 's/^v[0-9]\.[0-9]\.//')
	NEXTVERSION="$PRE"".""$(( $LAST + 1 ))"
}

if [ -n "$1" -a "$1" == "updateVC" ]; then
	git tag -a $NEXTVERSION -m "Tag for release $CURRENTVERSION"
	git push --tags
fi

dpkg-buildpackage

