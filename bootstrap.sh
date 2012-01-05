#!/bin/bash

REVISION=1

if [ -n "$1" ]; then
	REVISION=$1
fi

VERSION=$(git describe |sed -s 's/-.*$//')

function applyTemplate {
	local TMPL=$1
	local OUT=$2
	local TMPFILE="$OUT""~"

	cp -af $TMPL $TMPFILE
	sed -i -e "s/%%%VERSION%%%/$VERSION/g" $TMPFILE
	sed -i -e "s/%%%REVISION%%%/$REVISION/g" $TMPFILE
	mv -f $TMPFILE $OUT
}

echo "Current version is $VERSION-$REVISION"

applyTemplate configure.ac.tmpl configure.ac
applyTemplate debian/changelog.tmpl debian/changelog

git submodule init
git submodule update

autoreconf -f -i

