#!/bin/bash
PATH=/usr/lib/lightdm/lightdm:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games

function findLatestVer() {
        local REPOPATH=$1
        local TMPFILE=$(mktemp)
        local MAXVER=""
        local CURVER=""

        git ls-remote --tags $REPOPATH|grep -e '\^{}$'|sed -r 's/^[0-9a-f]*\s*refs\/tags\/([0-9][0-9]*\.[0-9][0-9]*\.[0-9][0-9]*)\^\{}/\1/' > $TMPFILE

        while read localver
        do
                CURVER=$localver
                if [ -z "$MAXVER" ]; then
                        MAXVER=$CURVER
                        true
                else
                        local MP1=$(echo $MAXVER|sed -r 's/^([0-9][0-9]*)\.[0-9][0-9]*\.[0-9][0-9]*$/\1/')
                        local CP1=$(echo $CURVER|sed -r 's/^([0-9][0-9]*)\.[0-9][0-9]*\.[0-9][0-9]*$/\1/')
                        local MP2=$(echo $MAXVER|sed -r 's/^[0-9][0-9]*\.([0-9][0-9]*)\.[0-9][0-9]*$/\1/')
                        local CP2=$(echo $CURVER|sed -r 's/^[0-9][0-9]*\.([0-9][0-9]*)\.[0-9][0-9]*$/\1/')
                        local MP3=$(echo $MAXVER|sed -r 's/^[0-9][0-9]*\.[0-9][0-9]*\.([0-9][0-9]*)$/\1/')
                        local CP3=$(echo $CURVER|sed -r 's/^[0-9][0-9]*\.[0-9][0-9]*\.([0-9][0-9]*)$/\1/')
                        if [ "$MP1" -lt "$CP1" ]; then
                                MAXVER=$CURVER
                                true
                        elif [ "$MP1" -eq "$CP1" -a "$MP2" -lt "$CP2" ]; then
                                MAXVER=$CURVER
                                true
                        elif [ "$MP1" -eq "$CP1" -a "$MP2" -eq "$CP2" -a "$MP3" -lt "$CP3" ]; then
                                MAXVER=$CURVER
                                true
                        fi
                fi

        done < $TMPFILE
        rm -f $TMPFILE
        echo $MAXVER
}
EPVER=$(findLatestVer https://github.com/mydlp/mydlp-endpoint-win.git)
MSIFN=mydlp_$(echo $EPVER|sed -s 's/\./_/g').msi

echo $EPVER > latest.txt || exit 1
[ -f "latest.msi" ] && mv -f latest.msi $MSIFN
wget -Nc http://ftp/ftp/pub/mydlp/endpoint/$MSIFN || exit 1
mv -f $MSIFN latest.msi

