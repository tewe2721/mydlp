#!/bin/bash
PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin

DOWNLOAD_BASE=https://raw.github.com/mydlp/mydlp/master/src/sysconf/ossim
SETUP_CFG=/etc/ossim/ossim_setup.conf

for f in mydlp.cfg rsyslog/mydlp.conf logrotate/mydlp mysql/mydlp.sql
do
	if [ ! -e $f ]; then
		mkdir -p $(dirname $f) 
		wget -O $f "$DOWNLOAD_BASE/$f"
		if [ ! -e $f ]; then
			echo "Can not get $f . Exiting !"
			exit 1
		fi
	fi
done

cp -a mydlp.cfg /etc/ossim/agent/plugins/
cp -a rsyslog/mydlp.conf /etc/rsyslog.d/
cp -a logrotate/mydlp /etc/logrotate.d/
cp -a mysql/mydlp.sql /usr/share/doc/ossim-mysql/contrib/plugins/

if [ -n "$(grep -e '^detectors=' $SETUP_CFG)" ]; then
	if [ -z "$(grep -e '^detectors=' $SETUP_CFG|grep mydlp)" ]; then
		sed -i -e 's/^detectors=/detectors=mydlp, /g' $SETUP_CFG
	fi
else
	echo "Can not find detectors entry in $SETUP_CFG . Please add manually by running ossim-setup command (Step 3 -> Step 3 -> Select mydlp -> Save Changes) and re-run this script."
	exit 1
fi

cat /usr/share/doc/ossim-mysql/contrib/plugins/mydlp.sql | ossim-db
/etc/init.d/rsyslog restart
ossim-reconfig
