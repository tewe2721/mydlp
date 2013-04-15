#!/bin/bash
SETUP_CFG=/etc/ossim/ossim_setup.conf

[ -e mydlp.cfg ] || ( wget https://raw.github.com/mydlp/mydlp/master/src/sysconf/ossim/mydlp.cfg )
cp -a mydlp.cfg /etc/ossim/agent/plugins/

[ -e rsyslog/mydlp.conf ] || ( mkdir -p rsyslog ; cd rsyslog ; wget https://raw.github.com/mydlp/mydlp/master/src/sysconf/ossim/rsyslog/mydlp.conf )
cp -a rsyslog/mydlp.conf /etc/rsyslog.d/

[ -e logrotate/mydlp ] || ( mkdir -p logrotate ; cd logrotate ; wget https://raw.github.com/mydlp/mydlp/master/src/sysconf/ossim/logrotate/mydlp )
cp -a logrotate/mydlp /etc/logrotate.d/

[ -e mysql/mydlp.sql ] || ( mkdir -p mysql ; cd mysql ; wget https://raw.github.com/mydlp/mydlp/master/src/sysconf/ossim/mysql/mydlp.sql )
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
