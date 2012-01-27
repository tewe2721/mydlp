#!/usr/bin/env python

###--------------------------------------------------------------------------
###
###    Copyright (C) 2010 Huseyin Kerem Cevahir <kerem@medra.com.tr>
###
###--------------------------------------------------------------------------
###    This file is part of MyDLP.
###
###    MyDLP is free software: you can redistribute it and/or modify
###    it under the terms of the GNU General Public License as published by
###    the Free Software Foundation, either version 3 of the License, or
###    (at your option) any later version.
###
###    MyDLP is distributed in the hope that it will be useful,
###    but WITHOUT ANY WARRANTY; without even the implied warranty of
###    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
###    GNU General Public License for more details.
###
###    You should have received a copy of the GNU General Public License
###    along with MyDLP.  If not, see <http://www.gnu.org/licenses/>.
###--------------------------------------------------------------------------

import os

from mydlp import Mydlp
from mydlp.ttypes import *

from thrift.transport import TSocket
from thrift.transport import TTransport
from thrift.protocol import TBinaryProtocol
from thrift.server import TServer

import daemon
import lxml.html

if os.name == "posix":
	import magic
else:
	import mymagic

if os.name == "posix":
	import BCFileIntegrity
else:
	import atexit

class MydlpHandler:
	def __init__(self):
		if os.name == "posix":
			self.mime = magic.open(magic.MAGIC_MIME)
			self.mime.load()
			self.bcfi = BCFileIntegrity.BCFileIntegrity()
		else:
			magicdb = os.environ['MYDLP_APPDIR'] + "/magic"
			self.mime = mymagic.Magic(mime=True, magic_file=magicdb)

	def getMagicMime(self, data):
		try:
			mtype = ''
			if os.name == "posix":
				mtype = self.mime.buffer(data)
			else:
				mtype = self.mime.from_buffer(data)

			sc = mtype.find(';')
			if sc == -1:
				return mtype
			else:
				return mtype[0:sc]
		except:
			ge = GeneralException()
			ge.why = "Python backend internal error..."
			raise ge

	def htmlToText(self, html):
		try:
			t = lxml.html.fromstring(html)
			return t.text_content().encode('utf-8', 'ignore')
		except:
			ge = GeneralException()
			ge.why = "Python backend internal error..."
			raise ge

	def checkBinaryIntegrity(self, file_path):
		try:
			if os.name == "posix":
				return self.bcfi.checkBinarySize(file_path)
			else:
				return False
		except:
			return True

	def checkArchiveIntegrity(self, file_path):
		try:
			if os.name == "posix":
				return self.bcfi.checkArchiveSize(file_path)
			else:
				return False
		except:
			return True

class MyDLPBackendServer(daemon.Daemon):

	def __init__(self, pidfile):
		daemon.Daemon.__init__(self, pidfile)

		self.handler = MydlpHandler()

		self.processor = Mydlp.Processor(self.handler)
		self.transport = TSocket.TServerSocket("127.0.0.1", 9090)
		self.tfactory = TTransport.TBufferedTransportFactory()
		self.pfactory = TBinaryProtocol.TBinaryProtocolFactory()

		#server = TServer.TSimpleServer(processor, transport, tfactory, pfactory)
		#server = TServer.TThreadedServer(processor, transport, tfactory, pfactory)
		self.server = TServer.TThreadPoolServer(self.processor, self.transport, self.tfactory, self.pfactory)

	def run(self):
		if os.name != "posix":
			# write pidfile
			atexit.register(self.delpid)
			pid = str(os.getpid())
			file(self.pidfile,'w+').write("%s\n" % pid)
		
		print 'Starting MyDLP Backend server...'
		self.server.serve()
		print 'done.'

	def stop(self):
		print 'Stopping MyDLP Backend server...'
		#self.server.stop()
		self.transport.close()
		print 'done.'

if __name__ == '__main__':
	pidfile = "mydlp-backend-py.pid"

	if os.name == "posix":
		pidfile = "/var/run/mydlp/backend-py.pid"
		if len(sys.argv) > 1:
			pidfile = sys.argv[1]
	else:
		pidfile = os.environ['MYDLP_APPDIR'] + "/run/backend-py.pid"

	s = MyDLPBackendServer(pidfile)

	if os.name == "posix":
		s.start()
	else:
		s.run()


