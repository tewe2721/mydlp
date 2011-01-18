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

from mydlp import Mydlp
from mydlp.ttypes import *

from thrift.transport import TSocket
from thrift.transport import TTransport
from thrift.protocol import TBinaryProtocol
from thrift.server import TServer

import magic

import iban
import daemon
import BCFileIntegrity

import lxml.html

class MydlpHandler:
	def __init__(self):
		self.mime = magic.open(magic.MAGIC_MIME)
		self.mime.load()
		self.bcfi = BCFileIntegrity.BCFileIntegrity()

	def getMagicMime(self, data):
		try:
			mtype = self.mime.buffer(data)
			sc = mtype.find(';')
			if sc == -1:
				return mtype
			else:
				return mtype[0:sc]
		except:
			ge = GeneralException()
			ge.why = "Python backend internal error..."
			raise ge

	def isValidIban(self, iban_str):
		try:
			myIBAN = iban.IBAN(iban_str)
			return myIBAN.is_valid()
		except:
			return False

	def htmlToText(self, html):
		try:
			t = lxml.html.fromstring(html)
			return  t.text_content().encode('utf-8', 'ignore')
		except:
			ge = GeneralException()
			ge.why = "Python backend internal error..."
			raise ge

	def checkBinaryIntegrity(self, file_path):
		try:
			return self.bcfi.checkBinarySize(file_path)
		except:
			return False	

	def checkArchiveIntegrity(self, file_path):
		try:
			return self.bcfi.checkArchiveSize(file_path)
		except:
			return False	

class MyDLPBackendServer(daemon.Daemon):
	def run(self):
		handler = MydlpHandler()

		processor = Mydlp.Processor(handler)
		transport = TSocket.TServerSocket(9090)
		tfactory = TTransport.TBufferedTransportFactory()
		pfactory = TBinaryProtocol.TBinaryProtocolFactory()

		#server = TServer.TSimpleServer(processor, transport, tfactory, pfactory)
		#server = TServer.TThreadedServer(processor, transport, tfactory, pfactory)
		server = TServer.TThreadPoolServer(processor, transport, tfactory, pfactory)

		print 'Starting MyDLP Backend server...'
		server.serve()
		print 'done.'

pidfile = "/var/run/mydlp/backend-py.pid"

if len(sys.argv) > 1:
	pidfile = sys.argv[1]

s = MyDLPBackendServer(pidfile)

s.start()
#s.run()


