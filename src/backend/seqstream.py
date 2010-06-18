#!/usr/bin/python

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

import uno, unohelper
from com.sun.star.io import XSeekable, XInputStream, XOutputStream

class SequenceOutputStream( unohelper.Base, XOutputStream ):
      def __init__( self ):
          self.s = uno.ByteSequence("")
          self.closed = 0

      def closeOutput(self):
          self.closed = 1

      def writeBytes( self, seq ):
          self.s = self.s + seq

      def flush( self ):
          pass

      def getSequence( self ):
          return self.s

class SequenceInputStream( XSeekable, XInputStream, unohelper.Base ):
      def __init__( self, seq ):
          self.s = seq
          self.nIndex = 0
          self.closed = 0

      def closeInput( self):
          self.closed = 1
          self.s = None

      def skipBytes( self, nByteCount ):
          if( nByteCount + self.nIndex > len(self.s) ):
              nByteCount = len(self.s) - self.nIndex
          self.nIndex += nByteCount

      def readBytes( self, retSeq, nByteCount ):
          nRet = 0
          if( self.nIndex + nByteCount > len(self.s) ):
              nRet = len(self.s) - self.nIndex
          else:
              nRet = nByteCount
          retSeq = uno.ByteSequence(self.s.value[self.nIndex : self.nIndex + nRet ])
          self.nIndex = self.nIndex + nRet
          return nRet, retSeq

      def readSomeBytes( self, retSeq , nByteCount ):
          #as we never block !
          return readBytes( retSeq, nByteCount )

      def available( self ):
          return len( self.s ) - self.nIndex

      def getPosition( self ):
          return self.nIndex

      def getLength( self ):
          return len( self.s )

      def seek( self, pos ):
          self.nIndex = pos

