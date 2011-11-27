#!/usr/bin/python
import ctypes
import sys
import magic
import os
import os.path
import subprocess
import re
from  string  import lower

class BCFileIntegrity:
    
    """Binary and Compressed File Analysis"""

    def __init__(self):
	if os.path.exists(".libs/libbfi.so"):
		self.libelfc = ctypes.CDLL("./.libs/libbfi.so")
	else:
		self.libelfc = ctypes.CDLL("libbfi.so")
	self.ms = magic.open(magic.MAGIC_NONE)
        self.ms.load()

    def getElfSize(self, filepath):
        return self.libelfc.get_elf_size(filepath) 

    def get7zSize(self, filepath):
        proc = subprocess.Popen(['/usr/bin/7z', 't', filepath], 
            stdout=subprocess.PIPE,
            )

        output = proc.communicate()[0] 
        pattern = r".*Compressed:\s*(\d+).*"
        result = re.match(pattern, output,re.MULTILINE| re.DOTALL)
        return int(result.group(1))

    def getType(self, filepath):
        ftype = self.ms.file(filepath)
        return ftype.split()[0]

    def getFileSize(self,filepath):
        return os.path.getsize(filepath)       

    def checkBinarySize(self, filepath):
	elfsize = self.getElfSize(filepath)
	filesize = self.getFileSize(filepath)
	if elfsize > 0:
        	return (filesize == elfsize)
	else:
		multiplier = 2
		mconffile = "/etc/mydlp/unstripped_x86_multiplier"
		if os.path.isfile(mconffile):
			try:
				mfloatstring = open(mconffile, 'r').read()
				multiplier = float(mfloatstring)
			except:
				multiplier = 2
		return (filesize < ( -(elfsize*multiplier) + 1024))

    def checkArchiveSize(self, filepath):
        return (self.getFileSize(filepath) == self.get7zSize(filepath))    

#file_path = ""
    
#bfa = BCFileIntegrity()

#print bfa.getElfSize(file_path)
#print bfa.getFileSize(file_path)
#print bfa.getType(file_path)
#print bfa.get7zSize(file_path)
#print bfa.checkBinarySize(file_path)
#print bfa.checkArchiveSize(file_path)

