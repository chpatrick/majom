import sys

from vision import Vision
from vision.ttypes import *

from thrift.transport import TSocket
from thrift.transport import TTransport
from thrift.protocol import TBinaryProtocol
from thrift.server import TServer

class VisionHandler:
  def observe(self):
    ret = Position()
    ret.x = 1.0
    ret.y = 2.0
    ret.z = 3.0
    return ret

handler = VisionHandler()
processor = Vision.Processor(handler)
transport = TSocket.TServerSocket(port=9090)
tfactory = TTransport.TBufferedTransportFactory()
pfactory = TBinaryProtocol.TBinaryProtocolFactory()

server = TServer.TSimpleServer(processor, transport, tfactory, pfactory)

print 'Starting the vision server...'
server.serve()
print 'done.'
