import sys

from vision import Vision
from vision.ttypes import *

from thrift.transport import TSocket
from thrift.transport import TTransport
from thrift.protocol import TBinaryProtocol
from thrift.server import TServer
from thrift.transport.TSocket import *

class VisionHandler:
  def observe(self):
    ret = Position()
    ret.x,ret.y,ret.z = (1,2,3)
    return ret

class MyServer(TSocketBase, TServerTransportBase):
  """Socket implementation of TServerTransport base."""

  def __init__(self, host=None, port=9090, unix_socket=None):
    self.host = host
    self.port = port
    self._unix_socket = unix_socket
    self.handle = None

  def listen(self):
    res0 = self._resolveAddr()
    for res in res0:
      if res[0] is socket.AF_INET6 or res is res0[-1]:
        break

    # We need remove the old unix socket if the file exists and
    # nobody is listening on it.
    if self._unix_socket:
      tmp = socket.socket(res[0], res[1])
      try:
        tmp.connect(res[4])
      except socket.error, err:
        eno, message = err.args
        if eno == errno.ECONNREFUSED:
          os.unlink(res[4])

    self.handle = socket.socket(res[0], res[1])
    self.handle.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    self.handle.setsockopt(socket.SOL_TCP, socket.TCP_NODELAY, 1);
    self.handle.setsockopt(socket.IPPROTO_TCP, socket.TCP_NODELAY, 1)
    if hasattr(self.handle, 'settimeout'):
      self.handle.settimeout(None)
    self.handle.bind(res[4])
    self.handle.listen(128)

  def accept(self):
    client, addr = self.handle.accept()
    result = MySocket()
    client.setsockopt(socket.IPPROTO_TCP, socket.TCP_NODELAY, 1)
    client.setsockopt(socket.SOL_TCP, socket.TCP_NODELAY, 1);
    result.setHandle(client)
    return result

class MySocket(TSocketBase):
  """Socket implementation of TTransport base."""

  def __init__(self, host='localhost', port=9090, unix_socket=None):
    """Initialize a TSocket

    @param host(str)  The host to connect to.
    @param port(int)  The (TCP) port to connect to.
    @param unix_socket(str)  The filename of a unix socket to connect to.
                             (host and port will be ignored.)
    """

    self.host = host
    self.port = port
    self.handle = None
    self._unix_socket = unix_socket
    self._timeout = None

  def setHandle(self, h):
    self.handle = h
    self.handle.setsockopt(socket.IPPROTO_TCP, socket.TCP_NODELAY, 1)
    self.handle.setsockopt(socket.SOL_TCP, socket.TCP_NODELAY, 1);

  def isOpen(self):
    return self.handle is not None

  def setTimeout(self, ms):
    if ms is None:
      self._timeout = None
    else:
      self._timeout = ms/1000.0

    if self.handle is not None:
      self.handle.settimeout(self._timeout)

  def open(self):
    try:
      res0 = self._resolveAddr()
      for res in res0:
        self.handle = socket.socket(res[0], res[1])
        self.handle.settimeout(self._timeout)
        try:
          self.handle.setsockopt(socket.IPPROTO_TCP, socket.TCP_NODELAY, 1)
          self.handle.setsockopt(socket.SOL_TCP, socket.TCP_NODELAY, 1);
          self.handle.connect(res[4])
        except socket.error, e:
          if res is not res0[-1]:
            continue
          else:
            raise e
        break
    except socket.error, e:
      if self._unix_socket:
        message = 'Could not connect to socket %s' % self._unix_socket
      else:
        message = 'Could not connect to %s:%d' % (self.host, self.port)
      raise TTransportException(type=TTransportException.NOT_OPEN, message=message)
    self.handle.setsockopt(socket.IPPROTO_TCP, socket.TCP_NODELAY, 1)
    self.handle.setsockopt(socket.SOL_TCP, socket.TCP_NODELAY, 1);

  def read(self, sz):
    try:
      self.handle.setsockopt(socket.IPPROTO_TCP, socket.TCP_NODELAY, 1)
      self.handle.setsockopt(socket.SOL_TCP, socket.TCP_NODELAY, 1);
      buff = self.handle.recv(sz)
    except socket.error, e:
      if (e.args[0] == errno.ECONNRESET and
          (sys.platform == 'darwin' or sys.platform.startswith('freebsd'))):
        # freebsd and Mach don't follow POSIX semantic of recv
        # and fail with ECONNRESET if peer performed shutdown.
        # See corresponding comment and code in TSocket::read()
        # in lib/cpp/src/transport/TSocket.cpp.
        self.close()
        # Trigger the check to raise the END_OF_FILE exception below.
        buff = ''
      else:
        raise
    if len(buff) == 0:
      raise TTransportException(type=TTransportException.END_OF_FILE, message='TSocket read 0 bytes')
    return buff

  def write(self, buff):
    if not self.handle:
      raise TTransportException(type=TTransportException.NOT_OPEN, message='Transport not open')
    sent = 0
    self.handle.setsockopt(socket.IPPROTO_TCP, socket.TCP_NODELAY, 1)
    self.handle.setsockopt(socket.SOL_TCP, socket.TCP_NODELAY, 1);
    have = len(buff)
    while sent < have:
      plus = self.handle.send(buff)
      if plus == 0:
        raise TTransportException(type=TTransportException.END_OF_FILE, message='TSocket sent 0 bytes')
      sent += plus
      buff = buff[plus:]

  def flush(self):
    pass
ret = Position()
handler = VisionHandler()
processor = Vision.Processor(handler)
transport = MyServer(port=9090)
tfactory = TTransport.TBufferedTransportFactory()
pfactory = TBinaryProtocol.TBinaryProtocolFactory()

server = TServer.TSimpleServer(processor, transport, tfactory, pfactory)

print 'Starting the vision server...'
server.serve()
print 'done.'
