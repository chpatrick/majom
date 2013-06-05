import SimpleHTTPServer
import sys
import SocketServer
import time
from detect import *

class VisionServer(SimpleHTTPServer.SimpleHTTPRequestHandler):
  history = []
  lastO = 0
  def do_GET(self):
    global base, filt
    self.send_response(200)
    #pos = differizer(base)
    pos = diffdiff2(base, filt, history=self.history)
    if pos:
      (x,y,z,o,v) = pos
      if o:
        self.lastO = o
      self.history.insert(0,(x,z))
      pos = str((x,y,z,self.lastO,v))
    else:
      pos = "(0.0,0.1,0.0,0.0,1)"
      if len(self.history) > 0:
        self.history.pop()

    print "FOO", pos
    self.send_header("POS:" + pos, "text/html")
    self.end_headers()

server = SocketServer.TCPServer(('localhost', 8080), VisionServer)
print "Getting base image..."
cam = Kinect()
#base = cam.getImage()
base = freenect.sync_get_depth()[0].copy()
filt = adapt(base,10000)

print "Starting server..."

if len(sys.argv) > 1 and sys.argv[1] == 'test':
  history = []
  while True:
    pos = diffdiff2(base, filt, history=history)
    if pos:
      (x,y,z,o,v) = pos
      history.insert(0,(x,z))
      print "{},{},{},{}".format(x,y,z,v)

else:
  try:
    server.serve_forever()
    pass
  except:
    print "Bye :("
    server.socket.close()
    server.shutdown()
