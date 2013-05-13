import SimpleHTTPServer
import SocketServer
import time
from detect import *

class VisionServer(SimpleHTTPServer.SimpleHTTPRequestHandler):
  def do_GET(self):
    global base, filt
    self.send_response(200)
    #pos = differizer(base)
    pos = diffdiff2(base, filt)
    if pos:
      (x,y,z,v) = pos
      pos = str((x,y,z,v))
    else:
      pos = "(0.0,0.1,0.0,1)"

    print "FOO", pos
    self.send_header("POS:" + pos, "text/html")
    self.end_headers()

server = SocketServer.TCPServer(('localhost', 8080), VisionServer)
print "Getting base image..."
cam = Kinect()
#base = cam.getImage()
base = freenect.sync_get_depth()[0].copy()
filt = adapt(base,2000)

print "Starting server..."

#while True:
#  pos = diffdiff2(base, filt)
#  if pos:
#    (x,y,z,v) = pos
#    print "{},{},{},{}".format(x,-y,z,v)

try:
  server.serve_forever()
  pass
except:
  print "Bye :("
  server.socket.close()
  server.shutdown()
