import SimpleHTTPServer
import SocketServer
import time
from detect import *

class VisionServer(SimpleHTTPServer.SimpleHTTPRequestHandler):
  def do_GET(self):
    global base
    self.send_response(200)
    pos = differizer(base)
    if pos:
      (x,y,z) = pos
      pos = str((x,-y,z))
      print x,y,z
    else:
      pos = "(0.0,0.1,0.0)"

    #print "FOO", pos
    self.send_header("POS:" + pos, "text/html")
    self.end_headers()

server = SocketServer.TCPServer(('localhost', 8080), VisionServer)
#print "Getting base image..."
cam = Kinect()
base = cam.getImage()

#print "Starting server..."
try:
  while True:
    pos = differizer(base)
    if pos:
      (x,y,z) = pos
      print "{},{},{}".format(x,-y,z)
except:
  print "Aww"

#try:
#  server.serve_forever()
#except:
#  #print "Bye :("
#  server.socket.close()
#  server.shutdown()
