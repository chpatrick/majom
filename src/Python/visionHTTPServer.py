import SimpleHTTPServer
import sys
import SocketServer
import time
from detect import *

class VisionServer(SimpleHTTPServer.SimpleHTTPRequestHandler):
  history = []
  lastO = 0
  def do_GET(self):
    global base, filt, rec, startTime
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
    if rec != None:
      rec.append((pos,time.time()-startTime))
    self.send_header("POS:" + pos, "text/html")
    self.end_headers()

server = SocketServer.TCPServer(('localhost', 8080), VisionServer)
print "Getting base image..."
cam = Kinect()
#base = cam.getImage()
base = freenect.sync_get_depth()[0].copy()
filt = adapt(base,10000)
rec = None
startTime = time.time()

print "Starting server..."

if len(sys.argv) > 1 and "rec" in sys.argv:
  rec = []
if len(sys.argv) > 1 and sys.argv[1] == 'test':
  history = []
  try:
    while True:
      pos = diffdiff2(base, filt, history=history)
      if pos:
        (x,y,z,o,v) = pos
        history.insert(0,(x,z))
        print "{},{},{},{}".format(x,y,z,v)
        if rec != None:
          rec.append((pos,time.time()-startTime))
      else:
        rec.append(((0.0,0.1,0.0,0.0,1), time.time()-startTime))
  except:
    if rec != None:
      out = open("rec_out.txt",'w')
      for p in rec:
        out.write(str(p) + "\n")
    
else:
  try:
    server.serve_forever()
  except:
    print "Bye :("
    server.socket.close()
    server.shutdown()
    if rec != None:
      out = open("rec_out.txt",'w')
      for p in rec:
        out.write(str(p) + "\n")
