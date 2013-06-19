import SimpleHTTPServer
import sys
import SocketServer
import time
import random
from detect import *
from surface import *

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


class SurfaceServer(SimpleHTTPServer.SimpleHTTPRequestHandler):
  def do_GET(self):
    surfaces = getSurfaces()
    self.send_response(200)
    self.send_header("SFS:" + str(surfaces), "text/html")
    self.end_headers()

def getSurfaces():
  ds = freenect.sync_get_depth()[0].copy()

  rmin = 220
  rdiff = 50
  ps = []
  uv = []
  a1,b1 = array([rmin + rdiff]),array([rmin + rdiff])
  a2,b2 = array([rmin + rdiff]),array([rmin])
  a3,b3 = array([rmin]),array([rmin + rdiff])

  p1 = depth2xyzuv(array([[ds[b1,a1]]]), a1, b1)[0][0]
  p2 = depth2xyzuv(array([[ds[b2,a2]]]), a2, b2)[0][0]
  p3 = depth2xyzuv(array([[ds[b3,a3]]]), a3, b3)[0][0]

  #img = cam.getImage()
  #img.drawPoints([(a1,b1), (a2,b2), (a3,b3)])
  #img.show()
  surfaces = [assembleSurface(p1,p2,p3)]
  print surfaces
  return surfaces

#server1 = SocketServer.TCPServer(('localhost', 8081), SurfaceServer)
surfaces = getSurfaces()

server2 = SocketServer.TCPServer(('localhost', 8080), VisionServer)
print "Getting base image..."
cam = Kinect()
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
        if rec != None:
          rec.append(((0.0,0.1,0.0,0.0,1), time.time()-startTime))
  except:
    if rec != None:
      out = open("rec_out.txt",'w')
      for p in rec:
        out.write(str(p) + "\n")
    
else:
  try:
    if 'vision' in sys.argv:
      server2.serve_forever()
    elif 'surface' in sys.argv:
      server1.serve_forever()
  except:
    print "Bye :("
    server1.socket.close()
    server1.shutdown()
    server2.socket.close()
    server2.shutdown()
    if rec != None:
      out = open("rec_out.txt",'w')
      for p in rec:
        out.write(str(p) + "\n")
    sys.exit()
