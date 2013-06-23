import SimpleHTTPServer
import sys
import SocketServer
from surface import *

# SurfaceServer implementation
class SurfaceServer(SimpleHTTPServer.SimpleHTTPRequestHandler):
  def do_GET(self):
    surfaces = getSurfaces()
    self.send_response(200)
    self.send_header("SFS:" + str(surfaces), "text/html")
    self.end_headers()

# Grab surfaces from three points in the scene
def getSurfaces():
  ds = freenect.sync_get_depth()[0].copy()

  a1,b1 = array([320]),array([240])
  a2,b2 = array([330]),array([250])
  a3,b3 = array([330]),array([230])

  p1 = depth2xyzuv(array([[ds[240,320]]]), b1, a1)[0][0]
  p2 = depth2xyzuv(array([[ds[250,330]]]), b2, a2)[0][0]
  p3 = depth2xyzuv(array([[ds[230,330]]]), b3, a3)[0][0]
  p1 = [p1[1],p1[0],p1[2]]
  p2 = [p2[1],p2[0],p2[2]]
  p3 = [p3[1],p3[0],p3[2]]

  img = cam.getImage()
  img.drawPoints([(320,240),(330,250),(330,230)])
  img.show()
  surfaces = [assembleSurface(p1,p2,p3)]
  print surfaces
  return surfaces

# Initialise server
server = SocketServer.TCPServer(('localhost', 8081), SurfaceServer)

# Start server
try:
  print "Starting server..."
  server.serve_forever()
except:
  print "Bye :("
  server.socket.close()
  server.shutdown()
