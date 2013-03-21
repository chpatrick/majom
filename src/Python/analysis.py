from detect import *
import time

def trace(base, outfile=None):
  if outfile:
    f = open(outfile, 'w')
  caps = []
  while True:
    try:
      pos = differizer(base)
      cap = correctRGB(cam.getImage())
      if pos:
        (x,y,z) = pos
        pos = (x,y,z)
        caps.append((pos,cap))
        if outfile:
          f.write(str(pos) + "\n")
        time.sleep(0.2)
      #Sleep for some time
    except:
      print "oh :("
      if outfile:
        f.close()
      return caps
      break

def printGrid(image):
  DIM = 1

  for x in xrange(-10,11):
    points = []
    for z in xrange(-1,2):
      points.append((DIM*x*0.1,-1*DIM,2+(DIM*z)))

    for z in xrange(1,-2,-1):
      points.append((DIM*x*0.1,1*DIM,2+(DIM*z)))
    points = map(worldToRGB, points)
    for i in xrange(len(points)-1):
      image.drawLine(points[i], points[i+1], color=Color.RED, thickness=1)
  for y in xrange(-10,11):
    points = []
    for z in xrange(-1,2):
      points.append((-1*DIM,DIM*y*0.1,2+(DIM*z)))

    for z in xrange(1,-2,-1):
      points.append((1*DIM,DIM*y*0.1,2+(DIM*z)))
    points = map(worldToRGB, points)
    for i in xrange(len(points)-1):
      image.drawLine(points[i], points[i+1], color=Color.RED, thickness=1)

  points = []
  for x in xrange(-1,2):
    points.append((DIM*x,0,2));

  points = map(worldToRGB, points)
  for i in xrange(len(points)-1):
    image.drawLine(points[i], points[i+1], color=Color.BLACK, thickness=3)

  points = []
  for y in xrange(-1,2):
    points.append((0,DIM*y,2))

  points = map(worldToRGB, points)
  for i in xrange(len(points)-1):
    image.drawLine(points[i], points[i+1], color=Color.BLACK, thickness=3)

  return image
def drawPath(image, path):
  pass
