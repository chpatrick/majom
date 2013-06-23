# Can combine the outputs from
#   a) change from the original scene
#   b) shape detection from near objects

# POA:
#   Add adaption to base image for differizer to 'absorb' areas of
#     image that don't move
#   Get 3D position of simple object (ball)
#   Get 3D position using multiple methods for helicopter
#   Try out some orientation coding
#     Machine learning?
#     Scan left to right - med -> small -> big = facing left etc.
#   Connect to bot
#   Add ability for both to fly in two dimensions


from SimpleCV import *
import cv2
import time
import orient
from calibkinect import *
from numpy import *

cam = Kinect()

def ignoreErr(x):
  if x == 2047:
    return 0
  return x

def setup():
  b = freenect.sync_get_depth()[0].copy()
  f = adapt(b, 10000)
  while True:
    try:
      out = diffdiff2(b,f)
    except:
      return (b,f)

def loop(b, f, b2=None):
  while True:
    try:
      diffdiff2(b,f, b2)
    except e:
      print e

ignoreErrVec = np.vectorize(ignoreErr)

# Want to assemble a 'tolerance' matrix for different areas to get a quiet image
def adapt(b, limit=1000):
  filt = np.zeros((len(b),len(b[0])))
  r = np.array([limit+1])
  count = 0
  loc = None
  while r.sum() > limit or count < 10:
    d = freenect.sync_get_depth()[0]
    m = (b == 2047) & (d == 2047)
    m1 = np.ma.MaskedArray(b, mask=m, fill_value=0)
    m2 = np.ma.MaskedArray(d, mask=m, fill_value=0)
    r = cv2.absdiff(m1.filled(), m2.filled())

    r = np.ma.MaskedArray(r, mask=(r < filt), fill_value=0).filled()

    loc = Image(r * (255/2047.0)).rotate90()#.erode().binarize(0).invert()
    loc.show()
    m3 = np.ma.MaskedArray(r, mask=((r==0)|(r<filt)), fill_value=0)
    m4 = np.ma.MaskedArray(filt, mask=(r>filt), fill_value=10)
    filt = m3.filled() + m4.filled()
    if r.sum() < limit:
      count += 1
    else:
      count = 0
  return filt

def drawArrow(img, (c1, c2), width, direction):
  pt1 = sin(radians(direction))
  pt2 = cos(radians(direction))
  pt1 = pt1 * width
  pt2 = pt2 * width / 3
  img.drawLine((pt1+c1, -pt2+c2), (-pt1+c1, pt2+c2), color=Color.RED, thickness=2)
  img.drawLine((-pt1+c1, pt2+c2),
      (-pt1+c1 + 5*sin((pi/2)-radians(direction)),
      pt2+c2 + 5*cos((pi/2)-radians(direction))), color=Color.RED, thickness=2)
  img.drawLine((-pt1+c1, pt2+c2),
      (-pt1+c1 - 5*sin((pi/2)-radians(direction)),
      pt2+c2 - 5*cos((pi/2)-radians(direction))), color=Color.RED, thickness=2)

def getVels(ps):
  if len(ps) < 2:
    return None
  vs = []
  for i in range(len(ps)-1):
    p1 = ps[i] 
    p2 = ps[i+1]
    v = []
    for j in range(len(p1)):
      v.append(p1[j] - p2[j])
    vs.append(tuple(v))
  return vs

def avgVel(vs):
  return average(vs, axis=0)

def velDirection(v):
  size = sqrt(sum(map(lambda x: x**2, v), axis=0))
  if size == 0:
    return None
  d = map(lambda x: x / size, v)
  theta = asin(abs(d[0]))
  if d[0] >= 0 and d[1] >= 0:
    return degrees(theta)
  elif d[0] <= 0 and d[1] <= 0:
    return degrees(theta + pi)
  elif d[0] <= 0:
    return degrees(2*pi - theta)
  else:
    return degrees(pi - theta)

def approxEq(a,b):
  return abs(a-b) < 1e-10

def diffdiff2(b, filt=None, imgBase=None, history=[]):
  print 0
  d = freenect.sync_get_depth()[0].copy()
  print 0.1
  img = cam.getImage()
  print 0.2
  m = (b == 2047) | (d == 2047)
  m1 = np.ma.MaskedArray(b, mask=m, fill_value=0)
  m2 = np.ma.MaskedArray(d, mask=m, fill_value=0)
  
  print 1
  r = cv2.absdiff(m1.filled(), m2.filled())
  if filt != None:
    m3 = np.ma.MaskedArray(r, mask=(r < filt), fill_value=0)
  else:
    m3 = np.ma.MaskedArray(r, mask=(r < 0), fill_value=0)
  
  print 2
  loc = Image(m3.filled() * (255/2047.0)).rotate90().erode().binarize(0).invert()
  print 3
  blobs = loc.findBlobs(appx_level=10)

  #if blobs:
  #  blobs.show()
  #return
  print 4
  if blobs:
    cents = map(lambda x: (x.centroid(), x.area()),blobs)
    area = sum(map(Blob.area, blobs))
    cx,cy = sum(map(lambda ((x,y),w): (int(x*(w/area)),int(y*(w/area))),cents),0)
    print 6
    loc.drawCircle((cx,cy), 10, color=Color.GREEN)
    print 7
    mb = max(blobs,key=Blob.area).centroid()
    print 8
    depth = array([[d[mb[1],mb[0]]]])
    if depth == 2047:
      print " oh noesss!"
      for x in range(3):
        for y in range(3):
          depth = array([[d[mb[1]-1+y,mb[0]-1+x]]])
          if depth != 2047:
            break;
        if depth != 2047:
          break;
    xyz,uv = depth2xyzuv(depth, cx, cy)
    print 9
    if (xyz != None) and len(xyz) > 0:
      x,y,z = xyz[0]
      img.drawCircle((int(uv[0,0]),int(uv[0,1])),10,color=Color.WHITE)
      img.drawText(str((round(x,2),round(y,2),round(z,2))), int(uv[0,0])+10, int(uv[0,1])+10)
      #heli = orient.cut(img, (int(uv[0,0]),int(uv[0,1])))
      #heliD = orient.cut(loc, (cx,cy))
      #b = orient.cut(imgBase, (int(uv[0,0]),int(uv[0,1])))

      #d = diffDetect(b,heli)
      #extHeli = d.stretch(30,250).binarize(0).invert()

      #heli.sideBySide(extHeli).show()
      #return (heli,extHeli)
      #img.sideBySide(heli).show()
      history = history[:5]
      d = 0
      if len(history) > 1:
        vels = getVels(history)
        v = avgVel(vels)
        d = velDirection(v)
        if d:
          drawArrow(img, (320,100), 100, d)
      img.show()
      img.save("detected.png")

      return (round(x,4), round(y,4), round(z,4), d, 0)
    else:
      pass


    print 10
    img.show()
    #blobs.show()
  else:
    loc.show()
  return None

def diffdiff(b):
  d = ignoreErrVec(freenect.sync_get_depth()[0])
  r = cv2.absdiff(b,d) * (255/2047.0)
  loc = Image(r).rotate90().binarize().invert()
  loc.show()

def differizer(base):
  print "Gettin images"
  i = cam.getImage()
  depths = cam.getDepthMatrix().T
  print "Got images..."
  n1 = i.getNumpy()
  n2 = base.getNumpy()

  d = cv2.absdiff(n1,n2)
  print "Makin' image"
  loc = Image(cv2.cvtColor(d, cv2.COLOR_RGB2GRAY)).stretch(50,250).binarize().invert().erode().dilate(4)
  print "Image madeee"
  loc = correctRGB(loc)
  print "Finding blobs..>"
  blobs = loc.findBlobs()
  print "blobs found!"
  i = correctRGB(i)
  if blobs:
    print "Blobbening!"
    #ds = assocBlobs(blobs, Image(depths))
    #pBlobs = partitionBlobs(zip(blobs, ds))
    m = loc.getGrayNumpy()
    mdepth = np.ma.MaskedArray(depths, mask=(m!=255))
    cleanDepth = np.ma.masked_values (mdepth, 2047)
    centers = map(Blob.centroid, blobs)
    (cx, cy) = average(centers, 0)

    (a,b,c) = depthToWorld(cx, cy, cleanDepth.min()) #closest value!
    (mx,my) = worldToRGB((0,-0.1,c))
    print "Drawing stuffff"
    i.drawCircle((mx,my), 10, color=Color.GREEN)

    (a,b,c) = tuple(k.mu_hat_est[1,:])
    i.drawCircle(worldToRGB((a,b,c)), 10, color=Color.RED)
    print "Done drawing!"
    #i = i.sideBySide(loc)
    i.show()
    return (round(a, 3),round(b,3),round(c,3))
    #for b in pBlobs:
    #  (x1,y1,x2,y2) = trackBlobs(b, None)
    #  i.drawRectangle(x1,y1,x2-x1,y2-y1)
    #  (x,y) = ((x2 - x1)/2 + x1, (y2 - y1)/2 + y1)
    #  i.drawCircle((x,y), 10)
  i.show()

  return None

def unzip(xs):
  x = []
  y = []
  for (a,b) in xs:
    x.append(a)
    y.append(b)
  return (x,y)

def diffDetect(base, img):
  n1 = base.getNumpy()
  n2 = img.getNumpy()
  n1 = n1 * 1
  n2 = n2 * 1

  return Image(cv2.absdiff(n1,n2))#.smooth().dilate(2)

def basic(img):
    dist = img.colorDistance(Color.WHITE).dilate(4)
    segmented = dist.stretch(150,255)
    return segmented.binarize()

def move(img, (x,y)):
  points= ((x, y), (img.width+x, y), (img.width+x, img.height+y), (x, img.height+y))
  return img.shear(points)

def scale(img, (fx, fy)):
  (x, y) = img.size()
  img = img.resize(int(fx*x), int(fy*y))
  if fx <= 1:
    img = img.embiggen((x, int(fy*y)))
  else:
    img = img.crop(fx*x/2, fy*y/2, x, fy*y, True)
  (x,_) = img.size()
  if fy <= 1:
    img = img.embiggen((x, y))
  else:
    img = img.crop(x/2, fy*y/2, x, y, True)
  return img

def getDepth():
  # Get depth image
  img = cam.getDepth()
  return img

def depthDetect():
  img = getDepth()
  # Restrict to foreground. Cut out everything else
  field = img.stretch(0,200)
  # Change stuff we can see to white and everything else to black
  visible = field.binarize()
  return visible

def correctDepth(img):
  return scale(move(img, (-20,20)), (0.9,0.9))

def correctRGB(img):
  return move(scale(img, (1.0/0.9,1.0/0.9)), (20,-20))

def depthFilter(img):
  filt = depthDetect().dilate(5)
  return img.applyBinaryMask(correctDepth(filt),Color.WHITE)
  #return img.applyBinaryMask(filt, Color.WHITE)

normalCrop = (90, 0, 420, 480)

def detect(foo, human=True, filt=None, crop=None):
  # Get starting time
  oldTime = time.time()

  # Crop image if necessary
  if crop:
    disp = cam.getImage().crop(crop).show()
  else:
    disp = cam.getImage().show()

  # Start running the detector
  while disp.isNotDone():
    stats = []
  # Crop image if necessary
  if crop:
    disp = cam.getImage().crop(crop).show()
  else:
    disp = cam.getImage().show()

  # Start running the detector
  while disp.isNotDone():
    stats = []
  else:
    disp = cam.getImage().show()

  # Start running the detector
  while disp.isNotDone():
    stats = []
    img = cam.getImage()
    if filt: # Prefilter?
      img = filt(img)
    if crop:
      img = img.crop(crop)
    if not human: # Show the machine-version
      img = foo(img).invert()
      blobs = img.findBlobs()
      img = img.invert()
      if blobs:
        map(lambda x: x.draw(),blobs)
        stats.append(str(len(blobs)) + " blobs found")

        ds = assocBlobs(blobs, getDepth())
        pBlobs = partitionBlobs(zip(blobs, ds))
        for b in pBlobs:
          (x1,y1,x2,y2) = trackBlobs(b, None)
          img.drawRectangle(x1,y1,x2-x1,y2-y1)

        for i in range(len(pBlobs)):
          (x,y) = pBlobs[i][0].centroid()
          img.drawText(str(round(ds[i],2)), x, y)

    # Calculate fps
    newTime = time.time()
    seconds = newTime - oldTime
      
    stats.append(str(round(1/seconds, 1)) + " fps")
    writeStats(img, stats)
    img.show()
    
    oldTime = newTime

    # Get user input
    if disp.leftButtonDownPosition() is not None: # left mouse button
      human = not human
      # Switch displays
    elif disp.rightButtonDownPosition() is not None:
      disp.quit()
      # stop running

def writeStats(img, stats):
  y = 10
  for s in stats:
    img.drawText(s, 10, y, Color.BLACK)
    y += 10

tolerance = 50
def partitionBlobs(blobs):
  bs = sorted(blobs,key=lambda x: x[1]) 
  ret = []
  curD = 0.0
  cur = []
  for (b,d) in bs:
    if d > curD + tolerance: # new block
      if cur:
        ret.append(cur)
      cur = [b]
    else:
      cur.append(b)
    curD = d

  if cur:
    ret.append(cur)
  return ret

def assocBlobs(blobs, values):
  ret = []
  for b in blobs:
    rect = b.minRect()
    x1 = max(0,min(rect, key=lambda x: x[0])[0])
    y1 = max(0,min(rect, key=lambda x: x[1])[1])
    x2 = max(0,max(rect, key=lambda x: x[0])[0])
    y2 = max(0,max(rect, key=lambda x: x[1])[1])

    mask = b.blobMask()
    mask = mask.embiggen(size=values.size(),pos=(int(x1),int(y1)))
    depthVals = values.applyBinaryMask(mask)
    meanDepth = depthVals.crop(x1,y1,x2-x1,y2-y1).meanColor()[0]
    ret.append(meanDepth)
  return ret

def trackBlobs(blobs, prev=None):
  if not blobs:
    return None
  # create largest bounding box of blobs, return
  blobs = map(lambda x: x.minRect(), blobs)
  xs = map(lambda y: map(lambda x: x[0], y),blobs)
  xs = [x for y in xs for x in y]
  ys = map(lambda y: map(lambda x: x[1], y),blobs)
  ys = [y for x in ys for y in x]

  x1 = min(xs)
  x2 = max(xs)

  y1 = min(ys)
  y2 = max(ys)

  if not prev:
    return (x1,y1,x2,y2)
  
  # otherwise, make new bounding box of blobs.
  # compare to last box. Is it the same size? Has it moved?
  # Has the relative positions of it's composite blobs changed?
  # Exclude blobs based on judgements

# ---------------
# Borrowed from http://graphics.stanford.edu/~mdfisher/Kinect.html

def rawToMeters(raw):
  if raw < 2047:
    return (1.0 / (raw * -0.0030711016 + 3.3309495161));

  return 0.0;
  
fx_d = 1.0 / 5.9421434211923247e+02
fy_d = 1.0 / 5.9104053696870778e+02
cx_d = 3.3930780975300314e+02
cy_d = 2.4273913761751615e+02

def depthToWorld(x, y, d):
  depth = rawToMeters(d)
  return (((x - cx_d) * depth * fx_d),((y - cy_d) * depth * fy_d),depth)

from numpy import *

def zipWith(f, xs, ys):
  zs = zip(xs,ys)
  return map(f, zs)

def addCol(m, v):
  return zipWith(lambda (x,y): x+y, m, v)

def addRow(m, v):
  return m + [v]

rot = [[ 0.99984629, -0.00147791,  0.01747042],
       [ 0.00126354,  0.99992386,  0.01227534],
       [-0.01748723, -0.01225138,  0.99977202]]
trans = [[-0.01998524],
         [0.00074424],
         [0.01091674]]

finalMatrix = matrix(addRow(addCol(rot, trans),[0,0,0,1]))

fx_rgb = 5.2921508098293293e+02
fy_rgb = 5.2556393630057437e+02
cx_rgb = 3.2894272028759258e+02
cy_rgb = 2.6748068171871557e+02

def worldToRGB((x,y,z)):
  inp = matrix([x,y,z,1]).transpose()
  res = finalMatrix *  inp
  [rx],[ry],[rz],[rf] = res.tolist()
  invZ = 1.0 / rz
  return (int(rx*fx_rgb*invZ + cx_rgb),
          int(ry*fy_rgb*invZ + cy_rgb))
  
def RGBToVector((x,y)):
  rx = (x - cx_rgb)/fx_rgb
  ry = (y - cy_rgb)/fy_rgb

  inp = matrix([rx,ry,1,1]).transpose()
