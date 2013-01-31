# Can combine the outputs from
#   a) change from the original scene
#   b) shape detection from near objects

# POA:
#   Make calibration board
#   Calibrate camera with depth sensor too
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

cam = Kinect()

def differizer(base):
  while True:
    i = cam.getImage()
    n1 = i.getNumpy()
    n2 = base.getNumpy()

    d = cv2.absdiff(n1,n2)
    Image(cv2.cvtColor(d, cv2.COLOR_RGB2GRAY)).stretch(50,250).binarize().invert().show()


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
  return img.resize(int(fx*x), int(fy*y))

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

def depthFilter(img):
  filt = depthDetect().dilate(5)
  return img.applyBinaryMask(move(filt, (-20,20)),Color.WHITE)
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

tolerance = 0.5
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

def trackBlobs(blobs, prev):
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
  res = finalMatrix.I * inp
  [retx],[rety],[retz],[f] = res.tolist()
  return (retx, rety, retz,)

def depthTest():
  d = cam.getDepth()
  m = cam.getDepthMatrix().T
  
  (sx, sy) = d.size()
  while True:
    d.drawCircle((sx/2,sy/2),10, Color.GREEN)
    writeStats(d, ["Center is " + str(rawToMeters(m[sx/2,sy/2])) + "m away"])
    d.show()
    d = cam.getDepth()
    m = cam.getDepthMatrix().T
