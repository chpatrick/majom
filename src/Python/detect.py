from SimpleCV import *
import cv2
import time

cam = Kinect()

def diffDetect(base, img):
  t1 = cv2.cvtColor(base.getNumpy(), cv2.COLOR_RGB2GRAY)
  t2 = cv2.cvtColor(img.getNumpy(), cv2.COLOR_RGB2GRAY)

  return Image(cv2.absdiff(t2,t1)).smooth().dilate(2)

def basic(img):
    dist = img.colorDistance(Color.WHITE).dilate(4)
    segmented = dist.stretch(180,255)
    return segmented.binarize()

def move(img, (x,y)):
  points= ((x, y), (img.width+x, y), (img.width+x, img.height+y), (x, img.height+y))
  return img.shear(points)

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
  filt = depthDetect().dilate(6)
  return img.applyBinaryMask(move(filt, (-20,20)),Color.WHITE)
#  return img.applyBinaryMask(filt)

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

    raw_input("Hi!")

    b.show()
    raw_input("The image looks like this (x and y are " + str(x1) + "," + str(y1) + ")")
    mask = b.blobMask()
    mask.show()
    raw_input("Embiggening...")
    mask = mask.embiggen(size=values.size(),pos=(int(x1),int(y1)))
    mask.show()
    raw_input("The mask is this")
    depthVals = values.applyBinaryMask(mask)
    depthVals.show()
    raw_input("The new depth vals are")
    meanDepth = depthVals.crop(x1,y1,x2-x1,y2-y1).meanColor()[0]
    print(meanDepth)
    raw_input()
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
