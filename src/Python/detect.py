from SimpleCV import *
cam = Kinect()

def basic(img):
    dist = img.colorDistance(Color.WHITE).dilate(2)
    segmented = dist.stretch(150,255)
    return segmented

def move(img, (x,y)):
  points= ((x, y), (img.width+x, y), (img.width+x, img.height+y), (x, img.height+y))
  return img.shear(points)

def depthDetect():
  # Get depth image
  img = cam.getDepth()
  # Restrict to foreground. Cut out everything else
  field = img.stretch(0,200)
  # Change stuff we can see to white and everything else to black
  visible = field.binarize()
  return visible

def depthFilter(img):
  filt = depthDetect().dilate(6)
  return img.applyBinaryMask(move(filt, (-20, 20)),Color.WHITE)
#  return img.applyBinaryMask(filt)

normalCrop = (90, 0, 420, 480)

def detect(foo, human=True, filt=None, crop=None):
  while True:
    if crop:
      disp = cam.getImage().crop(crop).show()
    else:
      disp = cam.getImage().show()
    while disp.isNotDone():
      img = cam.getImage()
      if filt:
        img = filt(img)
      if crop:
        img = img.crop(crop)
      if human:
        img.show()
      else:
        img = foo(img)
        blobs = img.findBlobs()
        map(lambda x: x.drawOutline(width=3),blobs)
        img.show()
      
      if disp.leftButtonDownPosition() is not None: # left mouse button
        human = not human
        # Switch displays
      elif disp.rightButtonDownPosition() is not None:
        disp.quit()
        # stop running

  #if blobs:
  #  circles = blobs.filter([b.isCircle(0.2) for b in blobs])
  #  if circles:
  #    img.drawCircle((circles[-1].x, circles[-1].y),circles[-1].radius(),Color.BLUE,3)
  #img.show()

def foo():
  disp = cam.getImage().show()
  while(disp.isNotDone()):
    if disp.leftButtonDownPosition() is not None: # left mouse button
      disp.writeFrame(cam.getImage())
      # Switch displays
    elif disp.rightButtonDownPosition() is not None:
      disp.quit()


