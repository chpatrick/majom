from SimpleCV import *
cam = Kinect()

def basic(img):
    dist = img.colorDistance(Color.WHITE).dilate(2)
    segmented = dist.stretch(180,255)
    blobs = segmented.findBlobs()
    return blobs

def detect(foo):
  while True:
    img = cam.getImage().crop(90,0,420,480)
    ret = foo(img)
    ret.show()
  #if blobs:
  #  circles = blobs.filter([b.isCircle(0.2) for b in blobs])
  #  if circles:
  #    img.drawCircle((circles[-1].x, circles[-1].y),circles[-1].radius(),Color.BLUE,3)
  #img.show()

