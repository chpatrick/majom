from SimpleCV import *
cam = Kinect()

while True:
  img = cam.getImage()
  dist = img.colorDistance(Color.BLACK).dilate(2)
  segmented = dist.stretch(200,255)
  blobs = segmented.findBlobs()
  if blobs:
    circles = blobs.filter([b.isCircle(0.2) for b in blobs])
    if circles:
      img.drawCircle((circles[-1].x, circles[-1].y),circles[-1].radius(),Color.BLUE,3)
  img.show()
