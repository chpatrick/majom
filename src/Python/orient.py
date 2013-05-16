from SimpleCV import *

def cut(img, (x,y), size=(150,100), resize=3):
  segment = img.crop((max(0,x), max(0,y)), (min(img.size()[0],x+size[0]), min(img.size()[1],y+size[1])), centered=True)
  ret = segment.resize(w=size[0]*resize)
  return ret

def measureLines(img, a=1):
  n = img.getNumpy()
  return n.sum(axis=a).sum(axis=1)

def findBlade(xs):
  ys = []
  for i in xrange(len(xs)):
    if xs[i] < 100000:
      if len(ys) > 0:
        if sum(ys) > 1000000:
          return i
        else:
          ys = []
          continue
      continue
    else:
      ys.append(xs[i])
  print "Couldn't find anything :("
  return -1

def findBlade2(xs):
  return findLargeMin(xs)

def findLargeMax(xs):
  cur = 0
  for i in xrange(len(xs)):
    if xs[i] < 60000:
      continue
    else:
      if cur <= xs[i]:
        cur = xs[i]
      else:
        if reduce(lambda x,y: x or y, map(lambda x: x > xs[i], xs[i:i+20])):
          continue
        else:
          return i

def findLargeMin(xs):
  i = findLargeMax(xs)
  cur = xs[i]
  for j in xrange(len(xs[i:])):
    if xs[i+j] <= cur:
      cur = xs[i+j]
    else:
      if abs(xs[i] - xs[i+j]) < (max(xs)/4):
        return i + j + findLargeMin(xs[i+j:])
      else:
        return i+j


def cutBlades(img):
  (w,h) = img.size()
  xs = measureLines(img, 0)
  i1 = findBlade(xs)
  i2 = findBlade(xs[i1:])
  return img.crop(0,i1+i2,w,h)

def cutBlades2(img):
  (w,h) = img.size()
  xs = measureLines(img, 0)
  i1 = findBlade2(xs)
  i2 = findBlade2(xs[i1:])
  return img.crop(0,i1+i2,w,h)
def graph(xs):
  height = max(xs)
  g = Image((len(xs),400))
  for x in range(len(xs)):
    g.drawLine((x,400),(x,400 - (xs[x]*400/height)), color=Color.WHITE)
  return g

def iterateImages(foo):
  for i in range(0, 360, 45):
    img = Image("h_{}.jpg".format(str(i).zfill(3)))
    foo(img)
    print "Applying to image h_{}.jpg...".format(str(i).zfill(3))
    raw_input()
