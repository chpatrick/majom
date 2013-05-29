from SimpleCV import *

def cut(img, (x,y), size=(150,100), resize=3):
  segment = img.crop((max(0,x), max(0,y)), (min(img.size()[0],x+size[0]), min(img.size()[1],y+size[1])), centered=True)
  ret = segment.resize(w=size[0]*resize)
  return ret

class HeliGraph:
  def __init__(self, gs):
    self.gs = gs

  def __delitem__(self,key):
    return HeliGraph(self.gs.__delitem__(key))
  def __getitem__(self,key):
    return HeliGraph(self.gs[key])
  def __setitem__(self, key, value):
    return HeliGraph(self.gs.__setitem__(key,value))

  def findBlade(self):
    xs = self.gs
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

  def findBlade2(self):
    return self.findLargeMin()

  def findLargeMax(self):
    xs = self.gs
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

  def findLargeMin(self):
    xs = self.gs
    i = self.findLargeMax()
    cur = xs[i]
    for j in xrange(len(xs[i:])):
      if xs[i+j] <= cur:
        cur = xs[i+j]
      else:
        if abs(xs[i] - xs[i+j]) < (max(xs)/4):
          return i + j + self[i+j:].findLargeMin()
        else:
          return i+j

  def graph(self, m1=None, m2=None, save=None):
    xs = self.gs
    height = max(xs)
    g = Image((len(xs),400))
    for x in range(len(xs)):
      g.drawLine((x,400),(x,400 - (xs[x]*400/height)), color=Color.WHITE)

    if m1:
      m1 = self.getMean()
      g.drawText("m1",m1,330)
      g.drawLine((m1,350),(m1,390), color=Color.RED, thickness=3)
    if m2:
      m2 = self.getNtiles(m2)
      g.drawLine((m2[0],370),(m2[1],370), color=Color.GREEN, thickness=2)
      g.drawText("m2L",m2[0],350)
      g.drawText("m2R",m2[1],350)
    g.show()
    if save:
      g.save(save)

  def getNtiles(self, n):
    xs = self.gs
    m1 = sum(xs)/n
    m2 = (n-1)*sum(xs)/n
    j = 0
    s = 0
    for i in xrange(len(xs)):
      s += xs[i]
      if s > m1 and j == 0:
        j = i
      if s > m2:
        return (j,i)

  def getMean(self):
    xs = self.gs
    m = sum(xs)/2
    s = 0
    for i in xrange(len(xs)):
      s += xs[i]
      if s > m:
        return i


class HeliImage:
  old = None
  def __init__(self, img):
    if type(img) == str:
      self.img = Image(img)
    else:
      self.img = img
    self.old = self.img.copy()

  def reset(self):
    self.img = self.old.copy()

  def measureLines(self, a=1):
    n = self.img.getNumpy()
    return HeliGraph(n.sum(axis=a).sum(axis=1))

  def cutBlades(self):
    (w,h) = self.img.size()
    xs = self.measureLines(0)
    i1 = xs.findBlade()
    i2 = xs[i1:].findBlade()
    self.img = self.img.crop(0,i1+i2,w,h)

  def cutBlades2(self):
    (w,h) = self.img.size()
    xs = self.measureLines(0)
    i1 = xs.findBlade2()
    i2 = xs[i1:].findBlade2()
    self.img =  self.img.crop(0,i1+i2,w,h)

  def show(self):
    self.img.show()

def iterateImages(foo):
  for i in range(0, 360, 45):
    img = Image("h_{}.jpg".format(str(i).zfill(3)))
    foo(img)
    print "Applying to image h_{}.jpg...".format(str(i).zfill(3))
    raw_input()

