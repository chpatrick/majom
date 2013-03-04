from detect import *

def trace(outfile):
  f = open(outfile, 'w')
  while True:
    try:
      pos = differizer(base)
      if pos:
        (x,y,z) = pos
        pos = str((x,-y,z))
        f.write(pos + "\n")
    except:
      print "oh :("
      f.close()
      break
