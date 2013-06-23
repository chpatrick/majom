# ('(0.0,0.1,0.0,0.0,1)', 296.58335995674133)

def parseRec(filename):
  f = open(filename, 'r')
  ps = f.readlines()
  f.close()
  f = open(filename + "_clean.csv", 'w')
  for p in ps:
    stripped = ""
    for x in p:
      if not x in '()\'':
        stripped += x
    x,y,z,o,v,t = map(float, stripped.split(','))
    f.write("{},{},{},{},{}\n".format(x,y,z,v,t))
  f.close()
  
import sys
parseRec(sys.argv[1])
