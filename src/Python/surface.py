from detect import *

def assembleSurface(pt1, pt2, pt3):
  n = cross(sub(pt2,pt1),sub(pt3,pt1))
  return (n,list(pt1))

def cross(a, b):
  c = [a[1]*b[2] - a[2]*b[1],
  a[2]*b[0] - a[0]*b[2],
  a[0]*b[1] - a[1]*b[0]]
  return c

def sub(a, b):
  c = []
  for i in range(len(a)):
    c.append(a[i] - b[i])
  return c
