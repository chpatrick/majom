import pygame
import detect
from math import *

def drawArrow(srf, (c1, c2), width, direction):
  pt1 = sin(radians(direction))
  pt2 = cos(radians(direction))
  pt1 = pt1 * width
  pt2 = pt2 * width / 3
  pygame.draw.line(srf, (255,0,0), (pt1+c1, -pt2+c2), (-pt1+c1, pt2+c2))
  pygame.draw.line(srf, (255,0,0),(-pt1+c1, pt2+c2),
    (-pt1+c1 + 5*sin((pi/2)-radians(direction)),
    pt2+c2 + 5*cos((pi/2)-radians(direction))))
  pygame.draw.line(srf, (255,0,0),(-pt1+c1, pt2+c2),
    (-pt1+c1 - 5*sin((pi/2)-radians(direction)),
    pt2+c2 - 5*cos((pi/2)-radians(direction))))

def drawGrid(srf, rangex = (-10,10), rangey = (-10,10), startx=30, starty=30):
  (sx,sy) = srf.get_size()
  # draw axis
  pygame.draw.line(srf, (255,255,255), (startx,starty), (startx,sy-(starty - 5)))
  pygame.draw.line(srf, (255,255,255), (startx-5,sy-starty), (sx-startx, sy-starty))

  #draw labels on axis
  font = pygame.font.SysFont("monospace", 15)
  xs = range(rangex[0], rangex[1]+1)
  for i in range(len(xs)):
    label = font.render(str(xs[i]), 1, (255,255,255))
    srf.blit(label, 
        (startx + (i*(sx - 2*startx)/(len(xs)-1)),
        sy- (starty - 5)))

  ys = range(rangey[0], rangey[1]+1)
  for i in range(len(ys)):
    label = font.render(str(ys[len(ys) - i -1]), 1, (255,255,255))
    srf.blit(label, 
        (startx - 25,
        starty + (i*(sy - 2*starty)/(len(ys)-1) -10 ))),

def main():
  pygame.init()

  window = pygame.display.set_mode((640,480))
  font = pygame.font.SysFont("monospace", 15)
  history = []

  while True:
    try:
      pos = raw_input()
      try:
        #get input
        (x,y,z,o) = map(float, pos.split(','))
        r = radians(o)
        (sizex, sizey) = window.get_size()
        #scale value to window size
        floor = 30
        wall = 30
        offsetX = 10
        offsetY = 10
        unitx = (sizex - 2*wall)/20
        unity = (sizey - floor)/20
        px = (unitx * (x+offsetX)) + wall
        py = sizey - ((unity * (z+offsetY)) + floor)
        label = font.render(str(y), 1, (255,255,255))

        #clear window and draw new shape
        window.fill((0,0,0))
        drawGrid(window)
        pygame.draw.line(window, (255,255,255), (px-5,py-5), (px+5,py+5))
        pygame.draw.line(window, (255,255,255), (px-5,py+5), (px+5,py-5))
        pygame.draw.line(window, (255,255,255), (px,py), (px+10*sin(r),py-10*cos(r)))
        pygame.draw.line(window, (255,255,255), (px,py), (px-10*sin(r),py+10*cos(r)))
        pygame.draw.line(window, (255,255,255),
            (px-10*sin(r),py+10*cos(r)),
            (px-10*sin(r)+5*sin((pi/2)-r),py+10*cos(r)+5*cos((pi/2)-r)))
        pygame.draw.line(window, (255,255,255),
            (px-10*sin(r),py+10*cos(r)),
            (px-10*sin(r)-5*sin((pi/2)-r),py+10*cos(r)-5*cos((pi/2)-r)))
        window.blit(label, (px + 20,py +20))

        history.insert(0, (x,z))
        history = history[:5]

        if len(history) > 1:
          vels = detect.getVels(history)
          v = detect.avgVel(vels)
          d = detect.velDirection(v)
          if d != None:
            drawArrow(window, (320,100), 100, d)

        pygame.display.flip()

      except:
        print "Got", pos, "dunno wot to do lol"
    except: 
      print "Input broken. Exiting..."
      break

if __name__ == "__main__":
  main()

