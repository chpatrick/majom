import pygame
from math import *

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
  print ys
  for i in range(len(ys)):
    label = font.render(str(ys[len(ys) - i -1]), 1, (255,255,255))
    srf.blit(label, 
        (startx - 25,
        starty + (i*(sy - 2*starty)/(len(ys)-1) -10 ))),

def main():
  pygame.init()

  window = pygame.display.set_mode((640,480))

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
        pygame.display.flip()
      except:
        print "Got", pos, "dunno wot to do lol"
    except:
      print "Input broken. Exiting..."
      break

if __name__ == "__main__":
  main()

