import pygame

pygame.init()

window = pygame.display.set_mode((640,480))

while True:
  try:
    pos = raw_input()
    try:
      #get input
      (x,y,z) = map(float, pos.split(','))
      (sizex, sizey) = window.get_size()
      #scale value to window size
      floor = 20
      unitx = sizex/2
      unity = (sizey - floor)/20
      px = (unitx * 1) 
      py = sizey - ((unity * y) + floor)

      #clear window and draw new shape
      window.fill((0,0,0))
      pygame.draw.line(window, (255,255,255), (px-5,py-5), (px+5,py+5))
      pygame.draw.line(window, (255,255,255), (px-5,py+5), (px+5,py-5))
      pygame.display.flip()
    except:
      print "Got", pos 
  except:
    print "Input broken. Exiting..."
    break
