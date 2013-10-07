majom
=====

Helicopter-flying-thinkerings

Check out the final report for this project here! https://db.tt/IHYyPu2y

And some videos of it in action... https://drive.google.com/folderview?id=0B6FZEpnvdjNfRDU1NVcwdzd6R3M&usp=sharing



Arduino Setup
-------------

- 1 x Arduino UNO board ( + USB to serial cable) 
- 4 x IR LEDs 
- 4 x 113 ohm resistors (brown brown orange black BROWN) 
- 5 x cables / wires 

Wiring
------

- PIN8  -> LED1 + -> resistor -> \
- PIN9  -> LED2 + -> resistor ->  -> GROUND
- PIN10 -> LED3 + -> resistor -> /
- PIN11 -> LED4 + -> resistor ->

Set up steps
------------

- Install arduino software
- Download majom.ino (in src/Arduino/majom/) to arduino
- Wire up arduino board as described above

- Run "cabal configure ; cabal build" in src/Haskell
- Run "./run"
- Success!
