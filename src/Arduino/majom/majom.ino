//Arduino code to control a helicotper.
int IRledPin =  12;    
int incomingByte = 0;
int pulseValues[32];
int pulseLength = 0;


void setup() {                
  // initialize the IR digital pin as an output:
  pinMode(IRledPin, OUTPUT); 
  Serial.begin(9600);
  
  for (int i=0; i < 13; i++) 
    pulseValues[i] = 0;
  //yaw
  pulseValues[0] = 0;
  pulseValues[1] = 0;
  pulseValues[2] = 1;
  pulseValues[3] = 1;
  pulseValues[4] = 1;
  pulseValues[5] = 1;
  pulseValues[6] = 1;
  pulseValues[7] = 1;
  //pitch
  pulseValues[8] = 0;
  pulseValues[9] = 0;
  pulseValues[10] = 1;
  pulseValues[11] = 1;
  pulseValues[12] = 1;
  pulseValues[13] = 1;
  pulseValues[14] = 1;
  pulseValues[15] = 1;
  //throttle
  pulseValues[16] = 0;
  pulseValues[17] = 0;
  pulseValues[18] = 1;
  pulseValues[19] = 1;
  pulseValues[20] = 1;
  pulseValues[21] = 1;
  pulseValues[22] = 1;
  pulseValues[23] = 1;
  //correction
  pulseValues[24] = 0;
  pulseValues[25] = 0;
  pulseValues[26] = 1;
  pulseValues[27] = 1;
  pulseValues[28] = 1;
  pulseValues[29] = 1;
  pulseValues[30] = 1;
  pulseValues[31] = 1;
  
}

void loop() {
      SendCode();  
}

void pulseIR(long microsecs) {
  cli();  // this turns off any background interrupts

  while (microsecs > 0) {
    // 38 kHz is about 13 microseconds high and 13 microseconds low
    digitalWrite(IRledPin, HIGH);  // this takes about 3 microseconds to happen
    delayMicroseconds(10);         // hang out for 10 microseconds
    digitalWrite(IRledPin, LOW);   // this also takes about 3 microseconds
    delayMicroseconds(10);         // hang out for 10 microseconds

    // so 26 microseconds altogether
    microsecs -= 26;

  }

  sei();  // this turns them back on
}

void Zero()
{  
  pulseIR(320);
  delayMicroseconds(280);
  pulseLength += 600;
}

void One()
{
  pulseIR(320);
  delayMicroseconds(680); 
  pulseLength += 1000;
}

void sendPulseValue(int pulseValue)
{
  if (pulseValue == 1)
    One();
  else
    Zero(); 
}

void SendCode() {

  while (true)
  {

    pulseIR(2000);
    delayMicroseconds(2000);
    pulseLength=4000;

    sendPulseValue(pulseValues[0]);
    sendPulseValue(pulseValues[1]);
    sendPulseValue(pulseValues[2]);
    sendPulseValue(pulseValues[3]);
    sendPulseValue(pulseValues[4]);
    sendPulseValue(pulseValues[5]);
    sendPulseValue(pulseValues[6]);
    sendPulseValue(pulseValues[7]);
    sendPulseValue(pulseValues[8]);
    sendPulseValue(pulseValues[9]);
    sendPulseValue(pulseValues[10]);
    sendPulseValue(pulseValues[11]);
    sendPulseValue(pulseValues[12]);
    sendPulseValue(pulseValues[13]);
    sendPulseValue(pulseValues[14]);
    sendPulseValue(pulseValues[15]);
    sendPulseValue(pulseValues[16]);
    sendPulseValue(pulseValues[17]);
    sendPulseValue(pulseValues[18]);
    sendPulseValue(pulseValues[19]);
    sendPulseValue(pulseValues[20]);
    sendPulseValue(pulseValues[21]);
    sendPulseValue(pulseValues[22]);
    sendPulseValue(pulseValues[23]);
    sendPulseValue(pulseValues[24]);
    sendPulseValue(pulseValues[25]);
    sendPulseValue(pulseValues[26]);
    sendPulseValue(pulseValues[27]);
    sendPulseValue(pulseValues[28]);
    sendPulseValue(pulseValues[29]);
    sendPulseValue(pulseValues[30]);
    sendPulseValue(pulseValues[31]);

    //Footer
    pulseIR(320); 
    delayMicroseconds( (28640 - pulseLength) ); 
   } 
}
