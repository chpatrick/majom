//Arduino code to control a helicotper.

//TODO CHECK BINARY THING WORKS
int IRledPin1 =  8; //currently hard-coded for speed. Need to change.
int IRledPin2 = 9;
int IRledPin3 = 10;
int IRledPin4 = 11;
int pulseOneOn = 320;
int pulseOneOff = 680;
int pulseZeroOn = 320;
int pulseZeroOff = 280;
int headerOn = 2000;
int headerOff = 2000;
int footerOn = 300;
int messageLength = 120000;

int incomingInstruction;
int incomingValue;

#define YAW 0
#define PITCH 1
#define THROTTLE 2
#define CORRECTION 3

int pulseValues[32];
int pulseLength = 0;


void setup() {                
  pinMode(IRledPin1, OUTPUT); 
  pinMode(IRledPin2, OUTPUT); 
  pinMode(IRledPin3, OUTPUT);
  pinMode(IRledPin4, OUTPUT); 
  setYaw(63);
  setPitch(63);
  setThrottle(0, 1);
  setCorrection(63);

  Serial.begin(9600);
}

void setBitRange(int start, int fin, int val) {
  for(int i=fin-1; i>=start; i--) {
    pulseValues[i] = val & B1;
    val = val >> 1;

  }
}

void setYaw(int val) {
  val = clamp(0, 127, val);
  setBitRange(0,8,val);
}

void setPitch(int val) {
  val = clamp(0, 127, val);
  setBitRange(8,16,val);
}

void setThrottle(int val, int channel) {
  val = clamp(0, 127, val);
  if(channel == 2) {
    setBitRange(16,24,val+128);
  } 
  else {
    setBitRange(16,24,val);
  }
}

void setCorrection(int val) {
  val = clamp(0, 127, val);
  setBitRange(24,32,val);
}

int clamp(int a, int b, int x) {
  if(x < a) {
    return a;
  }
  else if(x > b) {
    return b;
  }
  return x;
}

void loop() {
  // send data only when you receive data:
  while (Serial.available() > 0) {
    // read the incoming instruction byte
    incomingInstruction = Serial.read();
    //Serial.flush();
    if(Serial.available() > 0) {    
      // read the incoming value byte
      incomingValue = Serial.read();
      //Serial.print(incomingInstruction, DEC);
      //Serial.print(", ");
      //Serial.println(incomingValue, DEC);

      switch(incomingInstruction) {
      case YAW:
        setYaw(incomingValue);
        break;
      case PITCH:
        setPitch(incomingValue);
        break;
      case THROTTLE:
        setThrottle(incomingValue, 1);
        break;
      case CORRECTION:
        setCorrection(incomingValue);
        break;
      }
    }
  }
  SendCode();  
  delay(25);
}

void pulseIR(long microsecs) {
  cli();  // this turns off any background interrupts

  while (microsecs > 0) {
    //IR
    PORTB = B00001111;
    delayMicroseconds(13);
    PORTB &= ~(B00001111);
    PORTB &= ~(B00001111);
    PORTB &= ~(B00001111);
    PORTB &= ~(B00001111);
    PORTB &= ~(B00001111);
    delayMicroseconds(12);
    // so 26 microseconds altogether
    microsecs -= 26;

  }

  sei();  // this turns them back on
}

void Zero()
{  
  pulseIR(pulseZeroOn);
  delayMicroseconds(pulseZeroOff);
  pulseLength += (pulseZeroOn + pulseZeroOff);
}

void One()
{
  pulseIR(pulseOneOn);
  delayMicroseconds(pulseOneOff); 
  pulseLength += (pulseOneOn + pulseOneOff);
}

void sendPulseValue(int pulseValue)
{
  if (pulseValue == 1)
    One();
  else
    Zero(); 
}

void SendCode() {

  pulseIR(headerOn);
  delayMicroseconds(headerOff);
  pulseLength= headerOn + headerOff;

  for(int i=0; i < 32; i++) {
    sendPulseValue(pulseValues[i]);
  }

  //Footer
  pulseIR(footerOn);
  delayMicroseconds(messageLength - pulseLength - footerOn); 
}


