//Arduino code to control a helicotper.
int IRledPin1 =  12;    
int IRledPin2 = 11;
int incomingByte = 0;

void setup() {                
  // initialize the IR digital pin as an output:
  pinMode(IRledPin1, OUTPUT);      
  pinMode(IRledPin2, OUTPUT); 
  Serial.begin(9600);
}

void loop() {

  
      digitalWrite(IRledPin1, HIGH);
      digitalWrite(IRledPin2, LOW);
      delay(500);
      digitalWrite(IRledPin2, HIGH);
      digitalWrite(IRledPin1, LOW);
      delay(500);
    
}



