#include <Servo.h>

Servo servox; // create servo object to control a servo
Servo servoy;

int iCompensX = -10;
int iCompensY = 20;
int xcoordinate = 90;
int ycoordinate = 0;
String inString = "";

void setup() {
    Serial.begin(19200);
    servox.attach(9);
    servoy.attach(8);
    servox.write(xcoordinate+iCompensX);
    servoy.write(ycoordinate+iCompensY);    
}



void loop() {
    delay(50);

    
     if (Serial.available() > 0) 
    {
      readCoordinates();     
    }  
}


void readCoordinates(){

   int x = 0;
   int y = 0; 
   String sAux = ""; 
   
   while (Serial.available() > 0) 
   {
    int inChar = Serial.read();
    if (isDigit(inChar)) {
      // convert the incoming byte to a char and add it to the string:
      inString += (char)inChar;
    }
    // if you get a newline, print the string, then the string's value:
    if (inChar == '\n') {
      x=inString.substring(0,3).toInt();
    /*  Serial.print("X:");      
      Serial.println(x); */
      y=inString.substring(3,6).toInt();      
   /*   Serial.print("Y:");
      Serial.println(inString.substring(3,6).toInt()); */
      inString = "";
      servox.write(x+iCompensX);
      servoy.write(y-90+iCompensY);    
      
    }
   }
}    


    
