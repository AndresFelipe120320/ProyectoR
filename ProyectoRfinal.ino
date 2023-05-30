// SENSOR DHT11
const int pinAnalog = A4;

// lectura dato app inventor
char estado;
//dht DHT;
// Pines para el motor
int IN1 = 9;
int IN2 = 8;
int IN3 = 7;
int IN4 = 6;
int ENA = 10;
int ENB = 5;
//PINES PARA LDR 
const int ldrPin = A1; 
// Pines para sensor ultrasonico
// Definir pines de entrada y salida
   const int trig = 4;
   const int echo = 2;
   long duracion;
   int distancia;
   int valor_limite= 400;// Fija el valor limite en el que se activa la alarma    
   float valor_alcohol;

   const int sensorPin = A3; // Pin analógico conectado al sensor TPH
   const float voltageReference = 5.0; // Tensión de referencia de Arduino (en voltios)
   const float voltageResolution = voltageReference / 1023.0;

//void motores(void);
void Sensor_MQ3(void);
void Sensor_LDR(void);
void Sensor_DHT11(void);
void Sensor_proximidad(void);
//pines para MQ3

void setup() {
  Serial.begin(9600);
  //Sensor DHL11
  

  pinMode(IN1, OUTPUT);
  pinMode(IN2, OUTPUT);
  pinMode(IN3, OUTPUT);
  pinMode(IN4, OUTPUT);
  pinMode(ENA, OUTPUT);
  pinMode(ENB, OUTPUT);

  //Sensor ultrasonido
  pinMode(trig, OUTPUT);
  pinMode(echo, INPUT);

  //Sensor MQ3
  //Serial.println("LABEL,valor_alcohol,porcentaje");
 // valor_alcohol=analogRead(A0);
 // pinMode(13,OUTPUT); 
  
  //Sensor LDR
 

}

void loop() {


if (Serial.available() > 0) {
    estado = Serial.read();
    //Serial.println(estado);
  
 
 switch(estado){

   //ADELANTE
   case 'A':
     analogWrite(ENA, 200);
     analogWrite(ENB, 200);
     digitalWrite(IN1, 0);
     digitalWrite(IN2, 1);
     digitalWrite(IN3, 0);
     digitalWrite(IN4, 1);
   break;
   //ATRAS
   case 'B':
    analogWrite(ENA, 200);
    analogWrite(ENB, 200);
    digitalWrite(IN2, 0);
    digitalWrite(IN1, 1);
    digitalWrite(IN3, 1);
    digitalWrite(IN4, 0);
   break;

   //DERECHA
   case 'C':
   analogWrite(ENA, 50);
     analogWrite(ENB, 100); 
     digitalWrite(IN4, 1);
     digitalWrite(IN2, 0);
     digitalWrite(IN3, 0);
     digitalWrite(IN1, 1);
        break;

  //IZQUIERDA
   case 'D':
   
     analogWrite(ENA, 100);
     analogWrite(ENB, 50);
     digitalWrite(IN3, 1);
     digitalWrite(IN2, 1);
     digitalWrite(IN1, 0);
     digitalWrite(IN4, 0);

   break;

case 'E':

    analogWrite(ENA, 0);
    analogWrite(ENB, 0); 
    digitalWrite(IN4, 0);
    digitalWrite(IN2, 0);
    digitalWrite(IN3, 0);
    digitalWrite(IN1, 0);
   break;

case 'F':
 if(estado=='F'){

   //SENSOR MQ3----------------------------------
   
  valor_alcohol=analogRead(A0);
  //Serial.println(valor_alcohol);       // Envia al Serial el valor leido del Sensor MQ3 
  float porcentaje=(valor_alcohol/10000);  //calcula el porcentaje
  //Serial.println(porcentaje);
  delay(500);
  
   

 //LDR--------------------------------------
 
  int ldrValue = analogRead(ldrPin);  // Leer el valor analógico del LDR

  // Mostrar el valor leído en el Monitor Serie
  //Serial.print("Valor LDR: ");
  //Serial.println(ldrValue);
  delay(500);  // Esperar 1 segundo antes de realizar la siguiente lectura


  //DHT----------------------------------------
  
  int sensorValue = analogRead(pinAnalog);
  float millivolts = (sensorValue / 1024.0) * 5000;  // Convertir lectura a milivoltios
  float tempCelsius = (millivolts - 500) / 10;  // Calcular temperatura en grados Celsius
  
 // Serial.print("Temperatura: ");
  
 // Serial.println(" °C");
  
  delay(500); 
 

//ULTRASONICO----------------------------------

  // Establecer el pin trig en alto durante 10 microsegundos
  digitalWrite(trig, HIGH);
  delayMicroseconds(10);
  digitalWrite(trig, LOW);

  // Medir el tiempo de ida y vuelta del pulso
  duracion = pulseIn(echo, HIGH);

  // Calcular la distancia en centímetros
  distancia = duracion * 0.034 / 2;
  Serial.print(distancia); 
  Serial.print(","); 
  Serial.print(porcentaje);
  Serial.print(","); 
  Serial.print(ldrValue);
  Serial.print(",");
  Serial.print(tempCelsius);
  Serial.println();
 }
    break;
   }
 }
}
