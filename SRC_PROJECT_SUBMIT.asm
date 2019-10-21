;-----------------------------------------
; Project 1: Reflow Oven Controller
;University of British Columbia
;
;Electrical and Computer Engineering
;ELEC 291 201 Winter 2018/2019 
;Instructor: Dr. Jesus Calvino-Fraga
;
; Group B7 
;
; Group members:
;Daniyal Bhaila    
;Erick Chmelyk
;Mark Gnocato
;Braedon Norman
;Jian Yong (Alex) Wang
;Timothy Wriglesworth

;-----------------------------------------


; This code serves as a controller for the softprocesser 8052 synthesized onto the DE1-SoC FPGA too go throw a reflow solder process


$NOLIST
$MODDE1SOC
$LIST

; Bits used to access the LTC2308
LTC2308_MISO bit 0xF8 ; Read only bit
LTC2308_MOSI bit 0xF9 ; Write only bit
LTC2308_SCLK bit 0xFA ; Write only bit
LTC2308_ENN  bit 0xFB ; Write only bit


CLK           EQU 33333333 ; Microcontroller system crystal frequency in Hz
TIMER0_RATE   EQU 4096     ; 2048Hz squarewave (peak amplitude of CEM-1203 speaker)
TIMER0_RELOAD EQU ((65536-(CLK/(12*TIMER0_RATE)))) ; The prescaler in the CV-8052 is 12 unlike the AT89LP51RC2 where is 1.
TIMER2_RATE   EQU 1000     ; 1000Hz, for a timer tick of 1ms
TIMER2_RELOAD EQU ((65536-(CLK/(12*TIMER2_RATE))))

TIMER_speed EQU 1000
PWM_TIME	EQU 0x5
AUTO_ABORT_TIME EQU 0x60
AUTO_ABORT_TEMP EQU 50


BAUD EQU 57600
TIMER_1_RELOAD EQU (256-((2*CLK)/(12*32*BAUD)))

COOL_TEMP	  EQU 0x20 
;reset is key 0 and 1- this combination also boots
SOUND_OUT      equ P1.1
pwm			  		equ P1.0
UPDOWN        	equ SWA.0



; Reset vector
org 0x0000
    ljmp startup

; External interrupt 0 vector (not used in this code)
org 0x0003
	reti

; Timer/Counter 0 overflow interrupt vector
org 0x000B
	ljmp Timer0_ISR

; External interrupt 1 vector (not used in this code)
org 0x0013
	reti

; Timer/Counter 1 overflow interrupt vector (not used in this code)
org 0x001B
	reti

; Serial port receive/transmit interrupt vector (not used in this code)
org 0x0023 
	reti
	
; Timer/Counter 2 overflow interrupt vector
org 0x002B
	ljmp Timer2_ISR

; In the 8051 we can define direct access variables starting at location 0x30 up to location 0x7F
dseg at 0x30
Count1ms:     ds 2 ; Used to determine when half second has passed
BCD_counter:  ds 1 ; The BCD counter incrememted in the ISR and displayed in the main loop

next_state: ds 1
Reflow_temp: ds 2
Start_state_machine_flag: ds 1
ON_flag:     ds 1
Soak_temp: ds 2
Soak_time: ds 1
Reflow_time: ds 1

x: 		ds 4
y: 		ds 4
bcd: 	ds 5
thermo: ds 4
junc: 	ds 4
minutes:ds 1 
;oven

OV_TIME:	  ds 1 
TIME_FSM:	  ds 1

state: ds 1
temp:        ds 4

Passcode: ds 1

; In the 8051 we have variables that are 1-bit in size.  We can use the setb, clr, jb, and jnb
; instructions with these variables.  This is how you define a 1-bit variable:
bseg
half_seconds_flag: dbit 1 ; Set to one in the ISR every time 500 ms had passed
pwm_flag:			  dbit 1
; for math32
mf:                dbit 1
Passcode_CFlag:	   dbit 1
Pass_Message_Flag: dbit 1

cseg
; These 'equ' must match the wiring between the DE1-SoC board and the LCD!
; P0 is in connector JP2.  Check "CV-8052 Soft Processor in the DE1-SoC Board: Getting
; Started Guide" for the details.
ELCD_RS equ P0.4 ;green
ELCD_RW equ P0.5 ;white
ELCD_E  equ P0.6 ; brown
ELCD_D4 equ P0.0   ;yellow
ELCD_D5 equ P0.1
ELCD_D6 equ P0.2
ELCD_D7 equ P0.3

B4 equ P2.2
B3 equ P2.4
B2 equ P2.6
B1 equ P2.7

FT93C66_CE   EQU P1.2    ;1
FT93C66_MOSI EQU P1.4    ;3 
FT93C66_MISO EQU P1.6    ;4
FT93C66_SCLK EQU P2.0    ;2



$NOLIST
$include(math32.inc)
$include(FT93C66.inc)
$include(LCD_4bit_DE1SoC.inc) ; A library of LCD related functions and utility macros
$LIST

;                     1234567890123456    <- This helps determine the location of the counter
Time_Elap:  db 'ON S1 TIME:00:xx',0
Time_Elap2: db 'ON S2 TIME:',0
Time_Elap3: db 'ON S3 TIME:',0
Time_Elap4: db 'ON S4 TIME:',0
Time_Elap5: db 'ON S5 TIME:',0
Finished:   db 'Remove PCB      ',0
clear:      db '                ',0

;                              1234567890123456    <- This helps determine the location of the counter
Initial_Message:           db 'Press button to ', 0
Display_Change_variables:  db 'Change variables', 0
Display_Start_cook:        db 'Start cooking   ', 0
Start_cooking:             db 'Cooking         ',0
blank_line:				   db '                ',0
start_change_variables:    db 'Change variable:',0
dot:					   db '*',0
;                              1234567890123456    <- This helps determine the location of the counter
s_Soak_temp:			   db 'Soak Temp:  1  C',0
S_Soak_time:			   db 'Soak Time:     s',0
s_Reflow_temp:			   db 'Reflow Temp:2  C',0
s_Reflow_time:			   db 'Reflow Time:   s',0
running_line:			   db 'S:1  ,  R:2  ,  ',0

Abort:                     db 'ABORT Press KEY3',0

s_ERPOM:                   db 'KEY3 to save    ',0
s_Oven_off:                db 'Oven OFF        ',0            

Wrong_Passcode_Message:	   db 'Invalid Password',0	

Passcode_Message:		   db 'Enter Password  ',0
Passcode_Message2:		   db 'Using SW7-0     ',0	
Passcode_Message3:		   db 'Press B1        ',0
Passcode_Message4:		   db 'To Continue     ',0


CSEG 


WaitHalfSec:
    mov R2, #89
W3: mov R1, #250

W2: mov R0, #166
W1: djnz R0, W1 ; 3 cycles->3*45.21123ns*166=22.51519us
    djnz R1, W2 ; 22.51519us*250=5.629ms
    djnz R2, W3 ; 5.629ms*89=0.5s (approximately)
    ret
 
		
;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 0                     ;
;---------------------------------;
Timer0_Init:
	mov a, TMOD
	anl a, #0xf0 ; Clear the bits for timer 0
	orl a, #0x01 ; Configure timer 0 as 16-timer
	mov TMOD, a
	mov TH0, #high(TIMER0_RELOAD)
	mov TL0, #low(TIMER0_RELOAD)
	; Enable the timer and interrupts
    setb ET0  ; Enable timer 0 interrupt
    setb TR0  ; Start timer 0
	ret

;---------------------------------;
; ISR for timer 0.  Set to execute;
; every 1/4096Hz to generate a    ;
; 2048 Hz square wave at pin P3.7 ;
;---------------------------------;
Timer0_ISR:
	;clr TF0  ; According to the data sheet this is done for us already.
	mov TH0, #high(TIMER0_RELOAD) ; Timer 0 doesn't have autoreload in the CV-8052
	mov TL0, #low(TIMER0_RELOAD)
	cpl SOUND_OUT ; Connect speaker to P1.0 
	reti

;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 2                     ;
;---------------------------------;
Timer2_Init:
	mov T2CON, #0 ; Stop timer/counter.  Autoreload mode.
	mov TH2, #high(TIMER2_RELOAD)
	mov TL2, #low(TIMER2_RELOAD)
	; Set the reload value
	mov RCAP2H, #high(TIMER2_RELOAD)
	mov RCAP2L, #low(TIMER2_RELOAD)
	; Init One millisecond interrupt counter.  It is a 16-bit variable made with two 8-bit parts
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Enable the timer and interrupts
    setb ET2  ; Enable timer 2 interrupt
    setb TR2  ; Enable timer 2
	ret

;---------------------------------;
; ISR for timer 2                 ;
;---------------------------------;
Timer2_ISR:
	clr TF2  ; Timer 2 doesn't clear TF2 automatically. Do it in ISR
;	cpl P1.1 ; To check the interrupt rate with oscilloscope. It must be precisely a 1 ms pulse.
	
	; The two registers used in the ISR must be saved in the stack
	push acc
	push psw
	
	; Increment the 16-bit one mili second counter
	inc Count1ms+0    ; Increment the low 8-bits first
	mov a, Count1ms+0 ; If the low 8-bits overflow, then increment high 8-bits
	jnz Inc_Done
	inc Count1ms+1

Inc_Done:
	; Check if half second has passed
	mov a, Count1ms+0
	cjne a, #low(TIMER_speed), Timer2_ISR_done ; Warning: this instruction changes the carry flag!
	mov a, Count1ms+1
	cjne a, #high(TIMER_speed), Timer2_ISR_done
	
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Increment the BCD counter
	mov a, BCD_counter
;	jb UPDOWN, Timer2_ISR_decrement
	add a, #0x01

	sjmp Timer2_ISR_da
Timer2_ISR_decrement:
	add a, #0x99 ; Adding the 10-complement of -1 is like subtracting 1.
Timer2_ISR_da:
	da a ; Decimal adjust instruction.  Check datasheet for more details!
	mov BCD_counter, a
	
	; Increment the TIME_FSM
	mov a, TIME_FSM
	add a, #0x01
	da a
	mov TIME_FSM, a
	
	; 500 milliseconds have passed.  Set a flag so the main program knows
	setb half_seconds_flag ; Let the main program know half second had passed
	cpl Pass_Message_Flag
	; Toggle LEDR0 so it blinks
	cpl LEDRA.0
;	cpl TR0 ; Enable/disable timer/counter 0. This line creates a beep-silence-beep-silence sound.
	; Reset to zero the milli-seconds counter, it is a 16-bit variable

; define a isr for the oven on /off routine
; use a flag to compare if we want oven on or off	
jnb pwm_flag, cont 
CTRL:
	mov R0, OV_TIME
	djnz R0, OV_CTRL
	mov OV_TIME, R0
	lcall OV_RESET
	ljmp cont
OV_CTRL:
;	mov R0, a
	mov OV_TIME, R0
	cjne R0, #0x4, cont
	clr LEDRA.1
	clr pwm
cont:
	
Timer2_ISR_done:
	pop psw
	pop acc
	reti
	
OV_RESET:
	setb LEDRA.1
	setb pwm
	mov a, #PWM_TIME
	mov OV_TIME, a
	clr a
	ret

; Look-up table for the 7-seg displays. (Segments are turn on with zero) 
T_7seg:
    DB 40H, 79H, 24H, 30H, 19H, 12H, 02H, 78H, 00H, 10H

; Displays a BCD number in HEX1-HEX0
Display_BCD_7_Seg:
	
	mov dptr, #T_7seg

	mov a, BCD_counter
	swap a
	anl a, #0FH
	movc a, @a+dptr
	mov HEX1, a
	
	mov a, BCD_counter
	anl a, #0FH
	movc a, @a+dptr
	mov HEX0, a
	
	mov a, #0x2
	anl a, #0FH
	movc a, @a+dptr
	mov HEX4, a
	
	mov a, #0x4
	anl a, #0FH
	movc a, @a+dptr
	mov HEX5, a
	
	ret
	
EPROM_Test:

	 lcall FT93C66_Write_Enable
    
     mov dptr, #0x10 ; Random memory location to test
     mov a, #0x55 ; Value to write at location
     lcall FT93C66_Write
     lcall FT93C66_Read
     cjne a, #0x55, it_failed ; Read back and check if the location was written correctly
	 setb LEDRA.5
	 setb EA
	 setb TR0
    Wait_milli_seconds(#255)
    Wait_milli_seconds(#255)
    clr TR0
	 
it_failed:
	
    ; Turn off all the LEDs
   
    clr EA   ; Enable Global interrupts
    
ret

;------------------------------------------------serial temp


Initialize_Serial_Port:
	; Configure serial port and baud rate
	clr TR1 ; Disable timer 1
	anl TMOD, #0x0f ; Mask the bits for timer 1
	orl TMOD, #0x20 ; Set timer 1 in 8-bit auto reload mode
    orl PCON, #80H ; Set SMOD to 1
	mov TH1, #low(TIMER_1_RELOAD)
	mov TL1, #low(TIMER_1_RELOAD) 
	setb TR1 ; Enable timer 1
	mov SCON, #52H
	ret

putchar:
	jbc	TI,putchar_L1
	sjmp putchar
putchar_L1:
	mov	SBUF,a
	ret
	
getchar:
	jbc	RI,getchar_L1
	sjmp getchar
getchar_L1:
	mov	a,SBUF
	ret

SendString:
    clr a
    movc a, @a+dptr
    jz SendString_L1
    lcall putchar
    inc dptr
    sjmp SendString  
SendString_L1:
	ret
		
Initialize_ADC:
	; Initialize SPI pins connected to LTC2308
	clr	LTC2308_MOSI
	clr	LTC2308_SCLK
	setb LTC2308_ENN
	ret
	

LTC2308_Toggle_Pins:
    mov LTC2308_MOSI, c
    setb LTC2308_SCLK
    mov c, LTC2308_MISO
    clr LTC2308_SCLK
    ret

; Bit-bang communication with LTC2308.  Check Figure 8 in datasheet (page 18):
; https://www.analog.com/media/en/technical-documentation/data-sheets/2308fc.pdf
; The VREF for this 12-bit x` is 4.096V
; Warning: we are reading the previously converted channel! If you want to read the
; channel 'now' call this function twice.
;
; Channel to read passed in register 'b'.  Result in R1 (bits 11 downto 8) and R0 (bits 7 downto 0).
; Notice the weird order of the channel select bits!
LTC2308_RW:
    clr a 
	clr	LTC2308_ENN ; Enable ADC
    ; Send 'S/D', get bit 11
    setb c ; S/D=1 for single ended conversion
    lcall LTC2308_Toggle_Pins
    mov acc.3, c
    ; Send channel bit 0, get bit 10
    mov c, b.2 ; O/S odd channel select
    lcall LTC2308_Toggle_Pins
    mov acc.2, c 
    ; Send channel bit 1, get bit 9
    mov c, b.0 ; S1
    lcall LTC2308_Toggle_Pins
    mov acc.1, c
    ; Send channel bit 2, get bit 8
    mov c, b.1 ; S0
    lcall LTC2308_Toggle_Pins
    mov acc.0, c
    mov R1, a
    
    ; Now receive the lest significant eight bits
    clr a 
    ; Send 'UNI', get bit 7
    setb c ; UNI=1 for unipolar output mode
    lcall LTC2308_Toggle_Pins
    mov acc.7, c
    ; Send 'SLP', get bit 6
    clr c ; SLP=0 for NAP mode
    lcall LTC2308_Toggle_Pins
    mov acc.6, c
    ; Send '0', get bit 5
    clr c
    lcall LTC2308_Toggle_Pins
    mov acc.5, c
    ; Send '0', get bit 4
    clr c
    lcall LTC2308_Toggle_Pins
    mov acc.4, c
    ; Send '0', get bit 3
    clr c
    lcall LTC2308_Toggle_Pins
    mov acc.3, c
    ; Send '0', get bit 2
    clr c
    lcall LTC2308_Toggle_Pins
    mov acc.2, c
    ; Send '0', get bit 1
    clr c
    lcall LTC2308_Toggle_Pins
    mov acc.1, c
    ; Send '0', get bit 0
    clr c
    lcall LTC2308_Toggle_Pins
    mov acc.0, c
    mov R0, a

	setb LTC2308_ENN ; Disable ADC

	ret

; Converts the 16-bit hex number in [R1,R0] to a 
; 5-digit packed BCD in [R4,R3,R2] using the
; double-dabble algorithm.
hex2bcd16:
	clr a
	mov R4, a ; Initialize BCD to 00-00-00 
	mov R3, a
	mov R2, a
	mov R5, #16  ; Loop counter.

hex2bcd16_L1:
	; Shift binary left	
	mov a, R1
	mov c, acc.7 ; This way [R1,R0] remains unchanged!
	mov a, R0
	rlc a
	mov R0, a
	mov a, R1
	rlc a
	mov R1, a
    
	; Perform bcd + bcd + carry using BCD arithmetic
	mov a, R2
	addc a, R2
	da a
	mov R2, a
	mov a, R3
	addc a, R3
	da a
	mov R3, a
	mov a, R4
	addc a, R4
	da a
	mov R4, a

	djnz R5, hex2bcd16_L1

	ret



; Display the 4-digit bcd stored in [R3,R2] using the 7-segment displays
Display_Seven_Segment:
	mov dptr, #T_7seg
	; Display the channel in HEX5
	mov a, b
	anl a, #0x0f
	movc a, @a+dptr
	mov HEX5, a
	
	;  in HEX3, HEX2, HEX1, HEX0
	mov a, bcd+1
	swap a
	anl a, #0x0f
	movc a, @a+dptr
	mov HEX3, a
	
	mov a, bcd+1
	anl a, #0x0f
	movc a, @a+dptr
	mov HEX2, a
	
	mov a, bcd+0
	swap a
	anl a, #0x0f
	movc a, @a+dptr
	mov HEX1, a
	
	mov a, bcd+0
	anl a, #0x0f
	movc a, @a+dptr
	mov HEX0, a
	
	ret

; Send a 4-digit BCD number stored in [R3,R2] to the serial port	
SendNumber:
	mov a, bcd+1
	swap a
	anl a, #0x0f
	orl a, #'0'
	lcall putchar
	
	mov a, bcd+1
	anl a, #0x0f
	orl a, #'0'
	lcall putchar
	
	mov a, bcd+0
	swap a
	anl a, #0x0f
	orl a, #'0'
	lcall putchar
	
	mov a, bcd+0
	anl a, #0x0f
	orl a, #'0'
	lcall putchar
	
	mov a, #'\n'
	lcall putchar
	mov a, #'\r'
	lcall putchar
	ret
	
; Wait 1 millisecond using Timer 0
Wait1ms:
	mov	TH2, #high(TIMER2_RELOAD)
	mov	TL2, #low(TIMER2_RELOAD)
	clr	TF2
	setb TR2
	jnb	TF2,$
	clr	TR2
	ret
; Wait R2 milliseconds
MyDelay:
	lcall Wait1ms
    djnz R2, MyDelay
	ret
	
Do_Cold_Junction:     ;get the value from lm355 in volts and calculate the temperature in degrees celsius
	mov x+0, R0
	mov x+1, R1
	mov x+2, #0
	mov x+3, #0
	load_y(2730)
	lcall sub32
	load_y(10)
	lcall div32
	mov junc+0, x+0
    mov junc+1, x+1
    mov junc+2, x+2
    mov junc+3, x+3
	ret


Conversion:                     ;   get value from thermocouple in mV and convert it to degrees celsius
    mov x+0, R0
	mov x+1, R1
	mov x+2, #0
	mov x+3, #0
    load_y(1000)
    lcall mul32
    load_y(10200)
    lcall div32
    mov thermo+0, x+0
    mov thermo+1, x+1
    mov thermo+2, x+2
    mov thermo+3, x+3
   
	ret

	
	;----------------------------
; Serial and Temperature Code
;----------------------------
	
	
ADC_Readings:
	mov R7, #100
	load_x(0)
loop_measure:
	mov b, #0 
	lcall LTC2308_RW  ; Read the cold junction
	lcall Wait1ms
	mov b, #0
	lcall LTC2308_RW  ; Read the channel from the ADC again
	mov y+0, r0
	mov y+1, r1
	mov y+2, #0
	mov y+3, #0
	lcall add32
	djnz r7, loop_measure  ;average out the readings- software fix for 3degree temperature accuaracy alongside of hardware fix from using better GND
	load_y(100)
	lcall div32
	mov r0, x+0
	mov r1, x+1
   lcall Conversion
    ;read channel 2 (the Thermo couple)
    mov r7, #100
    load_x(0)
loop_thermo:
   	mov b, #2
	lcall LTC2308_RW  ; Read the channel from the ADC
	lcall Wait1ms
	mov b, #2
	lcall LTC2308_RW  ; Read the channel from the ADC again
	mov y+0, r0
	mov y+1, r1
	mov y+2, #0
	mov y+3, #0
	lcall add32
	djnz r7, loop_thermo
	load_y(100)          ;average out the readings- software fix for 3degree temperature accuaracy alongside of hardware fix from using better GND
	lcall div32
	mov r0, x+0
	mov r1, x+1
    lcall Do_Cold_Junction
    
	clr tr0
	                                    
	mov x+0, thermo+0                         ; need to add cold junction plus thermo for the overall accurate oven temperature
	mov x+1, thermo+1
	mov x+2, thermo+2
	mov x+3, thermo+3
	mov y+0, junc+0
	mov y+1, junc+1
	mov y+2, junc+2
	mov y+3, junc+3
	
	clr tr0
	
	lcall add32

	
	load_y(10)
	lcall sub32
	lcall Wait1ms
	lcall hex2bcd   ; Convert to bcd
	lcall Display_Seven_Segment ; Display using the 7-segment displays
	lcall SendNumber  ; Send to serial port
	
	mov temp+0,x+0
	mov temp+1,x+1
	mov temp+2,x+2 
	mov temp+3,x+3
	
	mov R2, #250
	lcall MyDelay
ret


;---------------------------------;
; Main program. Includes hardware ;
; initialization and 'forever'    ;
; loop.                           ;
;---------------------------------;
startup:
	; Initialization
    mov SP, #0x7F
;------------------------------------    
	;Set initial variables for Menu state maschine
    mov Start_state_machine_flag,#0
    mov ON_flag, #0
    mov TIME_FSM, #0x00	; Initializer FSM timer to zero
    mov a, #PWM_TIME
    mov OV_TIME, a
    mov temp, #0x0
    mov minutes, #0
	clr Pass_Message_Flag
	
    clr pwm
    ;setb P1.1
  
    mov Soak_temp,#160   ;160 default
    mov Soak_time,#0x75    ;75 default
    mov Reflow_temp,#240 ;240 default
    mov Reflow_time,#0x40  ;40 default
    mov Passcode, #01001101B ; password 0 Default
	
;---------------------------------------
; ADC setup
;-----------------------------------

	lcall Initialize_Serial_Port
	lcall Initialize_ADC
	
	
	;-----------------
	;EPROM setup
	;-----------------------------
	lcall Timer0_Init
    lcall Timer2_Init
    ; We use the pins of P0 to control the LCD.  Configure as outputs.
    mov P0MOD, #01111111b ; P0.0 to P0.6 are outputs.  ('1' makes the pin output)
    ; We use pins P1.0 and P1.1 as outputs also.  Configure accordingly.
    mov P1MOD, #00000011b ; P1.0 and P1.0 are outputs
	
	 mov LEDRA, #0 ; LEDRA is bit addressable
    mov LEDRB, #0 ; LEDRB is NOT bit addresable
	
	mov a, P1MOD    ;next set configures EPROM
	ORL a, #00010100B
	mov P1MOD, a 
	mov a, P2MOD
	ORL a , #00000001B
	mov P2MOD, a
	clr FT93C66_CE
    lcall FT93C66_INIT_SPI
    lcall ELCD_4BIT ; Configure LCD in four bit mode
    ; For convenience a few handy macros are included in 'LCD_4bit_DE1SoC.inc':
    
	;----------------------------end Eprom configuration code
	clr tr0

	
main:	
	
;------------------------------;
; Oven select Parameters State as well as before starting correct password must be entered:
;------------------------------;

Home:
    ;Display Initial String of text
    Set_Cursor(1,1)
	Send_Constant_String(#Initial_message)
	Set_Cursor(2,1)
	Send_Constant_String(#Display_Change_variables)
    
    ljmp State_0
    
change_next_state0:  ; two main menu options: can choose to pick variables or choose to start reflow process
	jnb B1,$
	mov a,ON_flag
	cjne a,#0,set_to_off
	Set_Cursor(2,1)
	Send_Constant_String(#Display_Start_cook)
	mov ON_flag,#1 ; set on
	ljmp ON_flag_set
set_to_off:
	mov ON_flag,#0x0; set off
	Set_Cursor(2,1)
	Send_Constant_String(#Display_Change_Variables)
	ljmp ON_flag_set
	
set_start:
	jnb B2,$
	
	;password protection
	lcall Passcode_Check	
	jb Passcode_CFlag, Wrong_Passcode_jmp

	mov Start_state_machine_flag,#1
	ljmp set_start_return  ;-------------------------------------------------------------------------------
    
Wrong_Passcode_jmp:
	ljmp Wrong_Passcode

Start_state_machine_jmp:
	ljmp Start_state_machine

State_0:
	jnb B1, change_next_state0
ON_flag_set:
	jnb B2, set_start
set_start_return: ;----------------------------------------------------------------------------------------
	mov a,Start_state_machine_flag
	cjne a,#0,Start_state_machine_jmp
	sjmp State_0 

Passcode_Check:    ; user will be promted to enter correct password to proceed. Acess only allowed when correct switches set up.
Pass_Msg_P1:
	jb Pass_Message_Flag, Pass_Msg_P2
	Set_Cursor(1,1)
	Send_Constant_String(#Passcode_Message)
	Set_Cursor(2,1)
	Send_Constant_String(#Passcode_Message2)
	sjmp Pass_Msg_End
Pass_Msg_P2:
	Set_Cursor(1,1)
	Send_Constant_String(#Passcode_Message3)
	Set_Cursor(2,1)
	Send_Constant_String(#Passcode_Message4)
	
Pass_Msg_End:
	;-----------------------------------------***********************************
	jb B1, Passcode_Check
	Wait_milli_seconds(#50)
	jb B1, Passcode_Check_jmp
	jnb B1, $
	
	mov a, SWA
	mov LEDRA, a
	mov a, SWA
	mov R1, #Passcode
	
	subb a, R1
	
	cjne a, #0, Wrong_Passcode_Flag
	clr Passcode_CFlag
	ret
	
Passcode_Check_jmp:
	ljmp Passcode_Check
	
Wrong_Passcode_Flag:
	setb Passcode_CFlag ; flag of 1 = wrong password
	ret
	
Wrong_Passcode:    
	Set_Cursor(1,1)
	Send_Constant_String(#Wrong_Passcode_Message)
	lcall WaitHalfSec
	ljmp Passcode_Check

	
Start_state_machine:
	mov a,ON_flag
	cjne a,#0,go_cooking
	ljmp set_variables

go_cooking:
	Set_Cursor(1,1)
	Send_Constant_String(#Start_cooking)
	Set_Cursor(2,1)
	Send_Constant_String(#running_line)
	Set_Cursor(2,4)
	mov x+0, soak_temp+0
	mov x+1, soak_temp+1
	mov x+2, #0
	mov x+3, #0
	lcall hex2bcd
	Display_BCD(bcd+0)
	Set_Cursor(2,7)
	Display_BCD(soak_time)
	Set_Cursor(2,12)
	mov x+0, reflow_temp+0
	mov x+1, reflow_temp+1
	mov x+2, #0
	mov x+3, #0
	lcall hex2bcd
	Display_BCD(bcd+0)
	Set_Cursor(2,15)
	Display_BCD(reflow_time)
waiting:	
	Set_Cursor(1,9)
	Send_Constant_String(#dot)
	lcall WaitHalfsec
	Set_Cursor(1,10)
	Send_Constant_String(#dot)
	lcall WaitHalfsec
	Set_Cursor(1,11)
	Send_Constant_String(#dot)
	lcall WaitHalfsec
	Set_Cursor(1,9)
	Send_Constant_String(#blank_line)
	lcall WaitHalfsec
	Set_Cursor(1, 1)
    Send_Constant_String(#Time_Elap)
    ;Set_Cursor(2, 1)
    ;Send_Constant_String(#Menu)
    ;setb half_seconds_flag
	mov BCD_counter, #0x00 ; Initialize counter to zero
	mov state, #1
	setb EA   ; Enable Global interrupts
	setb TR0
	mov R2, #250
	lcall MyDelay
    clr TR0
    setb tr2
	
	ljmp loop   ; we want to pass control to the next state machine-> let it take over after we are happy with selectable parameters
	
set_variables:
	Set_Cursor(1,1)
	Send_Constant_String(#Start_change_variables)
r_soak_temp:
	jnb B1,$
	Set_Cursor(2,1)
	Send_Constant_String(#s_Soak_temp)
check_soak_temp:
	mov a,soak_temp
	lcall sub_from_variable
	lcall add_to_variable
	lcall display_var
	jnb B1,r_soak_time
	mov soak_temp, a
	lcall return_home
	sjmp check_soak_temp
	;MENU options and selectibilty
r_soak_time:
	jnb B1,$
	mov soak_temp,a
	Set_Cursor(2,1)
	Send_Constant_String(#s_Soak_time)
check_soak_time:
	mov a,soak_time
	lcall sub_from_variable
	lcall add_to_variable
	lcall display_var
	jnb B1, r_reflow_temp
	mov soak_time, a
	lcall return_home
	sjmp check_soak_time
r_reflow_temp:
	jnb B1,$
	Set_Cursor(2,1)
	Send_Constant_String(#s_Reflow_temp)
check_reflow_temp:
	mov a,reflow_temp
	lcall sub_from_variable
	lcall add_to_variable
	lcall display_var
	jnb B1,r_reflow_time
	mov reflow_temp, a
	lcall return_home
	sjmp check_reflow_temp
r_reflow_time:
	jnb B1,$
	Set_Cursor(2,1)
	Send_Constant_String(#s_Reflow_time)
check_reflow_time:
	mov a,reflow_time
	lcall sub_from_variable
	lcall add_to_variable
	lcall display_var
	jnb B1,check_if_done
	mov reflow_time, a
	lcall return_home
	;sjmp check_reflow_time
	
	
;----------------------------------------NEW EPROM CODE
Save_Eprom:
    Set_Cursor(2,1)
	Send_Constant_String(#s_ERPOM)
Save_Eprom_loop:
	lcall return_Home
	jnb KEY.3,Save_OP
	jnb B1,check_if_done
	sjmp Save_Eprom
	
Save_OP:
  	
  	lcall EPROM_Test
  	mov Start_state_machine_flag,#0
	ljmp Home 
  		
check_if_done:
	ljmp r_soak_temp
	
	
display_var:
	Set_Cursor(2,14)
	mov x+0, a
	mov x+1, #0
	mov x+2, #0
	mov x+2, #0
	
	lcall hex2bcd
	Display_BCD(BCD+0)
	ret	

;lcall subs one from a when button 3 is pressed	
sub_from_variable:
	jnb B3,do_sub
	ret
do_sub:
	jnb B3,$
	add a,#0x99
	da a
	ret	

;lcall adds one to a when button 2 is pressed
add_to_variable:
	jnb B2,do_add
	ret
do_add:
	jnb B2,$
	add a,#1
	da a
	ret
	
;lcall return home when button 4 is pressed	
return_home:
	jnb B4,do_return
	ret
do_return:
	jnb B4,$
	mov Start_state_machine_flag,#0
	
	
save_select:		
  	Set_Cursor(1,1)
	Send_Constant_String(#s_ERPOM)
	ljmp Home
	; After initialization the program stays in this 'forever' loop


;-----------------------------
;loop structure: here we update screen values and check for abort issues
;---------------------------


Aboort1:
    ljmp Aboort
loop:
	clr tr0
	jnb KEY.2, Aboort1
	
	    
    mov a, BCD_counter
   	cjne a, #0x59, cont_inc_time
   	clr a
	mov BCD_counter, a
	inc minutes

	
cont_inc_time:  
	jb KEY.1, loop_a  ; if the KEY1 button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit_DE1SoC.inc'
	jb KEY.1, loop_a  ; if the KEY1 button is not pressed skip
	jnb KEY.1, $		; Wait for button release.  The '$' means: jump to same instruction.
	; A valid press of the 'BOOT' button has been detected, reset the BCD counter.
	; But first stop timer 2 and reset the milli-seconds counter, to resync everything.
	clr TR2 ; Stop timer 2
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	mov TIME_FSM, a
	; Now clear the BCD counter
	mov BCD_counter, a
;	setb ledra.2
	setb TR2    ; Start timer 2

cont1:
	sjmp loop_b ; Display the new value
	
;	mov a, state
loop_a:
	jnb half_seconds_flag, loop
loop_b:
	mov a, TIME_FSM
	clr ledra.2
	cjne a, #0x05, cont2
	setb ledra.2

cont2:
	
	lcall ADC_Readings
	setb tr2
    clr half_seconds_flag ; We clear this flag in the main loop, but it is set in the ISR for timer 2
	Set_Cursor(1, 15)     ; the place in the LCD where we want the BCD counter value
	Display_BCD(BCD_counter) ; This macro is also in 'LCD_4bit_DE1SoC.inc'
	Set_Cursor(1, 12)     ; the place in the LCD where we want the BCD counter value
	mov x+0, minutes
	mov x+1, #0
	mov x+2, #0
	mov x+2, #0
	
	lcall hex2bcd
	Display_BCD(BCD+0)	
	;lcall Display_BCD_7_Seg ; Also display the counter using the 7-segment displays.****************************
 
	mov a, state
	ljmp start_state
state_return:	
    ljmp loop

;-----------------------
; Start of State Maschine
;----------------------	
SOAK_jmp:
ljmp SOAK
start_state:


RAMP_TO_SOAK:
    cjne a, #1, SOAK_jmp
    setb pwm ;power=100%
    clr pwm_flag
    setb ledra.3
    clr ledra.4  

    
;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,;
;																;
;						Auto Aboort								;
;																;
;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,;	
	mov a, TIME_FSM												
	cjne a, #AUTO_ABORT_TIME, ramp_soak_continue											
	clr c 
	mov a, #AUTO_ABORT_TEMP														
	subb a, temp													
	jc ramp_soak_continue											
	ljmp aboort													
;,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,;
    
ramp_soak_continue:   
    clr a
    mov a, soak_temp
    clr c
    subb a, temp
    da a
    jnc RAMP_TO_SOAK_DONE
    clr ledra.1
    clr pwm
    mov OV_TIME, #PWM_TIME
    mov TIME_FSM, #0
    mov state, #2
    setb TR0
    
	mov R2, #250
	lcall MyDelay
 	clr TR0
 	setb tr2
	 
RAMP_TO_SOAK_DONE:
    ljmp state_return

SOAK:
	cjne a, #2, RAMP_TO_PEAK
;	clr pwm 
	Set_Cursor(1,1)
    Send_Constant_String(#Time_Elap2)
	clr ledra.3
	setb ledra.4
	setb pwm_flag ;flag for power=20%
	mov a, soak_time       
	clr c
	subb a, TIME_FSM
	jnc SOAK_DONE
	clr ledra.1
	mov OV_TIME, #PWM_TIME
	mov TIME_FSM, #0
	mov state, #3
	setb TR0
	
	mov R2, #250
	lcall MyDelay
    clr TR0
    setb tr2

SOAK_DONE:
	ljmp state_return

RAMP_TO_PEAK:
	cjne a, #3, REFLOW
	Set_Cursor(1,1)
	Send_Constant_String(#Time_Elap3)
	setb pwm ;power=100%
	clr pwm_flag
	clr ledra.4
	setb ledra.3
	clr a
    mov a, reflow_temp
    clr c
    subb a, temp
	jnc RAMP_TO_PEAK_DONE
	clr ledra.1
	clr pwm
	mov OV_TIME, #PWM_TIME
	mov TIME_FSM, #0
	mov state, #4
	setb TR0
	mov R2, #250
	lcall MyDelay
    clr TR0
    setb tr2

RAMP_TO_PEAK_DONE:
	ljmp state_return

REFLOW:
	cjne a, #4, COOLING
	Set_Cursor(1,1)
	Send_Constant_String(#Time_Elap4)
;	clr pwm
	setb pwm_flag ;flag for power=20%
	setb ledra.4
	clr ledra.3
	mov a, reflow_time
	clr c
	subb a, TIME_FSM
	jnc REFLOW_DONE
	clr ledra.1
	mov OV_TIME, #PWM_TIME
	mov TIME_FSM, #0x0
	mov state, #5
	setb TR0
	mov R2, #250
	lcall MyDelay

	mov R2, #250
	lcall MyDelay
	mov R2, #250
	lcall MyDelay
    clr TR0
    setb tr2
	;ljmp COOLING
	
REFLOW_DONE:
	ljmp state_return

state0_go:
	ljmp Home
	
COOLING_DONE:
	ljmp state_return 

COOLING:
	cjne a, #5, state0_go
	Set_Cursor(1,1)
	Send_Constant_String(#Time_Elap5)
	clr pwm ;power=0%
	clr pwm_flag
	setb ledra.4
	clr ledra.3

	mov a, #60
	subb a, temp
	jc COOLING_DONE
	clr ledra.1
	setb ledra.6
	mov OV_TIME, #PWM_TIME
	mov TIME_FSM, #0x0
	setb TR0
	Wait_milli_seconds(#255)
;		Wait_milli_seconds(#255)
	clr TR0
    Wait_milli_seconds(#255)
;    	Wait_milli_seconds(#255)
	setb TR0
    Wait_milli_seconds(#255)
 ;   	Wait_milli_seconds(#255)
	clr TR0
    Wait_milli_seconds(#255)
;    	Wait_milli_seconds(#255)
	setb TR0
    Wait_milli_seconds(#255)
;    	Wait_milli_seconds(#255)
	clr TR0
    Wait_milli_seconds(#255)
;    	Wait_milli_seconds(#255)
	setb TR0
    Wait_milli_seconds(#255)
;    	Wait_milli_seconds(#255)
    clr TR0
    Wait_milli_seconds(#255)
    	setb TR0
    Wait_milli_seconds(#255)
  ;  	Wait_milli_seconds(#255)
    clr TR0
    Wait_milli_seconds(#255)
    	setb TR0
    Wait_milli_seconds(#255)
 ;   	Wait_milli_seconds(#255)
    clr TR0
    setb tr2
    Set_Cursor(1, 1) 
    Send_Constant_String(#Finished)
    lcall WaitHalfSec
    lcall WaitHalfSec   ; this delay just allows user to see the oven is done cooking
    lcall WaitHalfSec
    lcall WaitHalfSec
    lcall WaitHalfSec
    lcall WaitHalfSec
    lcall WaitHalfSec
    lcall WaitHalfSec
    lcall WaitHalfSec
    lcall WaitHalfSec
	ljmp startup
	

Aboort:

	Set_Cursor(1, 1)    ; in aboort sound an alarm to let user know something is wrong
	Send_Constant_String(#Abort)
	Set_Cursor(2, 1) 
	Send_Constant_String(#s_Oven_off)
	setb TR0
    Wait_milli_seconds(#255)
    Wait_milli_seconds(#255)
    clr TR0
    Wait_milli_seconds(#255)
    setb TR0
    Wait_milli_seconds(#255)
    Wait_milli_seconds(#255)
    clr TR0
    Wait_milli_seconds(#255)
    setb TR0
    Wait_milli_seconds(#255)
   	Wait_milli_seconds(#255)
    clr TR0
    Wait_milli_seconds(#255)
    setb TR0
    Wait_milli_seconds(#255)
   	Wait_milli_seconds(#255)
   	Wait_milli_seconds(#255)
   	Wait_milli_seconds(#255)
    clr TR0
	clr pwm
	clr EA
	;mov BCD_Counter, #00

loop_abort:
	clr tr0
	lcall ADC_Readings
	clr tr0
	jnb KEY.3, exit
	ljmp loop_abort
exit:
	ljmp Startup
END
