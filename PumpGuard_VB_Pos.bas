'****************************************************************
'*  Name    : UNTITLED.BAS                                      *
'*  Author  : Peter W Truman                                    *
'*  Notice  : Copyright (c) 2025 PCT Remote Sensing Pty Ltd     *
'*          : All Rights Reserved                               *
'*  Date    : 24/07/2025                                        *
'*  Version : 1.0                                               *
'*  Notes   :                                                   *
'*          :                                                   *
'****************************************************************



'NOTEs

Device = 18F2525

Config_Start
  OSC = INTIO67	;Internal oscillator block, port function on RA6 and RA7
  FCMEN = OFF	;Fail-Safe Clock Monitor disabled
  IESO = OFF	;Oscillator Switchover mode disabled
  PWRT = OFF	;PWRT disabled
  BOREN = SBORDIS	;Brown-out Reset enabled in hardware only (SBOREN is disabled)
  BORV = 3	;Minimum setting
  WDT = OFF	;WDT disabled (control is placed on the SWDTEN bit)
  WDTPS = 32768	;1:32768
  CCP2MX = PORTC	;CCP2 input/output is multiplexed with RC1
  PBADEN = OFF	;PORTB<4:0> pins are configured as digital I/O on Reset
  LPT1OSC = OFF	;Timer1 configured for higher power operation
  MCLRE = On	;MCLR pin enabled; RE3 input pin disabled
  STVREN = On	;Stack full/underflow will cause Reset
  LVP = OFF	;Single-Supply ICSP disabled
  XINST = OFF	;Instruction set extension and Indexed Addressing mode disabled (Legacy mode)
  Debug = OFF	;Background debugger disabled, RB6 and RB7 configured as general purpose I/O pins
  Cp0 = OFF	;Block 0 (000800-003FFFh) not code-protected
  CP1 = OFF	;Block 1 (004000-007FFFh) not code-protected
  CP2 = OFF	;Block 2 (008000-00BFFFh) not code-protected
  CPB = OFF	;Boot block (000000-0007FFh) not code-protected
  CPD = OFF	;Data EEPROM not code-protected
  WRT0 = OFF	;Block 0 (000800-003FFFh) not write-protected
  WRT1 = OFF	;Block 1 (004000-007FFFh) not write-protected
  WRT2 = OFF	;Block 2 (008000-00BFFFh) not write-protected
  WRTC = OFF	;Configuration registers (300000-3000FFh) not write-protected
  WRTB = OFF	;Boot Block (000000-0007FFh) not write-protected
  WRTD = OFF	;Data EEPROM not write-protected
  EBTR0 = OFF	;Block 0 (000800-003FFFh) not protected from table reads executed in other blocks
  EBTR1 = OFF	;Block 1 (004000-007FFFh) not protected from table reads executed in other blocks
  EBTR2 = OFF	;Block 2 (008000-00BFFFh) not protected from table reads executed in other blocks
  EBTRB = OFF	;Boot Block (000000-0007FFh) not protected from table reads executed in other blocks
Config_End

OSCCON = %01110000 ' IRCF = 111 for 8 MHz
OSCTUNE.6 = 1 ' Enable PLL for x4 (8 MHz * 4 = 32 MHz)
ADCON1 = $0F       ' All pins digital

All_Digital = True
Declare Xtal = 32
Declare PORTB_Pullups=On

'Definition
Symbol _BUZZER  = PORTC.2
Symbol _PNP1    = PORTA.4
Symbol _PNP4    = PORTB.3
Symbol _PNP2    = PORTB.4
Symbol _PNP3    = PORTB.5
Symbol _SP1     = PORTC.0
Symbol _Out     = PORTC.1

' RTC Interrupt
Symbol RTC_INT = PORTB.0


' Rotary Encoder Definitions
Symbol _ENC_A = PORTB.1
Symbol _ENC_B = PORTB.2
Symbol _ENC_SW = PORTB.6

TRISA = %00010000
TRISB = %01000111
TRISC = %10000000

'I2C Pins for DS3231
Symbol SDA = PORTC.4
Symbol SCL = PORTC.3

'Setup USART 1 (Real World)
Declare Hserial_Baud = 115200
Declare Hserial_Clear = 1                        ' Enable Error clearing on received characters
Declare HRSOut_Pin = PORTB.6
Declare HRSIn_Pin = PORTB.7


'LCD Declares
Declare LCD_Type = 0
Declare LCD_DTPin = PORTA.0
Declare LCD_ENPin = PORTA.7
Declare LCD_RSPin = PORTA.6
Declare LCD_Interface = 4
Declare LCD_Lines = 4

'Variables
Dim W_EncoderPos As Word
Dim B_LastState  As Byte
Dim B_General As Byte
Dim B_Beep_Len As Byte
Dim B_AState     As Byte
Dim B_BState     As Byte
Dim B_ButtonState As Byte
Dim B_DebA       As Byte
Dim B_DebB       As Byte
Dim B_DebBtn     As Byte

' initialise debounced states
B_AState      = PORTB.1
B_BState      = PORTB.2
B_ButtonState = PORTB.6
B_LastState   = (B_AState * 2) + B_BState
B_DebA        = 0
B_DebB        = 0
B_DebBtn      = 0


Clear                                   'Start clear

' Constants

Symbol LONG_PRESS = 2000  ' 2 seconds for long press (in ms)
Symbol I2C_ADDR_DS3231 = $D0  ' DS3231 I2C address


' Timer0 configuration: prescaler 1:64 for 1ms tick @ 32MHz
T0CONbits_T0PS2 = 1        ' \ prescaler 1:64
T0CONbits_T0PS1 = 0        '  |
T0CONbits_T0PS0 = 0        ' /
T0CONbits_PSA  = 0         ' assign prescaler
T0CONbits_T0CS  = 0        ' internal clock
T0CONbits_T08BIT = 1       ' 8-bit mode
TMR0L = 256-125            ' preload for 1ms (125 counts)

'--------------------------------------------
' Interrupt setup
On_Hardware_Interrupt GoTo ISR_Handler
INTCONbits_T0IF = 0        ' clear flag
INTCONbits_T0IE = 1        ' enable Timer0 interrupt
INTCONbits_GIE = 1         ' global enable
T0CONbits_TMR0ON = 1       ' start timer




; Interrupt Handler

GoTo over_Interrupt
' Interrupt routine
ISR_Handler:
    Context Save
    Clrwdt                  ' in case watchdog enabled

    If INTCONbits_T0IF = 1 Then
        ' reload timer for 1ms
        TMR0L = 256-125
        INTCONbits_T0IF = 0

        ' debounce rotary encoder and button inputs
        Dim B_NewA  As Byte
        Dim B_NewB  As Byte
        Dim B_NewBtn As Byte

        B_NewA  = PORTB.1
        B_NewB  = PORTB.2
        B_NewBtn = PORTB.6

        If B_NewA <> B_AState Then
            Inc B_DebA
            If B_DebA >= 8 Then
                B_AState = B_NewA
                B_DebA = 0
            EndIf
        Else
            B_DebA = 0
        EndIf

        If B_NewB <> B_BState Then
            Inc B_DebB
            If B_DebB >= 8 Then
                B_BState = B_NewB
                B_DebB = 0
            EndIf
        Else
            B_DebB = 0
        EndIf

        If B_NewBtn <> B_ButtonState Then
            Inc B_DebBtn
            If B_DebBtn >= 16 Then
                B_ButtonState = B_NewBtn
                B_DebBtn = 0
            EndIf
        Else
            B_DebBtn = 0
        EndIf

        Dim B_Curr As Byte
        B_Curr = (B_AState * 2) + B_BState

        ' detect edges: Gray code sequence 00->01->11->10->00
        Dim B_Combined As Byte
        B_Combined = (B_LastState * 4) + B_Curr
        Select B_Combined
            Case 0b0001, 0b0111, 0b1110, 0b1000
                Dec W_EncoderPos
            Case 0b0010, 0b1011, 0b1101, 0b0100
                Inc W_EncoderPos
        EndSelect
        B_LastState = B_Curr
    EndIf
    'Piezzo
    If B_Beep_Len > 0 Then
        High _BUZZER
        Dec B_Beep_Len
    Else
        Low _BUZZER    
    EndIf   



    Context Restore







over_Interrupt:






' Buzzer Startup Procedure
Proc BuzzerStartup()
  Dim cycle As Byte
  For cycle = 1 To 5
    P_Buzzer(2)
    DelayMS 100
  Next
EndProc


' Main Program
Main:
BuzzerStartup()
Cls                   ' Clear the LCD using the cls command
HRSOut "Startup",13
P_LCD(1,6,"IRRISYS")
P_LCD(2,1,"FW Ver 1.0")
DelayMS 2000
Cls

'Main screen
Idle_Screen:          'Main display
P_LCD(1,1,"DD/MM/YY/ HH:MM:SS")
P_LCD(2,1,"XXX PSI - RUNNING")
P_LCD(3,1,"Runtime :hh:mm")
P_LCD(4,1,"Press for MENU")
DelayMS 100
'HRSOut "~",13
If _ENC_SW=0 Then P_SetDateTime()
GoTo Idle_Screen


End
'--------------------------------------------
'        PROCEDURES HERE
'--------------------------------------------
Proc P_Buzzer(B_Beep As Byte)
'beep for x ms based on the following format
'1 = beep per RE click
'2 = menu selection
'3 = menu timeout
Select B_Beep
    Case 1
        B_Beep_Len = 15
    Case 2
        B_Beep_Len = 50
    Case 3
        B_Beep_Len = 150
EndSelect
EndProc
'--------------------------------------------
Proc P_LCD(B_Ln As Byte, B_Pos As Byte, S_Data As String * 20)
    ' print data at the line and pos given
    Print At B_ln, B_pos, S_data
EndProc
'--------------------------------------------
' Procedure: SetDateTime
' Uses rotary encoder on RB1/RB2 and button on RB6 to set
'   DD/MM/YY and HH:MM:SS on a DS3231M RTC
'
Proc P_SetDateTime()
    Cls
    Dim B_Sec    As Byte
    Dim B_Min    As Byte
    Dim B_Hour   As Byte
    Dim B_Day    As Byte
    Dim B_Date   As Byte
    Dim B_Month  As Byte
    Dim B_Year   As Byte
    Dim W_LastPos As Word

    ' Read current time from RTC (address $68)
    I2CIn PORTC.4, PORTC.3, $D1, $00, [B_Sec, B_Min, B_Hour, B_Day, B_Date, B_Month, B_Year]

    B_Sec   = ((B_Sec / 16) * 10)
    B_Sec = B_Sec  + (B_Sec & %00001111)
    B_Min   = ((B_Min / 16) * 10)
    B_Min = B_Min  + (B_Min & %00001111)
    B_Hour  = ((B_Hour / 16) * 10)
    B_Hour = B_Hour + (B_Hour & %00001111)
    B_Date  = ((B_Date / 16) * 10)
    B_Date = B_Date + (B_Date & %00001111)
    B_Month = ((B_Month / 16) * 10)
    B_Month = B_Month + (B_Month & %00001111)
    B_Year  = ((B_Year / 16) * 10)
    B_Year = B_Year + (B_Year & %00001111)

    W_LastPos = W_EncoderPos
    Cls
    Print At 1,1,"Set Date & Time"

    'Print At 3, 1, Dec2 B_Date, ":/", Dec2 B_Month, "/", Dec2 B_Year," ", Dec2 B_Hour, ":", Dec2 B_Min, ":", Dec2 B_Sec    
    SetField (B_Date, 1, 31, W_LastPos, 3, 1)
    SetField (B_Month, 1, 12, W_LastPos, 3, 5)
    SetField (B_Year, 0, 99, W_LastPos, 3, 8)
    SetField (B_Hour, 0, 23, W_LastPos, 3,11)
    SetField (B_Min, 0, 59, W_LastPos, 3,14)
    SetField (B_Sec, 0, 59, W_LastPos, 3,17)

    B_Sec = ((B_Sec / 10) * 16)
    B_Sec = B_Sec + (B_Sec // 10)
    B_Min   = ((B_Min / 10) * 16)
    B_Min = B_Min + (B_Min // 10)
    B_Hour  = ((B_Hour / 10) * 16)
    B_Hour = B_Hour + (B_Hour // 10)
    B_Date  = ((B_Date / 10) * 16)
    B_Date = B_Date + (B_Date // 10)
    B_Month = ((B_Month / 10) * 16)
    B_Month = B_Month + (B_Month // 10)
    B_Year  = ((B_Year / 10) * 16)
    B_Year = B_Year + (B_Year // 10)

    I2COut PORTC.4, PORTC.3, $D0, $00, [B_Sec, B_Min, B_Hour, B_Day, B_Date, B_Month, B_Year]
EndProc

'--------------------------------------------
' Helper procedure: adjust a value with the rotary encoder
Proc SetField(ByRef B_Value As Byte, B_Min As Byte, B_Max As Byte, ByRef W_LastPos As Word, B_Row As Byte, B_Col As Byte)
    P_Buzzer(2)
    HRSOut "B_Value",B_Value,13
    HRSOut "B_Min = ",Dec3 B_Min,13
    HRSOut "B_Max = ",Dec3 B_Max,13
    HRSOut "Lastpos = ",Dec5 W_LastPos,13
    HRSOut "B_Row = ",Dec3 B_Row,13
    HRSOut "B_Col = ",Dec3 B_Col,13
    HRSOut "----------------------",13 

    Print At B_Row,3,"/"
    Print At B_Row,6,"/"
    Print At B_Row,9," "
    Print At B_Row,12,":"
    Print At B_Row,15,":"

    While 1 = 1
        If W_EncoderPos > W_LastPos Then
            P_Buzzer(1)
            Inc B_Value
            If B_Value > B_Max Then B_Value = B_Min
            W_LastPos = W_EncoderPos
        EndIf

        If W_EncoderPos < W_LastPos Then
            P_Buzzer(1)
            Dec B_Value
            If B_Value < B_Min Then B_Value = B_Max
            W_LastPos = W_EncoderPos
        EndIf

        Print At B_Row, B_Col, Dec2 B_Value

        If PORTB.6 = 0 Then          ' button pressed
            DelayMS 20               ' debounce
            While PORTB.6 = 0 : Wend
            GoTo Exit_SetField
        EndIf
    Wend
    Exit_SetField:
EndProc
