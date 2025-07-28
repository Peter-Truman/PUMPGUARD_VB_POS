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
Declare SDA_Pin PORTC.4                                                                                     '12C declares
Declare SCL_Pin PORTC.3
Declare Slow_Bus On
 
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
Dim B_AState     As Byte
Dim B_BState     As Byte
Dim B_ButtonState As Byte
Dim B_DebA       As Byte
Dim B_DebB       As Byte
Dim B_DebBtn     As Byte
Dim B_General As Byte
Dim B_Second As Byte
Dim B_Minute As Byte
Dim B_Hour As Byte
Dim B_Day As Byte
Dim B_Date As Byte
Dim B_Month As Byte
Dim B_Year As Byte
Dim B_BeepLen As Byte


Clear                                   'Start clear

' Constants

Symbol LONG_PRESS = 2000  ' 2 seconds for long press (in ms)

Symbol WriteRCT = %11010000 'set the 1337 to receive data                                                'RTC address write
Symbol ReadRTC = %11010001 'set the 1337 to transmit data                                            'RTC address read

' Timer0 configuration: prescaler 1:64 for 1ms tick @ 8MHz
T0CONbits_T0PS2 = 1        ' \ prescaler 1:64
T0CONbits_T0PS1 = 0        '  |
T0CONbits_T0PS0 = 0        ' /
T0CONbits_PSA  = 0         ' assign prescaler
T0CONbits_T0CS  = 0        ' internal clock
T0CONbits_T08BIT = 1       ' 8-bit mode
TMR0L = 256-31             ' preload for 1ms (31 counts)

'--------------------------------------------
' Interrupt setup
On_Hardware_Interrupt GoTo ISR_Handler
INTCONbits_T0IF = 0        ' clear flag
INTCONbits_T0IE = 1        ' enable Timer0 interrupt
INTCONbits_GIE = 1         ' global enable
T0CONbits_TMR0ON = 1       ' start timer




; Interrupt Handler

GoTo over_Interrupt
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
            If B_DebA >= 50 Then
                B_AState = B_NewA
                B_DebA = 0
            EndIf
        Else
            B_DebA = 0
        EndIf

        If B_NewB <> B_BState Then
            Inc B_DebB
            If B_DebB >= 50 Then
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


    If B_BeepLen > 0 Then                               'count down in ms
        High _BUZZER
        Dec B_BeepLen    
    Else
        Low _BUZZER                                     'buzzer off    
    EndIf
    Context Restore

over_Interrupt:


'startup delay to settle
DelayMS 500

''Time for the moment
'B_Second =13
'B_Minute = 46
'B_Hour = 15
'B_Date = 27
'B_Month = 7
'B_Year=25

'P_WriteTime()

DelayMS 100
P_ReadTime()
HRSOut Dec2 B_Date,"/",Dec2 B_Month,"/",Dec2 B_Year,"  ",Dec2 B_Hour,":",Dec2 B_Minute,":",Dec2 B_Second,13

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
'write the current time as 0's
'P_WriteTime()
Idle_Screen:          'Main display
P_LCD(1,1,"Test RTC")

P_ReadTime()            'get the time (should be in global vars)
'hrsout "Current time ="

DelayMS 100
HRSOut Dec2 B_Date,"/",Dec2 B_Month,"/",Dec2 B_Year," ",Dec2 B_Hour,":",Dec2 B_Minute,":",Dec2 B_Second, 13
While 1 = 1
    P_LCD(1,1,"Main Screen")
    P_ReadTime()
    P_LCD(3,1,Str$(Dec2 B_Date)+"/"+Str$(Dec2 B_Month)+"/"+Str$(Dec2 B_Year)+" "+Str$(Dec2 B_Hour)+":"+Str$(Dec2 B_Minute)+":"+Str$(Dec2 B_Second))
    DelayMS 100
    If _ENC_SW =0 Then P_SetDateTime()
Wend
End
'--------------------------------------------
'        PROCEDURES HERE
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
    P_Beep(2)                                                       'Beep In
    While _ENC_SW = 0 :DelayMS 10: Wend: DelayMS 50
    Dim W_LastPos As Word

    HRSOut "P_SetDateTime()",13
    P_LCD(1,1,"Set Date and Time")
    P_ReadTime()                         ' Read current time from RTC (address $68)
    P_LCD(3,1,Str$(Dec2 B_Date)+"/"+Str$(Dec2 B_Month)+"/"+Str$(Dec2 B_Year)+" "+Str$(Dec2 B_Hour)+":"+Str$(Dec2 B_Minute)+":"+Str$(Dec2 B_Second))
    
    'Date
    B_Date = P_SetField(3,1,2,B_Date,1,31,W_LastPos)       'Date       
    P_LCD(3,1,Str$(Dec2 B_Date)+"/"+Str$(Dec2 B_Month))

    'Month
    B_Month = P_SetField(3,4,2,B_Month,1,12,W_LastPos)       'Month               
    P_LCD(3,4,Str$(Dec2 B_Month))

    'Year
    B_Year = P_SetField(3,7,2,B_Year,25,99,W_LastPos)       'Year 
    P_LCD(3,7,Str$(Dec2 B_Year))    

    'Hours     
    B_Hour = P_SetField(3,10,2,B_Hour,0,23,W_LastPos)      'Hour
    P_LCD(3,10,Str$(Dec2 B_Hour))
    
    'Minutes
    B_Minute = P_SetField(3,13,2,B_Minute,0,59,W_LastPos)    'Minute
    P_LCD(3,13,Str$(Dec2 B_Minute))
    
    'Second
    B_Second = P_SetField(3,16,2,B_Second,0,59,W_LastPos)   'Second         
    P_LCD(3,16,Str$(Dec2 B_Second))

    'ok - unless timed our or early exit, write the time
    P_WriteTime()
    Cls
EndProc

'--------------------------------------------
' Helper procedure: adjust a value with the rotary encoder
Proc P_SetField(B_Ln As Byte, B_col As Byte,B_Zero As Byte,B_Value As Byte, B_Min As Byte, B_Max As Byte, ByRef W_LastPos As Word), Word
    'line, col,leading 0, current val, min,, max and RE lastpos
    While _ENC_SW =0:DelayMS 10: Wend                               'debounce
    DelayMS 100
    HRSOut "P_SetField",13
    While 1 =1
        If W_EncoderPos > W_LastPos Then
            P_Beep(1)
            Inc B_Value
            If B_Value > B_Max Then B_Value = B_Min
            W_LastPos = W_EncoderPos
        EndIf

        If W_EncoderPos < W_LastPos Then
            P_Beep(1)
            Dec B_Value
            If B_Value < B_Min Then B_Value = B_Max
            W_LastPos = W_EncoderPos
        EndIf

        'Display B_Value to the user here
        Select b_Zero
            Case 2  'Dec2
                P_LCD(B_Ln,B_col,Str$(Dec2 B_Value)) 
            Case 3  'Dec3
                P_LCD(B_Ln,B_col,Str$(Dec3 B_Value)) 
            Case 5  'Dec5 
                P_LCD(B_Ln,B_col,Str$(Dec5 B_Value)) 
        EndSelect        
        If _ENC_SW = 0 Then          ' button pressed
            DelayMS 20               ' debounce
            While _ENC_SW = 0 :DelayMS 100: Wend: DelayMS 50
            GoTo Exit_P_SetField:
        EndIf
        DelayMS 50
    Wend
    Exit_P_SetField:
    Result = B_Value                                        'return a value
EndProc
'---------------------------------------------------------
Proc B2BCD(B_convert As Byte),Byte
        'BIN_TO_BCD1
        Dim temp1 As Byte
        Dim temp2 As Byte

        temp1 = Dig B_convert,0                                                               'get the first decimal digit
        temp2 = Dig B_convert,1                                                               'second nyble
        temp2=temp2 <<4                                                                     'move number to 2nd nyble
        Result = temp1^temp2
EndProc
'---------------------------------------------------------
Proc  B2BIN(B_convert As Byte),Byte
    'BCD_TO_BIN1:
    Dim temp1 As Byte
    Dim temp2 As Byte

    B_Temp_1 = B_convert & $F                                                               'Convert values from BCD to Binary
    B_Temp_2 = B_convert & $F0                                                              'mask off either side
    B_Temp_2 = B_Temp_2 >>4                                                                 'divide by 16
    B_Temp_2 = B_Temp_2 * 10                                                                'X 10
    Result = B_Temp_1 + B_Temp_2                                                            'add them together
EndProc
'---------------------------------------------------------
'Procedure: P_BCDConvert
' Converts between BCD and decimal.
' Dir=0 : BCD->Decimal, Dir<>0 : Decimal->BCD
Proc P_BCDConvert(ByRef B_Value As Byte, B_Dir As Byte)
    If B_Dir = 0 Then
        B_Value = ((B_Value / 16) * 10)
        B_Value = B_Value + (B_Value & %00001111)
    Else
        B_Value = ((B_Value / 10) * 16)
        B_Value = B_Value + (B_Value // 10)
    EndIf
EndProc
'--------------------------------------------
' Procedure: P_ReadTime
' Reads current time from DS3231 RTC
Proc P_ReadTime()
    Dim B_Day As Byte
    'HRSOut "Readtime",13
    BusIn ReadRTC, 0, [B_Second, B_Minute, B_Hour, B_Day, B_Date, B_Month, B_Year]

    P_BCDConvert (B_Second, 0)
    P_BCDConvert (B_Minute, 0)
    P_BCDConvert (B_Hour, 0)
    P_BCDConvert (B_Date, 0)
    P_BCDConvert (B_Month, 0)
    P_BCDConvert (B_Year, 0)

'hrsout "D&T at Readtime",13
'HRSOut Dec2 B_Date,"/",Dec2 B_Month,"/",Dec2 B_Year," ",Dec2 B_Hour,":",Dec2 B_Minute,":",Dec2 B_Second, 13


EndProc
'--------------------------------------------
' Procedure: P_WriteTime
' Writes global time variables to DS3231 RTC
Proc P_WriteTime()
    Dim B_Day As Byte
HRSOut "write time - before convertion",13
HRSOut Dec2 B_Date,"/",Dec2 B_Month,"/",Dec2 B_Year," ",Dec2 B_Hour,":",Dec2 B_Minute,":",Dec2 B_Second, 13
HRSOut "--------------",13



    ' convert values to BCD
    P_BCDConvert (B_Second, 1)
    P_BCDConvert (B_Minute, 1)
    P_BCDConvert (B_Hour, 1)
    P_BCDConvert (B_Date, 1)
    P_BCDConvert (B_Month, 1)
    P_BCDConvert (B_Year, 1)

    B_Day = 1

HRSOut "BCD Write values Seconds = ",Dec3 B_Second,13
HRSOut "BCD Write values Hours = ",Dec3 B_Hour,13
HRSOut "BCD Write values Minutes = ",Dec3 B_Minute,13
HRSOut "BCD Write values Date = ",Dec3 B_Date,13
HRSOut "BCD Write values Month = ",Dec3 B_Month,13
HRSOut "BCD Write values Year = ",Dec3 B_Year,13
HRSOut "=========================",13

    BusOut WriteRCT, 0, [B_Second, B_Minute, B_Hour, B_Day, B_Date, B_Month, B_Year]

    ' convert back to decimal
    P_BCDConvert (B_Second, 0)
    P_BCDConvert (B_Minute, 0)
    P_BCDConvert (B_Hour, 0)
    P_BCDConvert (B_Date, 0)
    P_BCDConvert (B_Month, 0)
    P_BCDConvert (B_Year, 0)

HRSOut "Readback",13
HRSOut Dec2 B_Date,"/",Dec2 B_Month,"/",Dec2 B_Year," ",Dec2 B_Hour,":",Dec2 B_Minute,":",Dec2 B_Second, 13
HRSOut "--------------",13


EndProc
'--------------------------------------------
Proc P_Beep(B_Len As Byte)
    'sets the buzzer going - decriment in interrupt
    Select B_len
        Case 1
            B_BeepLen=5
        Case 2
            B_BeepLen=50
        Case 2
            B_BeepLen=100
    EndSelect
EndProc
'--------------------------------------------
' Buzzer Startup Procedure
Proc BuzzerStartup()
  Dim cycle As Byte
  For cycle = 1 To 5
    P_Beep(3)
    DelayMS 100
  Next
EndProc


