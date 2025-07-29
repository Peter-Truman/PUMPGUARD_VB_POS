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
Dim b_Isolate As Bit
Dim B_RE_Count As Byte
Dim B_Selected As Byte

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
        Inc B_RE_Count                                              'check the RE every 10 ms
        If B_RE_Count>5 Then 
            ' debounce rotary encoder and button inputs
            Dim B_NewA  As Byte
            Dim B_NewB  As Byte
            Dim B_NewBtn As Byte
    
            B_NewA  = PORTB.1
            B_NewB  = PORTB.2
            B_NewBtn = PORTB.6
    
            If B_NewA <> B_AState Then
                Inc B_DebA
                If B_DebA >= 6 Then
                    B_AState = B_NewA
                    B_DebA = 0
                EndIf
            Else
                B_DebA = 0
            EndIf
    
            If B_NewB <> B_BState Then
                Inc B_DebB
                If B_DebB >= 6 Then
                    B_BState = B_NewB
                    B_DebB = 0
                EndIf
            Else
                B_DebB = 0
            EndIf
    
            If B_NewBtn <> B_ButtonState Then
                Inc B_DebBtn
                If B_DebBtn >= 6 Then
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
    
    
            'Isolate changes here (to avoid changes while pressing the knob)
            If b_Isolate=0 Then 
                Select B_Combined
                    Case 0b0001, 0b0111, 0b1110, 0b1000
                        Dec W_EncoderPos
                    Case 0b0010, 0b1011, 0b1101, 0b0100
                        Inc W_EncoderPos
                EndSelect
            EndIf
            B_LastState = B_Curr
            Clear B_RE_Count                                                    'start the count again
        EndIf
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
P_Startup()
DelayMS 100
P_ReadTime()
HRSOut Dec2 B_Date,"/",Dec2 B_Month,"/",Dec2 B_Year,"  ",Dec2 B_Hour,":",Dec2 B_Minute,":",Dec2 B_Second,13

' Main Program
Main:
Cls                   ' Clear the LCD using the cls command
DelayMS 10
HRSOut "Startup",13
P_LCD(1,6,"IRRISYS")
P_LCD(2,1,"FW Ver 1.0")
DelayMS 2000
Cls

'Main screen
'write the current time as 0's
'P_WriteTime()
Idle_Screen:          'Main display

P_ReadTime()            'get the time (should be in global vars)
'hrsout "Current time ="

DelayMS 100
'HRSOut Dec2 B_Date,"/",Dec2 B_Month,"/",Dec2 B_Year," ",Dec2 B_Hour,":",Dec2 B_Minute,":",Dec2 B_Second, 13
While 1 = 1
    P_ReadTime()
    P_LCD(1,1,"Static     "+Str$(Dec2 B_Hour)+":"+Str$(Dec2 B_Minute)+":"+Str$(Dec2 B_Second))
    P_LCD(2,1,"000psi     No Flow")
    P_LCD(4,1,"READY") 
    B_Selected = P_MenuSelect(MenuTable, 10)
    'If B_ButtonState =0 Then P_screen1()
    DelayMS 100
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
    Retry:
    P_Beep(3)                                                       'Beep In
    P_Debounce()
    Dim W_LastPos As Word
    DelayMS 500
    HRSOut "P_SetDateTime()",13
    P_LCD(1,1,"Set Date and Time")
    P_ReadTime()                         ' Read current time from RTC (address $68)
    P_LCD(3,1,Str$(Dec2 B_Date)+"/MM/YY HH:MM:SS")
 
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
    
    'check if ok to continue   (ASCII 174 for back arrow, 175 for forward arrow)
    If P_Ok(W_LastPos)=0 Then    'try again
        P_Retry()
        HRSOut "Retry",13
        Cls
        DelayMS 250
        GoTo Retry
    EndIf
    'ok - unless timed our or early exit, write the time
    P_WriteTime()
    Cls
EndProc

'--------------------------------------------
' Helper procedure: adjust a value with the rotary encoder
Proc P_SetField(B_Ln As Byte, B_col As Byte,B_Zero As Byte,B_Value As Byte, B_Min As Byte, B_Max As Byte, ByRef W_LastPos As Word), Word
    'line, col,leading 0, current val, min,, max and RE lastpos
    P_Debounce()
    'While _ENC_SW =0:DelayMS 10: Wend: DelayMS 100                             'debounce
   
    'HRSOut "P_SetField",13
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
        If B_ButtonState = 0 Then          ' button pressed
            P_Beep(2) 
            While B_ButtonState = 0 :DelayMS 100: Wend: DelayMS 50
            GoTo Exit_P_SetField:
        EndIf
        DelayMS 75
    Wend
    Exit_P_SetField:
    Result = B_Value                       'return a value
EndProc
'---------------------------------------------------------
Proc P_Debounce()
While B_ButtonState =0:DelayMS 10: Wend: DelayMS 100      
EndProc
'---------------------------------------------------------
Proc P_Ok(W_LastPos As Word),Bit             'always on the last row            
    Dim b_flag As Bit
    P_Debounce()
    While 1=1
        If b_flag=0 Then
            Print At 4,1,"   [OK]    Retry "
            Result = 1
        Else
            Print At 4,1,"    OK    [Retry]"
            Result = 0
        EndIf
        If W_EncoderPos <> W_LastPos Then
            P_Beep(2)
            b_flag = ~b_flag
            W_LastPos = W_EncoderPos
        EndIf
 
        If _ENC_SW = 0 Then          ' button pressed
            P_Beep(2) 
            While _ENC_SW = 0 :DelayMS 10: Wend: DelayMS 50
            GoTo Exit_P_Ok:
        EndIf
        DelayMS 150        
    Wend
    Exit_P_Ok:    
    HRSOut "Result = ",Dec1 b_flag,13
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

'HRSOut "BCD Write values Seconds = ",Dec3 B_Second,13
'HRSOut "BCD Write values Hours = ",Dec3 B_Hour,13
'HRSOut "BCD Write values Minutes = ",Dec3 B_Minute,13
'HRSOut "BCD Write values Date = ",Dec3 B_Date,13
'HRSOut "BCD Write values Month = ",Dec3 B_Month,13
'HRSOut "BCD Write values Year = ",Dec3 B_Year,13
'HRSOut "=========================",13

    BusOut WriteRCT, 0, [B_Second, B_Minute, B_Hour, B_Day, B_Date, B_Month, B_Year]

    ' convert back to decimal
    P_BCDConvert (B_Second, 0)
    P_BCDConvert (B_Minute, 0)
    P_BCDConvert (B_Hour, 0)
    P_BCDConvert (B_Date, 0)
    P_BCDConvert (B_Month, 0)
    P_BCDConvert (B_Year, 0)

'HRSOut "Readback",13
'HRSOut Dec2 B_Date,"/",Dec2 B_Month,"/",Dec2 B_Year," ",Dec2 B_Hour,":",Dec2 B_Minute,":",Dec2 B_Second, 13
'HRSOut "--------------",13


EndProc
'--------------------------------------------
Proc P_Beep(B_Len As Byte)
    'sets the buzzer going - decriment in interrupt
    Select B_len
        Case 1
            B_BeepLen=1
        Case 2
            B_BeepLen=50
        Case 3
            B_BeepLen=100
        Case 4
            B_BeepLen=200        
    EndSelect
EndProc
'--------------------------------------------
' Buzzer Startup Procedure
Proc P_Startup()
  Dim cycle As Byte
  For cycle = 1 To 5
    P_Beep(3)
    DelayMS 100
  Next
EndProc
'--------------------------------------------
' Retry Procedure
Proc P_Retry()
  Dim cycle As Byte
  For cycle = 1 To 5
    P_Beep(2)
    DelayMS 100
  Next
EndProc
'--------------------------------------------
Proc P_Screen1(),Byte                                                  'this will be the main menu screen - 3 options
    Cls
    P_Beep(3)                                                       'Beep In
    P_Debounce()
    Dim W_LastPos As Word
    Dim B_Option As Byte
    Dim S_Op1 As String * 18
    Dim S_Op2 As String * 18    
    Dim S_Op3 As String * 18
    Dim B_End As Byte
    S_Op1="Main Menu"
    S_Op2="Utility Menu"
    S_Op3="Setup Menu"



    B_Option = 1                                                    'start at pos 1
    DelayMS 500                                                     'Standard entry
    'draw the main screen
    P_LCD(1,6,"OPTIONS")
    P_LCD(2,2,S_Op1)
    P_LCD(3,2,S_Op2)
    P_LCD(4,2,S_Op3)
    
    'now set the cursor

    While 1=1
        Select B_Option
            Case 1
                P_LCD(2,1,"[")
                P_LCD(3,1," ")                
                P_LCD(4,1," ")
                B_End=Len(S_Op1)+1
                P_LCD(2,B_End,"]")
                B_End=Len(S_Op2)+1                
                P_LCD(3,B_End," ")                
                B_End=Len(S_Op3)+1                
                P_LCD(4,B_End," ")                  
            Case 2
                P_LCD(2,1," ")
                P_LCD(3,1,"[")                
                P_LCD(4,1," ")
                B_End=Len(S_Op1)+1
                P_LCD(2,B_End," ")
                B_End=Len(S_Op2)+1                
                P_LCD(3,B_End,"]")                
                B_End=Len(S_Op3)+1                
                P_LCD(4,B_End," ")             
            Case 3
                P_LCD(2,1," ")
                P_LCD(3,1," ")                
                P_LCD(4,1,"[")
                B_End=Len(S_Op1)+1
                P_LCD(2,B_End," ")
                B_End=Len(S_Op2)+1                
                P_LCD(3,B_End," ")                
                B_End=Len(S_Op3)+1                
                P_LCD(4,B_End,"]")              
        EndSelect

        If W_EncoderPos > W_LastPos Then
            P_Beep(1)
            Inc B_Option
            If B_Option > 3 Then B_Option = 1
            W_LastPos = W_EncoderPos
        EndIf
         If W_EncoderPos < W_LastPos Then
            P_Beep(1)
            Dec B_Option
            If B_Option < 1 Then B_Option = 3
            W_LastPos = W_EncoderPos
        EndIf
        If B_ButtonState = 0 Then          ' button pressed
            P_Beep(2) 
            While B_ButtonState = 0 :DelayMS 100: Wend: DelayMS 50
            GoTo Exit_P_MainMen:
        EndIf
        DelayMS 200
    Wend
    Exit_P_MainMen:
    HRSOut "B_Option = ",Dec3 B_Option,13
    Result = B_Option
    Cls
EndProc
'------------------------------------------------------------------
' Procedure: P_MenuSelect
' Shows a list of items on a 4-line LCD window and returns the
' selected item number (1..B_Count). Items are stored as a Flash16
' table holding addresses of null terminated strings.
Proc P_MenuSelect(W_Table As Word, B_Count As Byte), Byte
    Dim B_Index   As Byte
    Dim B_First   As Byte
    Dim B_I       As Byte
    Dim W_Addr    As Word
    Dim W_LastPos As Word

    B_Index = 0
    B_First = 0
    W_LastPos = W_EncoderPos
    Cls

    While 1=1
        ' display current window
        For B_I = 0 To 3
            If (B_First + B_I) < B_Count Then
                W_Addr = CRead16 W_Table[B_First + B_I]
                If (B_First + B_I) = B_Index Then
                    Print At B_I, 0, ">"
                Else
                    Print At B_I, 0, " "
                EndIf
                Print At B_I, 1, CStr W_Addr
            Else
                Print At B_I, 0, "                "
            EndIf
        Next

        ' handle rotary movement
        If W_EncoderPos > W_LastPos Then
            Inc B_Index
            If B_Index >= B_Count Then B_Index = 0
            W_LastPos = W_EncoderPos
        EndIf

        If W_EncoderPos < W_LastPos Then
            If B_Index = 0 Then
                B_Index = B_Count - 1
            Else
                Dec B_Index
            EndIf
            W_LastPos = W_EncoderPos
        EndIf

        ' shift window if needed
        If B_Index >= (B_First + 4) Then
            B_First = B_Index - 3
        EndIf

        If B_Index < B_First Then
            B_First = B_Index
        EndIf

        ' button confirms selection
        If B_ButtonState = 0 Then
            While B_ButtonState = 0 : Wend
            DelayMS 20
            Result = B_Index + 1
            GoTo Exit_P_MenuSelect
        EndIf
    Wend
    Exit_P_MenuSelect:
EndProc
'--------------------------------------------
' Procedure: P_PrintWord
' Prints one of the custom menu words on the LCD
'  Index: 1=OK, 2=Retry, 3=[OK], 4=[Retry]
Proc P_PrintWord(B_Index As Byte, B_Row As Byte, B_Col As Byte)
    Dim W_Addr As Word
    Select B_Index
        Case 1
            W_Addr = High_Pressure
        Case 2
            W_Addr = HP_Bypass
        Case 3
            W_Addr = Low_Preassure
        Case 4
            W_Addr = Prim_LP_Bypass
        Case 5
            W_Addr = Sec_LP_Bypass
        Case 6
            W_Addr = Use_Clock? 
        Case 7
            W_Addr = Use_DDV?                 
        Case 8
            W_Addr = Use_Flow_Sw?          
        Case 9
            W_Addr = Use_Flow_Rate?       
        Case 10
            W_Addr = Use_Flow_Volume? 
        Else
            Exit
    EndSelect
    Print At B_Row, B_Col, CStr W_Addr
EndProc



