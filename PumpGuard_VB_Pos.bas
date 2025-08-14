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

'duplicate LCD pin assignments
Symbol LCD_D4_PIN = PORTA.0        ' D4 is the first of 4 contiguous pins (D4..D7)
Symbol LCD_D5_PIN = PORTA.1
Symbol LCD_D6_PIN = PORTA.2
Symbol LCD_D7_PIN = PORTA.3
Symbol LCD_RS_PIN = PORTA.6        ' your screenshot shows PORTA.6
Symbol LCD_E_PIN  = PORTA.7        ' and PORTA.7


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

'Variables for eeprom storage
Dim B_Version As Byte                               'Which version are we on
Dim B_Log_Pos As Byte                                'Last Used Log Position
Dim B_Menu_Timeout As Byte                          'Timeout after x seconds
Dim B_Contrast As Byte                              'set a contrast
Dim B_Relay_Pulse As Byte                           'How long to hold the pulse relay
Dim B_System_Flags As Byte                           'System Flag - set alias for each but
Dim B_HT As Byte                                    'High Temp Limit (deg C)
Dim B_LFlo As Byte                                  'Low Flow limit (%)  

Dim W_Con_2_Cnfg As Word                            'Config Input 1 (Con 2)
Dim W_Con_3_Cnfg As Word                            'Config Input 2 (Con 3)                            
Dim W_Con_4_Cnfg As Word                            'Config Input 3 (Con 4)  


Dim W_Con_2_4ma As Word                             'Configure Input 2
Dim W_Con_2_20ma As Word
Dim W_Con_3_4ma As Word
Dim W_Con_3_20ma As Word
Dim W_Con_4_4ma As Word
Dim W_Con_4_20ma As Word
Dim W_HP As Word
Dim W_LP As Word
Dim W_HP_BP As Word
Dim W_PLP_BP As Word
Dim W_SLP_BP As Word
Dim W_HTBP As Word
Dim W_LFloBP As Word

Dim L_New_RunTime As Long
Dim L_Current_RunTime As Long
Dim L_Last_Run As Long

Dim SB_Current_temp As Byte
Dim b_MTimeout As Bit




Clear                                   'Start clear


' Constants

Symbol LONG_PRESS = 2000  ' 2 seconds for long press (in ms)
Symbol WriteRCT = %11010000 'set the 1337 to receive data                                                'RTC address write
Symbol ReadRTC = %11010001 'set the 1337 to transmit data                                            'RTC address read

'––– 1. EEPROM offsets –––
'––– EEPROM address map –––

' 1. Byte-sized fields (1 byte each)  
Symbol EE_B_Version        = 0x00  ' B_Version  
Symbol EE_B_Log_Pos        = 0x01  ' B_Log_Pos  
Symbol EE_B_Menu_Timeout   = 0x02  ' B_Menu_Timeout  
Symbol EE_B_Contrast       = 0x03  ' B_Contrast  
Symbol EE_B_Relay_Pulse    = 0x04  ' B_Relay_Pulse  
Symbol EE_B_System_Flags   = 0x05  ' B_System_Flags  
Symbol EE_B_HT             = 0x06  ' B_HT  
Symbol EE_B_LFlo           = 0x07  ' B_LFlo  

' 0x08–0x0F reserved for future byte fields  

' 2. Word-sized fields (2 bytes each)  
Symbol EE_W_Con_2_Cnfg     = 0x10  ' W_Con_2_Cnfg  
Symbol EE_W_Con_3_Cnfg     = 0x12  ' W_Con_3_Cnfg  
Symbol EE_W_Con_4_Cnfg     = 0x14  ' W_Con_4_Cnfg  

Symbol EE_W_Con_2_4ma      = 0x16  ' W_Con_2_4ma  
Symbol EE_W_Con_2_20ma     = 0x18  ' W_Con_2_20ma  
Symbol EE_W_Con_3_4ma      = 0x1A  ' W_Con_3_4ma  
Symbol EE_W_Con_3_20ma     = 0x1C  ' W_Con_3_20ma  
Symbol EE_W_Con_4_4ma      = 0x1E  ' W_Con_4_4ma  
Symbol EE_W_Con_4_20ma     = 0x20  ' W_Con_4_20ma  

Symbol EE_W_HP             = 0x22  ' W_HP  
Symbol EE_W_LP             = 0x24  ' W_LP  
Symbol EE_W_HP_BP          = 0x26  ' W_HP_BP  
Symbol EE_W_PLP_BP         = 0x28  ' W_PLP_BP  
Symbol EE_W_SLP_BP         = 0x2A  ' W_SLP_BP  
Symbol EE_W_HTBP           = 0x2C  ' W_HTBP  
Symbol EE_W_LFloBP         = 0x2E  ' W_LFloBP  

' 0x30–0x35 reserved for future word fields  

' 3. Long-sized fields (6 bytes each)  
Symbol EE_L_New_RunTime    = 0x30  ' L_New_RunTime (6 bytes: 0x30–0x35)  
Symbol EE_L_Current_RunTime= 0x36  ' L_Current_RunTime (6 bytes: 0x36–0x3B)  
Symbol EE_L_Last_Run       = 0x3C  ' L_Last_Run (6 bytes: 0x3C–0x41)  

' 4. Signed-word fields (2 bytes each)  
Symbol EE_S_W_Con_2        = 0x42  ' S_W_Con_2  
Symbol EE_S_W_Con_3        = 0x44  ' S_W_Con_3  
Symbol EE_S_W_Con_4        = 0x46  ' S_W_Con_4  
Symbol EE_S_W_Word_1       = 0x48  ' S_W_Word_1  
Symbol EE_S_W_Word_2       = 0x4A  ' S_W_Word_2  
Symbol EE_S_W_Word_3       = 0x4C  ' S_W_Word_3  

' 0x4E–0x4F reserved for future signed-word fields  

' 5. Next free address  
Symbol EE_NextFree         = 0x4E  

' Schema version  
Symbol CURRENT_VERSION     = 1  

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
        If B_RE_Count>9 Then 
            ' debounce rotary encoder and button inputs
            Dim B_NewA  As Byte
            Dim B_NewB  As Byte
            Dim B_NewBtn As Byte
    
            B_NewA  = PORTB.1
            B_NewB  = PORTB.2
            B_NewBtn = PORTB.6
    
            If B_NewA <> B_AState Then
                Inc B_DebA
                If B_DebA >= 2 Then
                    B_AState = B_NewA
                    B_DebA = 0
                EndIf
            Else
                B_DebA = 0
            EndIf
    
            If B_NewB <> B_BState Then
                Inc B_DebB
                If B_DebB >= 2 Then
                    B_BState = B_NewB
                    B_DebB = 0
                EndIf
            Else
                B_DebB = 0
            EndIf
    
            If B_NewBtn <> B_ButtonState Then
                Inc B_DebBtn
                If B_DebBtn >= 2 Then
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

'P_WriteTime()
P_Startup()
DelayMS 100
P_ReadTime()
HRSOut Dec2 B_Date,"/",Dec2 B_Month,"/",Dec2 B_Year,"  ",Dec2 B_Hour,":",Dec2 B_Minute,":",Dec2 B_Second,13




'––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
'
'           EEPROM READS
'
'––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
' EEPROM read wrappers using built-in Eread
'––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
' Read one byte (8-bit)
Proc EEPROM_ReadByte(addr As Byte), Byte
    Result = ERead addr
EndProc

' Read one word (16-bit)
Proc EEPROM_ReadWord(addr As Byte), Word
    Result = ERead addr
EndProc

' Read one long (24-bit)
Proc EEPROM_ReadLong(addr As Byte), Long
    Result = ERead addr
EndProc

' Read one double (32-bit)
Proc EEPROM_ReadDouble(addr As Byte), Double
    Result = ERead addr
EndProc

' Read one signed word (16)
Proc EEPROM_ReadSWord(addr As Byte), Integer
    Dim raw As Word
    raw    = ERead addr             ' read 16-bit unsigned
    If raw And &H8000 Then          ' if sign bit set
        Result = raw - &H10000      ' convert to negative
    Else
        Result = raw                ' positive as-is
    EndIf
EndProc

'––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
' EEPROM write wrappers using built-in Ewrite
'––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––

' Write one byte (8-bit)
Proc EEPROM_WriteByte(addr As Byte, value As Byte)
    EWrite addr, [value]
EndProc

' Write one word (16-bit)
Proc EEPROM_WriteWord(addr As Byte, value As Word)
    EWrite addr, [value]
EndProc

' Write one long (24-bit)
Proc EEPROM_WriteLong(addr As Byte, value As Long)
    EWrite addr, [value]
EndProc

' Write one double (32-bit)
Proc EEPROM_WriteDouble(addr As Byte, value As Double)
    EWrite addr, [value]
EndProc

' Write a signed 16-bit value (two’s-complement) to EEPROM at “addr”
'––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
Proc EEPROM_WriteSWord(addr As Byte, value As Word)
    Dim raw As Word
    If value < 0 Then
        raw = value + 0x10000       ' map negative to two’s-complement
    Else
        raw = value
    EndIf
    EWrite addr, [raw]              ' write 16-bit word
EndProc

'––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
' Main Program
Main:
Cls                   ' Clear the LCD using the cls command
DelayMS 10
'––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––––
P_InitEEPROM()                                              'check the eeprom
P_LCD_SafeInit()                                            'initialize the LCD



HRSOut "Startup",13
P_LCD(1,6,"IRRISYS")
P_LCD(2,1,"FW Ver 1.0")


DelayMS 1000
Cls

'Main screen
'write the current time as 0's
'P_WriteTime()
Idle_Screen:          'Main display

'for b_general = 0 to 10
'    hrsout p_title(b_general),13
'next b_general


P_ReadTime()            'get the time (should be in global vars)
'hrsout "Current time ="

Dim B_Option As Byte
Dim B_Result As Byte
Dim L_Result As Long
Dim L_Mask As Long


DelayMS 100
'HRSOut Dec2 B_Date,"/",Dec2 B_Month,"/",Dec2 B_Year," ",Dec2 B_Hour,":",Dec2 B_Minute,":",Dec2 B_Second, 13
While 1 = 1
    P_ReadTime()
    P_LCD(1,1,"Static     "+Str$(Dec2 B_Hour)+":"+Str$(Dec2 B_Minute)+":"+Str$(Dec2 B_Second))
    P_LCD(2,1,"000psi     No Flow")
    P_LCD(4,1,"READY") 

    'experiment with the runtime
    'If B_ButtonState =0 Then L_result=P_HH(1,733,0,20)'Current value(in seconds), minimum, maximum (in whole hours:minutes
    'hrsout "L_result = ",dec8 L_result,13
    'If B_ButtonState =0 Then L_New_RunTime = p_signed(L_New_RunTime,0,5999)
    'If B_ButtonState =0 Then P_Menu("Setup Menu",%11111111111111111111)    
    'If B_ButtonState =0 Then P_menu("UTILITY MENU",%111111)
'-------------------------------------------------------------
    'Button press for options
    'B_Option=P_Menu("OPTIONS",%11110000000000000000) 
    If B_ButtonState =0 Then 
        L_Mask   = P_BuildMask10(1,2,3,20,0,0,0,0,0,0)           'This is the Options Menu (10 items max)
        B_Option=P_Menu("OPTIONS",L_Mask)
    EndIf
Wend
End
'
'--------------------------------------------
'        Sub Routines HERE
'--------------------------------------------
Menus:          'Main menu system


'Options first
'Title, start at, how many items
    P_Debounce()
    Select B_Option             'This is the options menu
        Case 1                  'Main Menu
            'Main Menu will include the following fields
            'Title, Time, Pessure, Temperature, Flow (if enabled),back
            B_Result=P_Menu("Main Menu",%1111100000000000000)
        Case 2                  'Utility Menu

        Case 3                  'Setup Menu
            B_Option=P_Menu("Setup Menu",%1111111111111111111)
    EndSelect 

    DelayMS 100
Return
'--------------------------------------------
'        PROCEDURES HERE
'––– 1. Load settings from EEPROM –––
'--------------------------------------------




Proc P_LoadSettingsFromEEPROM()
    Dim storedVer As Byte
    storedVer = EEPROM_Read(EE_B_Version)
    
    If storedVer = CURRENT_VERSION Then
        ' Byte fields
        B_Log_Pos       = EEPROM_Read(EE_B_Log_Pos)
        B_Menu_Timeout  = EEPROM_Read(EE_B_Menu_Timeout)
        B_Contrast      = EEPROM_Read(EE_B_Contrast)
        B_Relay_Pulse   = EEPROM_Read(EE_B_Relay_Pulse)
        B_System_Flags  = EEPROM_Read(EE_B_System_Flags)
        B_HT            = EEPROM_Read(EE_B_HT)
        B_LFlo          = EEPROM_Read(EE_B_LFlo)

        ' Word fields
        W_Con_2_Cnfg    = EEPROM_ReadWord(EE_W_Con_2_Cnfg)
        W_Con_3_Cnfg    = EEPROM_ReadWord(EE_W_Con_3_Cnfg)
        W_Con_4_Cnfg    = EEPROM_ReadWord(EE_W_Con_4_Cnfg)

        W_Con_2_4ma     = EEPROM_ReadWord(EE_W_Con_2_4ma)
        W_Con_2_20ma    = EEPROM_ReadWord(EE_W_Con_2_20ma)
        W_Con_3_4ma     = EEPROM_ReadWord(EE_W_Con_3_4ma)
        W_Con_3_20ma    = EEPROM_ReadWord(EE_W_Con_3_20ma)
        W_Con_4_4ma     = EEPROM_ReadWord(EE_W_Con_4_4ma)
        W_Con_4_20ma    = EEPROM_ReadWord(EE_W_Con_4_20ma)

        W_HP            = EEPROM_ReadWord(EE_W_HP)
        W_LP            = EEPROM_ReadWord(EE_W_LP)
        W_HP_BP         = EEPROM_ReadWord(EE_W_HP_BP)
        W_PLP_BP        = EEPROM_ReadWord(EE_W_PLP_BP)
        W_SLP_BP        = EEPROM_ReadWord(EE_W_SLP_BP)
        W_HTBP          = EEPROM_ReadWord(EE_W_HTBP)
        W_LFloBP        = EEPROM_ReadWord(EE_W_LFloBP)

        ' Long fields (6-byte values)
        L_New_RunTime      = EEPROM_ReadLong(EE_L_New_RunTime)
        L_Current_RunTime  = EEPROM_ReadLong(EE_L_Current_RunTime)
        L_Last_Run         = EEPROM_ReadLong(EE_L_Last_Run)
    Else
        ' first run or schema change: initialize defaults
        P_InitEEPROM()
    EndIf
EndProc


'––– 2. Save settings to EEPROM –––
Proc SaveSettingsToEEPROM()
    ' stamp version
    EEPROM_WriteIfChanged EE_B_Version, CURRENT_VERSION

    ' Byte fields
    EEPROM_WriteIfChanged EE_B_Log_Pos,      B_Log_Pos
    EEPROM_WriteIfChanged EE_B_Menu_Timeout, B_Menu_Timeout
    EEPROM_WriteIfChanged EE_B_Contrast,     B_Contrast
    EEPROM_WriteIfChanged EE_B_Relay_Pulse,  B_Relay_Pulse
    EEPROM_WriteIfChanged EE_B_System_Flags, B_System_Flags
    EEPROM_WriteIfChanged EE_B_HT,           B_HT
    EEPROM_WriteIfChanged EE_B_LFlo,         B_LFlo

    ' Word fields
    EEPROM_WriteWordIfChanged EE_W_Con_2_Cnfg,   W_Con_2_Cnfg
    EEPROM_WriteWordIfChanged EE_W_Con_3_Cnfg,   W_Con_3_Cnfg
    EEPROM_WriteWordIfChanged EE_W_Con_4_Cnfg,   W_Con_4_Cnfg

    EEPROM_WriteWordIfChanged EE_W_Con_2_4ma,    W_Con_2_4ma
    EEPROM_WriteWordIfChanged EE_W_Con_2_20ma,   W_Con_2_20ma
    EEPROM_WriteWordIfChanged EE_W_Con_3_4ma,    W_Con_3_4ma
    EEPROM_WriteWordIfChanged EE_W_Con_3_20ma,   W_Con_3_20ma
    EEPROM_WriteWordIfChanged EE_W_Con_4_4ma,    W_Con_4_4ma
    EEPROM_WriteWordIfChanged EE_W_Con_4_20ma,   W_Con_4_20ma

    EEPROM_WriteWordIfChanged EE_W_HP,           W_HP
    EEPROM_WriteWordIfChanged EE_W_LP,           W_LP
    EEPROM_WriteWordIfChanged EE_W_HP_BP,        W_HP_BP
    EEPROM_WriteWordIfChanged EE_W_PLP_BP,       W_PLP_BP
    EEPROM_WriteWordIfChanged EE_W_SLP_BP,       W_SLP_BP
    EEPROM_WriteWordIfChanged EE_W_HTBP,         W_HTBP
    EEPROM_WriteWordIfChanged EE_W_LFloBP,       W_LFloBP

    ' Long fields
    EEPROM_WriteLongIfChanged EE_L_New_RunTime,     L_New_RunTime
    EEPROM_WriteLongIfChanged EE_L_Current_RunTime, L_Current_RunTime
    EEPROM_WriteLongIfChanged EE_L_Last_Run,        L_Last_Run
EndProc


























'--------------------------------------------
Proc P_InitEEPROM()
    Dim storedVer As Byte

    '-- Read schema version --
    storedVer = EEPROM_ReadByte(EE_B_Version)

    If storedVer <> CURRENT_VERSION Then
        '--- Byte defaults ---
        EEPROM_WriteByte(EE_B_Version,      0)
        EEPROM_WriteByte(EE_B_Log_Pos,      0)
        EEPROM_WriteByte(EE_B_Menu_Timeout, 120)
        EEPROM_WriteByte(EE_B_Contrast,     127)
        EEPROM_WriteByte(EE_B_Relay_Pulse,  5)
        EEPROM_WriteByte(EE_B_System_Flags, 0)
        EEPROM_WriteByte(EE_B_HT,           40)
        EEPROM_WriteByte(EE_B_LFlo,         25)

        '--- Word defaults (16-bit) ---
        EEPROM_WriteWord(EE_W_Con_2_Cnfg,   444)
        EEPROM_WriteWord(EE_W_Con_3_Cnfg,  1293)
        EEPROM_WriteWord(EE_W_Con_4_Cnfg,  1026)
        EEPROM_WriteWord(EE_W_Con_2_4ma,     0)
        EEPROM_WriteWord(EE_W_Con_2_20ma,  360)
        EEPROM_WriteWord(EE_W_Con_3_4ma,     0)
        EEPROM_WriteWord(EE_W_Con_3_20ma,  100)
        EEPROM_WriteWord(EE_W_Con_4_4ma,     0)
        EEPROM_WriteWord(EE_W_Con_4_20ma,  100)
        EEPROM_WriteWord(EE_W_HP,          300)
        EEPROM_WriteWord(EE_W_LP,           30)
        EEPROM_WriteWord(EE_W_HP_BP,        0)
        EEPROM_WriteWord(EE_W_PLP_BP,     120)
        EEPROM_WriteWord(EE_W_SLP_BP,      60)
        EEPROM_WriteWord(EE_W_HTBP,        60)
        EEPROM_WriteWord(EE_W_LFloBP,      60)

        '--- 32-bit defaults (Double) ---
        EEPROM_WriteDouble(EE_L_New_RunTime,     0)
        EEPROM_WriteDouble(EE_L_Current_RunTime, 0)
        EEPROM_WriteDouble(EE_L_Last_Run,        0)

        '--- Signed-word defaults (16-bit two’s-complement) ---
        EEPROM_WriteSWord(EE_S_W_Con_2, 0)
        EEPROM_WriteSWord(EE_S_W_Con_3, 0)
        EEPROM_WriteSWord(EE_S_W_Con_4, 0)
        EEPROM_WriteSWord(EE_S_W_Word_1,  0)
        EEPROM_WriteSWord(EE_S_W_Word_2,  0)
        EEPROM_WriteSWord(EE_S_W_Word_3,  0)

        '--- IMPORTANT: mark schema as updated LAST ---
        EEPROM_WriteByte(EE_B_Version, CURRENT_VERSION)


    End If
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
        Case 5
            B_BeepLen=255                
    EndSelect
EndProc
'--------------------------------------------
'Buzzer normal exit menu
Proc P_Exit_OK()
Dim B_Beeps As Byte
For B_Beeps=0 To 2
    P_Beep(2)
    DelayMS 200    
Next B_Beeps
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
  Dim B_Cycle As Byte
  For B_Cycle = 1 To 5
    P_Beep(3)
    DelayMS 150
  Next
EndProc
'--------------------------------------------
'Timeout
Proc P_P_Timeout()
    Dim B_Cycle As Byte
    For B_cycle = 1 To 5
        P_Beep(2)
    DelayMS 100
  Next
EndProc
'--------------------------------------------
' Set bit (ID-1) in an existing mask and return the new mask.
' No beep here (called multiple times by the builder).
Proc P_MaskSetBit(L_In As Long, B_ID As Byte), Long
    Dim L_Bit As Long
    Dim B_I   As Byte

    ' IDs are 1..24 for a 24-bit Long; ignore 0 / out-of-range
    If B_ID = 0 Then
        Result = L_In
        GoTo Exit_P_MaskSetBit
    EndIf
    If B_ID > 24 Then
        Result = L_In
        GoTo Exit_P_MaskSetBit
    EndIf

    ' L_Bit = 1 << (B_ID-1)
    L_Bit = 1
    If B_ID > 1 Then
        For B_I = 1 To B_ID - 1
            L_Bit = L_Bit * 2
        Next
    EndIf

    ' If bit already set, leave as-is; else add the bit
    If (L_In & L_Bit) <> 0 Then
        Result = L_In
    Else
        Result = L_In + L_Bit
    EndIf

Exit_P_MaskSetBit:
EndProc

'--------------------------------------------

' Build a mask from up to TEN decimal IDs (1..24). Pass 0 to skip a slot.
' Returns the mask via Result (Long = 24-bit in Positron).
Proc P_BuildMask10(B1 As Byte, B2 As Byte, B3 As Byte, B4 As Byte, B5 As Byte, B6 As Byte, B7 As Byte, B8 As Byte, B9 As Byte, B10 As Byte), Long
    P_Beep(3)
    P_Debounce()

    Dim L_Mask As Long
    L_Mask = 0

    If B1  <> 0 Then L_Mask = P_MaskSetBit(L_Mask, B1)
    If B2  <> 0 Then L_Mask = P_MaskSetBit(L_Mask, B2)
    If B3  <> 0 Then L_Mask = P_MaskSetBit(L_Mask, B3)
    If B4  <> 0 Then L_Mask = P_MaskSetBit(L_Mask, B4)
    If B5  <> 0 Then L_Mask = P_MaskSetBit(L_Mask, B5)
    If B6  <> 0 Then L_Mask = P_MaskSetBit(L_Mask, B6)
    If B7  <> 0 Then L_Mask = P_MaskSetBit(L_Mask, B7)
    If B8  <> 0 Then L_Mask = P_MaskSetBit(L_Mask, B8)
    If B9  <> 0 Then L_Mask = P_MaskSetBit(L_Mask, B9)
    If B10 <> 0 Then L_Mask = P_MaskSetBit(L_Mask, B10)

    Result = L_Mask
    GoTo Exit_P_BuildMask10
Exit_P_BuildMask10:
EndProc



'--------------------------------------------
' Lookup strings by index (1-based)
'--------------------------------------------------------------------
Proc P_Menu(S_Title As String * 18, L_Mask As Long), Byte
    ' ...
    Dim B_IDs[24] As Byte
    Dim L_bitmask As Long
    Dim B_Count As Byte
    Dim B_I As Byte

    B_Count   = 0
    L_bitmask = 1
    For B_I = 0 To 23                    ' IDs 1..24 (bit0->ID1)
        If (L_Mask & L_bitmask) <> 0 Then
            B_IDs[B_Count] = B_I + 1
            Inc B_Count
        EndIf
        L_bitmask = L_bitmask * 2
    Next
    ' ... (rest of your existing code unchanged)

    '–– display & timeout state ––
    Dim B_Index      As Byte    ' current selection
    Dim B_First      As Byte    ' first visible index in window
    Dim W_LastPos    As Word
    Dim S_Line       As String * 18
    Dim B_Len        As Byte
    Dim W_msCounter  As Word    ' ms accumulator
    Dim B_secCounter As Byte    ' seconds elapsed

    Cls
    Print At 1,1, S_Title         ' title fixed at top
    B_Index      = 0
    W_LastPos    = W_EncoderPos
    W_msCounter  = 0
    B_secCounter = 0
    b_MTimeout   = 0

    While 1 = 1
        '–– three-line sliding window ––
        B_First = B_Index - 2
        If B_First < 0 Then B_First = 0
        If B_First > (B_Count - 3) Then B_First = B_Count - 3
        If B_First < 0 Then B_First = 0

        '–– draw lines 2–4 ––
        For B_I = 0 To 2
            Print At B_I + 2,1,"                    "  ' clear 20 chars
            If (B_First + B_I) < B_Count Then
                S_Line = P_GetMenuString(B_IDs[B_First + B_I])
                B_Len  = Len(S_Line)
                If (B_First + B_I) = B_Index Then
                    Print At B_I + 2,1,"["
                    Print At B_I + 2,2,S_Line
                    Print At B_I + 2,2 + B_Len,"]"
                Else
                    Print At B_I + 2,2,S_Line
                EndIf
            EndIf
        Next

        '–– encoder movement (no wrap), beep on valid move & reset timeout ––
        If W_EncoderPos > W_LastPos Then
            W_LastPos = W_EncoderPos
            If B_Index < B_Count - 1 Then
                Inc B_Index: P_Beep(1)
            EndIf
            W_msCounter = 0: B_secCounter = 0
        ElseIf W_EncoderPos < W_LastPos Then
            W_LastPos = W_EncoderPos
            If B_Index > 0 Then
                Dec B_Index: P_Beep(1)
            EndIf
            W_msCounter = 0: B_secCounter = 0
        EndIf

        '–– button = select ––
        If B_ButtonState = 0 Then
            P_Exit_OK(): P_Debounce()
            Result = B_IDs[B_Index]
            GoTo Exit_P_Menu
        EndIf

'        '–– timeout tick ––
'        DelayMS(125)
'        W_msCounter = W_msCounter + 125
'        If W_msCounter >= 1000 Then
'            W_msCounter  = W_msCounter - 1000
'            Inc B_secCounter
'            If B_secCounter >= B_Menu_Timeout Then
'                P_P_Timeout()   ' timeout beep pattern
'                b_MTimeout = 1      'set flag to indicate timeout
'                'Result = &HFF    ' timeout code
'                GoTo Exit_P_Menu
'            EndIf
'        EndIf
    Wend

Exit_P_Menu:
    'If b_MTimeout = 0 Then P_Exit_OK()   ' normal exit beep
EndProc


'--------------------------------------------------------------------
MenuTable:
    'Group 1 Options 

    'Options    first screen title
    Dim S_String1 As Flash8 = "Main Menu", 0        'option 1     
    Dim S_String2 As Flash8 = "Utility Menu", 0     'option 2 
    Dim S_String3 As Flash8 = "Setup Menu", 0       'option 3   

    'Utility Menu   Utility menu title
    Dim S_String4 As Flash8 = "Date and Time", 0    'option 1
    Dim S_String5 As Flash8 = "View Log", 0         'option 2
    Dim S_String6 As Flash8 = "Clear Log", 0        'option 3
    Dim S_String7 As Flash8 = "Menu Timeout", 0     'option 4
    Dim S_String8 As Flash8 = "Contrast", 0        'option 5
    Dim S_String9 As Flash8 = "Pwr Fail Delay", 0  'option 6

    'Setup Menu 'Setup menu title
    Dim S_String10 As Flash8 = "Input 1", 0         'option 1
    Dim S_String11 As Flash8 = "Input 2", 0         'option 2
    Dim S_String12 As Flash8 = "Input 3", 0         'option 3
    Dim S_String13 As Flash8 = "End Runtime", 0     'option 4
    Dim S_String14 As Flash8 = "Pulse Duration", 0  'option 5
    
                    
    'Main Menu Options
    Dim S_String15 As Flash8 = "RunTime", 0     'option 2
    Dim S_String16 As Flash8 = "Pressure", 0            'option 3
    Dim S_String17 As Flash8 = "Temperature", 0            'option 3 
    Dim S_String18 As Flash8 = "Flow", 0            'option 3 
    Dim S_String19 As Flash8 = "Vacuum", 0            'option 3 
    Dim S_String20 As Flash8 = "BACK", 0

    
    'Alternate Names
    Dim S_String22 As Flash8 = "Something Else", 0            'option 3     

'––– RAM buffers for display –––
Dim S_String1 As String * 18
Dim S_String2 As String * 18
Dim S_String3 As String * 18
Dim S_String4 As String * 18
Dim S_String5 As String * 18
Dim S_String6 As String * 18
Dim S_String7 As String * 18
Dim S_String8 As String * 18
Dim S_String9 As String * 18
Dim S_String10 As String * 18
Dim S_String11 As String * 18
Dim S_String12 As String * 18
Dim S_String13 As String * 18
Dim S_String14 As String * 18
Dim S_String15 As String * 18
Dim S_String16 As String * 18
Dim S_String17 As String * 18
Dim S_String18 As String * 18
Dim S_String19 As String * 18
Dim S_String20 As String * 18

' copy flash into RAM once at startup
Proc InitMenuStrings()
    S_String1 = S_String1_F
    S_String2 = S_String2_F
    S_String3 = S_String3_F
    S_String4 = S_String4_F    
    S_String5 = S_String5_F    
    S_String6 = S_String6_F    
    S_String7 = S_String7_F    
    S_String8 = S_String8_F    
    S_String9 = S_String9_F    
    S_String10 = S_String10_F    
    S_String11 = S_String11_F    
    S_String12 = S_String12_F    
    S_String13 = S_String13_F    
    S_String14 = S_String14_F    
    S_String15 = S_String15_F
    S_String16 = S_String16_F
    S_String17 = S_String17_F
    S_String18 = S_String18_F
    S_String19 = S_String19_F
    S_String19 = S_String20_F
EndProc
'---------------------------------------------------------------
'––– lookup routine –––
Proc P_GetMenuString(B_ID As Byte), String * 18
    Select Case B_ID
    Case 1
        Result = S_String1
    Case 2
        Result = S_String2
    Case 3
        Result = S_String3
    Case 4
        Result = S_String4
    Case 5
        Result = S_String5
    Case 6
        Result = S_String6
    Case 7
        Result = S_String7
    Case 8
        Result = S_String8
    Case 9
        Result = S_String9
    Case 10
        Result = S_String10
    Case 11
        Result = S_String11
    Case 12
        Result = S_String12
    Case 13
        Result = S_String13
    Case 14
        Result = S_String14
    Case 15
        Result = S_String15
    Case 16
        Result = S_String16
    Case 17
        Result = S_String17
    Case 18
        Result = S_String18
    Case 19
        Result = S_String19
    Case 20
        Result = S_String20
    Case Else
        Result = " "    ' blank if unknown
    End Select
EndProc
'---------------------------------------------------------------
'--- low-level helpers (temporary bit-bang only for wake-up) ---
Proc _LCD_SetHiNibble(B_Val As Byte)      ' drives bits onto D7..D4 from Val[7:4]
    If B_Val.7 = 1 Then
        High LCD_D7_PIN    
    Else
        Low LCD_D7_PIN    
    EndIf
    If B_Val.6 = 1 Then
        High LCD_D6_PIN    
    Else
        Low LCD_D6_PIN    
    EndIf 

    If B_Val.5 = 1 Then
        High LCD_D5_PIN    
    Else
        Low LCD_D5_PIN    
    EndIf

    If B_Val.4 = 1 Then
        High LCD_D4_PIN    
    Else
        Low LCD_D4_PIN    
    EndIf

EndProc
'---------------------------------------------------------------
Proc _LCD_PulseE()
    High LCD_E_PIN
    DelayUS 1              ' >300 ns
    Low  LCD_E_PIN         ' falling edge latches data
    DelayUS 50
EndProc
'---------------------------------------------------------------
'--- robust wake + switch to 4-bit, then finish with library-friendly commands ---
Proc P_LCD_SafeInit()
    ' Ensure control lines inactive
    Low LCD_RS_PIN
    Low LCD_E_PIN

    ' Give the module a proper power-up settle
    DelayMS 50             ' >40 ms after VDD rises

    ' ==== Initialize by Instruction (8-bit style on the upper nibble) ====
    ' Send 0x30 three times on D7..D4 with required delays
    _LCD_SetHiNibble($30) : _LCD_PulseE() : DelayMS 5
    _LCD_SetHiNibble($30) : _LCD_PulseE() : DelayUS 200
    _LCD_SetHiNibble($30) : _LCD_PulseE() : DelayUS 200

    ' Select 4-bit interface (send 0x20 on D7..D4)
    _LCD_SetHiNibble($20) : _LCD_PulseE() : DelayUS 200

    ' ==== From here the Positron LCD driver can take over (4-bit aligned) ====
    ' Use standard Print $FE,cmd control bytes so we don't bypass the library

    ' Function set: 4-bit, 2-line, 5x8
    Print $FE, $28

    ' Display OFF
    Print $FE, $08

    ' Clear display
    Print $FE, $01
    DelayMS 2              ' >1.52 ms

    ' Entry mode: increment, no shift
    Print $FE, $06

    ' Display ON, cursor OFF, blink OFF
    Print $FE, $0C
EndProc
'---------------------------------------------------------------
' Edit a signed word with encoder and button
' Returns the final value via Result
' - S_Current : starting value (SWord)
' - S_Upper   : max allowed (SWord)
' - S_Lower   : min allowed (SWord)
Proc P_Signed(S_Current As SWord, S_Upper As SWord, S_Lower As SWord), SWord
    P_Beep(3)         ' entry beep
    P_Debounce()

    Dim S_Value   As SWord
    Dim W_LastPos As Word
    Dim W_Abs     As Word
    Dim B_Changed As Byte
    Dim W_msCounter As Word
    Dim B_secCounter As Byte
    Clear b_MTimeout                'Clear the timeout flag
    ' ensure limits make sense
    If S_Upper < S_Lower Then
        Dim S_Tmp As SWord
        S_Tmp = S_Upper : S_Upper = S_Lower : S_Lower = S_Tmp
    EndIf

    Cls
    ' clamp start value
    S_Value = S_Current
    If S_Value < S_Lower Then S_Value = S_Lower
    If S_Value > S_Upper Then S_Value = S_Upper

    ' ---- initial draw BEFORE any change ----

    If S_Lower < 0 Then 'this could be a minus value
        Print At 2,1,S_Current                          'could be signed - so print sign
    Else    'can't be minus
        Print At 3,1,Dec5 S_Current    
    EndIf
    ' ---------------------------------------

    ' snapshot encoder and enter edit loop
    W_LastPos = W_EncoderPos
    B_Changed = 0

    While 1 = 1
        ' encoder move? (no wrap; beep only on actual change)
        If W_EncoderPos > W_LastPos Then
            W_LastPos = W_EncoderPos
            If S_Value < S_Upper Then
                Inc S_Value
                P_Beep(1)
                B_Changed = 1
            EndIf
        ElseIf W_EncoderPos < W_LastPos Then
            W_LastPos = W_EncoderPos
            If S_Value > S_Lower Then
                Dec S_Value
                P_Beep(1)
                B_Changed = 1
            EndIf
        EndIf

        ' redraw only when changed
        If B_Changed = 1 Then
            'redraw the screen
            If S_Lower < 0 Then 'this could be a minus value
                Print At 2,1,S_Value                          'could be signed - so print sign
            Else    'can't be minus
                Print At 3,1,Dec5 S_Value    
            EndIf
            B_Changed = 0
        EndIf

        ' button confirms and exits
        If B_ButtonState = 0 Then
            P_Exit_OK()
            P_Debounce()
            Result = S_Value
            HRSOut "Result = ",Dec S_Value,13
            GoTo Exit_P_Signed
        EndIf

        DelayMS 50
    Wend
    Exit_P_Signed:
EndProc
'---------------------------------------------------------------
' Edit HH:MM (B_Mode=0) or MM:SS (B_Mode=1)
' B_Min/B_Max are limits for the BIG field (hours or minutes), 0..99
' L_Current is in SECONDS (Long, 24-bit)
' Returns total seconds via Result
Proc P_HH(B_Mode As Byte, L_Current As Long, B_Min As Byte, B_Max As Byte), Long
    P_Beep(3)                  ' entry beep
    P_Debounce()

    Dim W_LastPos  As Word
    Dim B_Changed  As Byte
    Dim B_BigMin   As Byte
    Dim B_BigMax   As Byte
    Dim B_Big      As Byte     ' HH (mode 0) or MM (mode 1)
    Dim B_Small    As Byte     ' MM (mode 0) or SS (mode 1)
    Dim W_TmpW     As Word
    Dim L_Tmp      As Long
    Dim L_Total    As Long

    ' ---- derive initial fields from L_Current ----
    If B_Mode = 0 Then
        ' HH:MM
        W_TmpW = L_Current / 3600         ' whole hours
        If W_TmpW > 99 Then W_TmpW = 99   ' cap for 2-digit display
        B_Big  = W_TmpW

        ' remaining seconds after hours (use Long math)
        L_Tmp    = W_TmpW
        L_Tmp    = L_Tmp * 3600
        L_Tmp    = L_Current - L_Tmp
        B_Small  = L_Tmp / 60             ' 0..59

        Print At 3,1,"HH:MM"
    Else
        ' MM:SS
        W_TmpW = L_Current / 60           ' whole minutes
        If W_TmpW > 99 Then W_TmpW = 99
        B_Big  = W_TmpW

        ' seconds remainder (use Long math)
        L_Tmp   = W_TmpW
        L_Tmp   = L_Tmp * 60
        L_Tmp   = L_Current - L_Tmp
        If L_Tmp < 0 Then L_Tmp = 0
        If L_Tmp > 59 Then L_Tmp = 59
        B_Small = L_Tmp                   ' 0..59

        Print At 3,1,"MM:SS"
    EndIf

    ' ---- big-field limits ----
    B_BigMin = B_Min : If B_BigMin > 99 Then B_BigMin = 99
    B_BigMax = B_Max : If B_BigMax > 99 Then B_BigMax = 99
    If B_Big < B_BigMin Then B_Big = B_BigMin
    If B_Big > B_BigMax Then B_Big = B_BigMax

    ' ---- initial draw ----
    Print At 4,1, Dec2 B_Big
    Print At 4,3, ":"
    Print At 4,4, Dec2 B_Small

    ' ===========================
    ' Edit BIG field first
    ' ===========================
    W_LastPos = W_EncoderPos
    B_Changed = 0
    While 1 = 1
        If W_EncoderPos > W_LastPos Then
            W_LastPos = W_EncoderPos
            If B_Big < B_BigMax Then
                Inc B_Big
                P_Beep(1)
                B_Changed = 1
            EndIf
        ElseIf W_EncoderPos < W_LastPos Then
            W_LastPos = W_EncoderPos
            If B_Big > B_BigMin Then
                Dec B_Big
                P_Beep(1)
                B_Changed = 1
            EndIf
        EndIf

        If B_Changed = 1 Then
            B_Changed = 0
            Print At 4,1, Dec2 B_Big
        EndIf

        ' Button ? move to small field
        If B_ButtonState = 0 Then
            P_Beep(2)
            P_Debounce()
            GoTo Edit_Small
        EndIf

        DelayMS 25
    Wend

Edit_Small:
    ' ===========================
    ' Edit SMALL field (0..59)
    ' ===========================
    W_LastPos = W_EncoderPos
    B_Changed = 0
    While 1 = 1
        If W_EncoderPos > W_LastPos Then
            W_LastPos = W_EncoderPos
            If B_Small < 59 Then
                Inc B_Small
                P_Beep(1)
                B_Changed = 1
            EndIf
        ElseIf W_EncoderPos < W_LastPos Then
            W_LastPos = W_EncoderPos
            If B_Small > 0 Then
                Dec B_Small
                P_Beep(1)
                B_Changed = 1
            EndIf
        EndIf

        If B_Changed = 1 Then
            B_Changed = 0
            Print At 4,4, Dec2 B_Small
        EndIf

        ' Button ? accept and compute total seconds
        If B_ButtonState = 0 Then
            P_Beep(2)
            P_Debounce()

            If B_Mode = 0 Then
                ' HH:MM ? seconds
                L_Tmp   = B_Big
                L_Tmp   = L_Tmp * 3600
                L_Total = L_Tmp
                L_Tmp   = B_Small
                L_Tmp   = L_Tmp * 60
                L_Total = L_Total + L_Tmp
            Else
                ' MM:SS ? seconds
                L_Tmp   = B_Big
                L_Tmp   = L_Tmp * 60
                L_Total = L_Tmp + B_Small
            EndIf

            Result = L_Total
            GoTo Exit_P_HH
        EndIf

        DelayMS 25
    Wend

Exit_P_HH:
EndProc

'---------------------------------------------------------------


'''''Proc P_HHMM(L_Current as long, B_Min as Byte, B_Max as byte),long
'''''    'enter HH:MM at line 4
'''''    'Max and min are the upper and lower values in seconds
'''''    cls
'''''    P_Beep(3)         ' entry beep
'''''    P_Debounce()
'''''    dim W_LastPos as word
'''''    dim B_Changed as byte
'''''    dim B_HrsMax as byte
'''''    dim B_HrsMin as byte
'''''    Dim W_Hours as word    
'''''    dim B_Minutes as byte
'''''    dim W_NewHRS as word
'''''    dim W_New_Min as word
'''''    dim L_RemSecs  As Long
'''''    dim L_Total as long

'''''    'Convert the current value
'''''    W_Hours   = L_Current / 3600
'''''    L_RemSecs = L_Current - (W_Hours * 3600)   ' explicit remainder, no //
'''''    B_Minutes = L_RemSecs / 60                 ' 0..59
    
'''''    HRSOut "L_Current  = ", Dec8 L_Current, 13
'''''    HRSOut "W_Hours    = ", Dec5 W_Hours,   13          'Total Hours
'''''    'HRSOut "L_RemSecs  = ", Dec8 L_RemSecs, 13

'''''    hrsout "---",13
 
'''''    B_HrsMax=B_Max
'''''    B_HrsMin=B_Min
'''''    W_NewHRS=W_Hours
'''''    hrsout "B_HrsMax    = ",dec3 B_HrsMax,13
'''''    Hrsout "B_HrsMin    = ",dec3 B_HrsMin,13

'''''    'Hours first

'''''    print at 3,1,"HH:MM"            'Title line 3
'''''    print at 4,1,dec2 W_NewHRS,":"  'value line 4
'''''    print at 4,4,dec2 B_Minutes
'''''    W_LastPos = W_EncoderPos
'''''    B_Changed = 0
'''''    while 1=1
'''''        ' snapshot encoder and enter edit loop
'''''        'Hours First
'''''        ;Increment
'''''        if W_encoderPos > W_LastPos then
'''''            W_LastPos = W_EncoderPos
'''''            if  W_NewHRS < B_HrsMax then
'''''                p_beep(1)
'''''                inc W_NewHRS        
'''''                B_changed=1
'''''                hrsout "Inc",13
'''''            endif
'''''        endif
'''''        ;decriment
'''''       If W_EncoderPos < W_LastPos Then
'''''            W_LastPos = W_EncoderPos
'''''            If  W_NewHRS > B_HrsMin Then
'''''                P_Beep(1)
'''''                dec W_NewHRS          
'''''                B_changed=1
'''''                hrsout "Dec",13
'''''            EndIf
'''''        EndIf        

'''''        if B_Changed=1 then
'''''            B_Changed=0
'''''            print at 4,1,dec2 W_NewHRS
'''''            hrsout "W_NewHRS = ",dec3 W_NewHRS,13        
'''''        endif
'''''        delayms 25 

'''''        ' ---- button: move to smaller field, then accept ----
'''''        If B_ButtonState = 0 Then
'''''            p_beep(02)
'''''            P_Debounce()
'''''            L_Total = (W_NewHRS*60)*60      'New value in seconds (So far)          
'''''            goto EXIT_HH
'''''        endif
'''''    wend
'''''    EXIT_HH:

'''''    'Now do Minutes
    
'''''    HRSOut "B_Minutes  = ", Dec3 B_Minutes, 13          'Total Minutes
'''''    W_LastPos = W_EncoderPos
'''''    B_Changed = 0
'''''    While 1=1
'''''        ' snapshot encoder and enter edit loop
'''''        'Hours First
'''''        ;Increment
'''''        If W_EncoderPos > W_LastPos Then
'''''            W_LastPos = W_EncoderPos
'''''            If  W_New_Min < 59 Then
'''''                P_Beep(1)
'''''                Inc W_New_Min       
'''''                B_changed=1
'''''                HRSOut "Inc",13
'''''            EndIf
'''''        EndIf
'''''        ;decriment
'''''       If W_EncoderPos < W_LastPos Then
'''''            W_LastPos = W_EncoderPos
'''''            If  W_New_Min > 0 Then
'''''                P_Beep(1)
'''''                Dec W_New_Min          
'''''                B_changed=1
'''''                HRSOut "Dec",13
'''''            EndIf
'''''        EndIf        

'''''        If B_Changed=1 Then
'''''            B_Changed=0
'''''            Print At 4,4,Dec2 W_New_Min
'''''            HRSOut "W_NewMins = ",Dec3 W_New_Min,13        
'''''        EndIf
'''''        DelayMS 25 

'''''        ' ---- button: move to smaller field, then accept ----
'''''        If B_ButtonState = 0 Then
'''''            P_Beep(02)
'''''            P_Debounce()
'''''            L_Total = L_Total+ (W_New_Min*60)     'New value in seconds (So far)          
'''''            goto EXIT_MM
'''''        EndIf
'''''    Wend


'''''    EXIT_MM:
'''''    hrsout "Result = ",dec8 L_Total,13 

'''''endproc

'---------------------------------------------------------------



