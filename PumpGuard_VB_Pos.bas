'****************************************************************
'*  Name    : IRRISYS_MAIN.BAS                                  *
'*  Author  : Peter W Truman                                    *
'*  Notice  : Copyright (c) 2025 PCT Remote Sensing Pty Ltd     *
'*          : All Rights Reserved                               *
'*  Date    : 24/07/2025                                        *
'*  Version : 1.1                                               *
'*  Notes   : Simplified list-based menu system (Positron 8)    *
'*            - No bitmask builders                             *
'*            - Max 8 IDs per menu screen                       *
'*            - Order preserved; BACK optional (ID=20)          *
'****************************************************************
(*
Input configurations registers ( W_In_1_Cnfg x3 )

MSB
15      14      13      12      11      10      09      08      07      06      05      04      03      02      01      00
        D1      D0      FASL1   FASL0   FAPL1   FAPL0   FAH1    FAH0    DF1     DF0     IE1     IE0     ME      S1      S0
        
Bit     Funct
00-01   Sensor              00=digital
S0/S1                       01=pressure
                            10=temperature
                            11=flow
                                                                                        
02      Master Enable       0=Disabled
ME                          1=enabled

03-04   Indiv Enable        00=high and low disabled
IE0/IE1                     01=low disabled, high enabled                         
                            10=low enabled, high disabled
                            11=low enabled, high enabled
                        
05-06   Dig Fault State     00=digital not used
DF0/DF1                     01=high going
                            10=low going
                            
07-08   Fault Action High   00=no action        
FAH0/FAH1                   01=Pulse
                            10=latch

09-10   Fault Action PL     00=no action
FAPL0/FAPL1                 01=Pulse
                            10=Latch
                            
11-12   Fault Action SL     00=no action
FASL0/FASL1                 01=Pulse
                            10=Latch

13-14   Display             00=no display
D0/D1                       01=always display
                            10=display if enabled

'-------------------------------------------------------------
Procedure for setting up inputs
Meeting with Damien 20/8/25

Don't use 'BACK' stick to the long press (not quite as long as it is now)

Manu Options are 
1 - Main Menu
2 - Setup Menu
3 - Utility Menu
4 - Config Menu

Main Menu - depends on what is enabled / disabled
    


Setup Menu - procedure
1   Select input (Input 1,2,3)
2 - Enable Disable (Skip the input setup if disabled)
3 - Select Pressure, Temperature, Flow
4 - work through the following (as a scrollable screen - depending on the type) for each input
   
    INPUT X
    PRESSURE
    Scale 4ma           [nnnn] PSI
    Scale 20ma          [NNNN] PSI
    Set High BP         [00:00] mm:ss
    Set PLP BP          [00:00] mm:ss
    Set SLP BP          [00:00] mm:ss      
    Rly High            [Latch/Pulse/Off]   (same as enabling / disabling the protection)            
    Rly PLP             [Latch/Pulse/Off] 
    Rly SLP             [Latch/Pulse/Off]
    Display Pressure    [Yes/No]
     
    INPUT X
    TEMPERATURE
    Scale 4ma           [nnnn] Deg C
    Scale 20ma          [NNNN] Deg C
    Set High BP         [00:00] mm:ss
    Set Low BP          [00:00] mm:ss
    Rly High            [Latch/Pulse/Off]            
    Rly Low             [Latch/Pulse/Off]
    Display Temp        [Yes/No]
    
    INPUT X
    FLOW
    Sensor              [Digital/Analogue]
    If Analogue
        Units           [%/LPS]
        Scale 4ma       [nnnn]
        Scale 20ma      [NNNN}
        Set Low BP      [00:00] mm:ss
        Rly Lo Flow BP  [Latch/Pulse/Off]
        Rly High        [Latch/Pulse/Off]
        Rly Low         [Latch/Pulse/Off]

    If Digital                                  Units could be 'Flo/NoFlo' 
        Set Flow BP     [00:00] mm:ss
        No Flow         [Latch/Pulse/Off]

    Display Flow        [Yes/No]    
    
5 - Use Clock           [Yes/No]                Consider short cut on main menu by setting 0 hours 
    End Runtime         [Latch/Pulse/Off]    
   
    
6 - Utility Menu
    View Log            [Yes/No]
    Clear Log           [Yes/No]

7 - Config Menu
    Time                [dd/mm/yy/ hh:mm:ss]       'Can't do this on 1 line
    Menu Timeout        [00:00] mm:ss
    Contrast            [0-10]
    Brightness          [0-10]
    Pwr Fail Delay      [00:00] mm:ss
    Rly Pulse Duration  [00:00] mm:ss    
    Max Log Entries     [10-200]

    

    




*)
' === Device & configuration ===================================================
Device = 18F2525

Config_Start
  OSC = INTIO67
  FCMEN = OFF
  IESO = OFF
  PWRT = OFF
  BOREN = SBORDIS
  BORV = 3
  WDT = OFF
  WDTPS = 32768
  CCP2MX = PORTC
  PBADEN = OFF
  LPT1OSC = OFF
  MCLRE = On
  STVREN = On
  LVP = OFF
  XINST = OFF
  Debug = OFF
  Cp0 = OFF
  CP1 = OFF
  CP2 = OFF
  CPB = OFF
  CPD = OFF
  WRT0 = OFF
  WRT1 = OFF
  WRT2 = OFF
  WRTC = OFF
  WRTB = OFF
  WRTD = OFF
  EBTR0 = OFF
  EBTR1 = OFF
  EBTR2 = OFF
  EBTRB = OFF
Config_End

OSCCON   = %01110000        ' 8 MHz
OSCTUNE.6 = 1               ' PLL x4 -> 32 MHz
ADCON1  = $0F               ' All digital

All_Digital = True
Declare Xtal = 32
Declare PORTB_Pullups=On

' === Pins =====================================================================
Symbol _BUZZER  = PORTC.2
Symbol _PNP1    = PORTA.4
Symbol _PNP4    = PORTB.3
Symbol _PNP2    = PORTB.4
Symbol _PNP3    = PORTB.5
Symbol _SP1     = PORTC.0
Symbol _Out     = PORTC.1

' LCD (4-bit)
Symbol LCD_D4_PIN = PORTA.0
Symbol LCD_D5_PIN = PORTA.1
Symbol LCD_D6_PIN = PORTA.2
Symbol LCD_D7_PIN = PORTA.3
Symbol LCD_RS_PIN = PORTA.6
Symbol LCD_E_PIN  = PORTA.7

' RTC INT
Symbol RTC_INT   = PORTB.0

' Rotary encoder + button
Symbol _ENC_A    = PORTB.1
Symbol _ENC_B    = PORTB.2
Symbol _ENC_SW   = PORTB.6

TRISA = %00010000
TRISB = %01000111
TRISC = %10000000

' I2C (DS3231M)
Declare SDA_Pin PORTC.4
Declare SCL_Pin PORTC.3
Declare Slow_Bus On

' USART
Declare Hserial_Baud  = 115200
Declare Hserial_Clear = 1
Declare HRSOut_Pin    = PORTC.6
Declare HRSIn_Pin     = PORTC.7

' LCD declares
Declare LCD_Type      = 0
Declare LCD_DTPin     = PORTA.0
Declare LCD_ENPin     = PORTA.7
Declare LCD_RSPin     = PORTA.6
Declare LCD_Interface = 4
Declare LCD_Lines     = 4

' === Variables ================================================================
' Encoder/button state
Dim W_EncoderPos   As Word
Dim B_LastState    As Byte
Dim B_AState       As Byte
Dim B_BState       As Byte
Dim B_ButtonState  As Byte
Dim B_DebA         As Byte
Dim B_DebB         As Byte
Dim B_DebBtn       As Byte
Dim B_RE_Count     As Byte
Dim B_BeepLen      As Byte
Dim b_Long         As Bit
Dim W_BtnHoldMS    As Word
Dim S_Qacc         As SByte
Dim b_ReInitLCD    As Bit
Dim b_ReadRTC      As Bit
Dim b_MTimeout     As Bit

' Date/Time buffers
Dim B_Seconds As Byte
Dim B_Minute  As Byte
Dim B_Hour    As Byte
Dim B_Day     As Byte
Dim B_Date    As Byte
Dim B_Month   As Byte
Dim B_Year    As Byte

' EEPROM settings currently used in UI
Dim B_Version       As Byte    ' kept for schema completeness (not directly used)
Dim B_Log_Pos       As Byte    ' kept
Dim B_Menu_Timeout  As Byte
Dim B_Contrast      As Byte
Dim B_PwrFailDelay  As Byte
Dim B_System_Flags  As Byte    ' kept
Dim B_HT            As Byte    ' kept
Dim B_LFlo          As Byte    ' kept

' Other stored parameters kept (future use)
Dim W_In_1_Cnfg As Word
Dim W_In_2_Cnfg As Word
Dim W_In_3_Cnfg As Word

' --- Analogue input scaling (signed) ---
Dim S_In_1_Scale_4  As SWord
Dim S_In_1_Scale_20 As SWord
Dim S_In_2_Scale_4  As SWord
Dim S_In_2_Scale_20 As SWord
Dim S_In_3_Scale_4  As SWord
Dim S_In_3_Scale_20 As SWord

Dim W_HP        As Word
Dim W_LP        As Word
Dim W_HP_BP     As Word
Dim W_PLP_BP    As Word
Dim W_SLP_BP    As Word
Dim W_HTBP      As Word
Dim W_LFloBP    As Word

Dim L_New_RunTime       As Long
Dim L_Current_RunTime   As Long
Dim L_Last_Run          As Long

Dim L_TimeoutRemain     As Long
Dim b_Escape            As Bit
Dim b_Isolate As Bit
'Dim S_4_Min As SWord
'Dim S_20_Min As SWord


' === Constants ================================================================
Symbol LONG_PRESS     = 2000       ' ms
Symbol Write_To_3231  = %11010000
Symbol Read_From_3231 = %11010001

' EEPROM map (byte/word/long etc.)
Symbol EE_B_Version        = 0x00
Symbol EE_B_Log_Pos        = 0x01
Symbol EE_B_Menu_Timeout   = 0x02
Symbol EE_B_Contrast       = 0x03
Symbol EE_B_Relay_Pulse    = 0x04
Symbol EE_B_System_Flags   = 0x05
Symbol EE_B_HT             = 0x06
Symbol EE_B_LFlo           = 0x07
Symbol EE_B_PwrFailDelay   = 0x08

Symbol EE_W_In_1_Cnfg      = 0x10
Symbol EE_W_In_2_Cnfg      = 0x12
Symbol EE_W_In_3_Cnfg      = 0x14
Symbol EE_W_Con_2_4ma      = 0x16
Symbol EE_W_Con_2_20ma     = 0x18
Symbol EE_W_Con_3_4ma      = 0x1A
Symbol EE_W_Con_3_20ma     = 0x1C
Symbol EE_W_Con_4_4ma      = 0x1E
Symbol EE_W_Con_4_20ma     = 0x20
Symbol EE_W_HP             = 0x22
Symbol EE_W_LP             = 0x24
Symbol EE_W_HP_BP          = 0x26
Symbol EE_W_PLP_BP         = 0x28
Symbol EE_W_SLP_BP         = 0x2A
Symbol EE_W_HTBP           = 0x2C
Symbol EE_W_LFloBP         = 0x2E
Symbol EE_W_RelayPulseSec  = 0x50

Symbol EE_L_New_RunTime    = 0x30
Symbol EE_L_Current_RunTime= 0x36
Symbol EE_L_Last_Run       = 0x3C

Symbol EE_S_W_Con_2        = 0x42
Symbol EE_S_W_Con_3        = 0x44
Symbol EE_S_W_Con_4        = 0x46
Symbol EE_S_W_Word_1       = 0x48
Symbol EE_S_W_Word_2       = 0x4A
Symbol EE_S_W_Word_3       = 0x4C

Symbol EE_NextFree         = 0x4E
Symbol CURRENT_VERSION     = 1

' Blink period for P_Scale (in loop ticks, loop delays ~15ms each)
Symbol SCALE_BLINK_TICKS = 33   ' ~33 * 15ms ˜ 495ms (about 2 Hz)




' === Timer0 1ms tick @32MHz ===================================================
T0CONbits_T0PS2 = 1
T0CONbits_T0PS1 = 0
T0CONbits_T0PS0 = 0
T0CONbits_PSA   = 0
T0CONbits_T0CS  = 0
T0CONbits_T08BIT= 1
TMR0L           = 6

' === Interrupt setup ==========================================================
On_Hardware_Interrupt GoTo ISR_Handler
INTCONbits_T0IF = 0
INTCONbits_T0IE = 1
INTCONbits_GIE  = 1
T0CONbits_TMR0ON= 1

' INT0 from DS3231M 1Hz
TRISB.0                 = 1
INTCON2bits_INTEDG0     = 1
INTCONbits_INT0IF       = 0
INTCONbits_INT0IE       = 1

' === ISR ======================================================================
GoTo over_Interrupt
ISR_Handler:
    Context Save
    Clrwdt                                                              ' watchdog tick (safe)

    ' --- RB0/INT0: 1 Hz SQW from RTC ---------------------------------------
    If INTCONbits_INT0IF = 1 Then
        INTCON2bits_INTEDG0 = ~INTCON2bits_INTEDG0     ' toggle edge to see both
        INTCONbits_INT0IF = 0                           ' clear interrupt flag
        b_ReadRTC = 1                                   'flag for a clock read        
    EndIf

    If INTCONbits_T0IF = 1 Then
        TMR0L = 6                                                       ' 1 ms @ 32MHz, prescaler 1:32
        INTCONbits_T0IF = 0                                             ' clear TMR0 interrupt flag

        Inc B_RE_Count                                                  ' service RE every ~10 ms
        If B_RE_Count > 9 Then                                          ' 10 × 1 ms = ~10 ms
            '--- Debounce samples ------------------------------------------------
            Dim B_NewA  As Byte
            Dim B_NewB  As Byte
            Dim B_NewBtn As Byte

            B_NewA   = PORTB.1                                          ' sample encoder A
            B_NewB   = PORTB.2                                          ' sample encoder B
            B_NewBtn = PORTB.6                                          ' sample button

             ' A debounce (~10-20 ms)
            If B_NewA <> B_AState Then
                Inc B_DebA
                If B_DebA >= 2 Then           ' Now 2  -> ~20 ms
                    B_AState = B_NewA
                    B_DebA = 0
                EndIf
            Else
                B_DebA = 0
            EndIf
            
            ' B debounce (~10-20 ms)
            If B_NewB <> B_BState Then
                Inc B_DebB
                If B_DebB >= 2 Then           ' now 2  -> ~20 ms
                    B_BState = B_NewB
                    B_DebB = 0
                EndIf
            Else
                B_DebB = 0
            EndIf


            ' Button debounce (~20 ms). On *accepted* edge: reload inactivity timer
            If B_NewBtn <> B_ButtonState Then
                Inc B_DebBtn
                If B_DebBtn >= 2 Then
                    B_ButtonState   = B_NewBtn
                    L_TimeoutRemain = B_Menu_Timeout * 1000             ' reload inactivity (ms)
                    b_MTimeout      = 0                                 ' clear timeout flag
                    B_DebBtn        = 0
                EndIf
            Else
                B_DebBtn = 0
            EndIf

            '--- Encoder gray-code decode (full-cycle; 1 step/detent) ------------
            Dim B_Curr As Byte
            B_Curr = (B_AState * 2) + B_BState                   ' 00,01,11,10 (0..3)

            Dim B_Combined As Byte
            B_Combined = (B_LastState * 4) + B_Curr              ' last<<2 | curr

            If b_Isolate = 0 Then                                ' ignore while user holds knob
                ' Accumulate intermediate transitions (directional)
                Select B_Combined
                    Case %0001, %0111, %1110, %1000             ' leftward edge set
                        Dec S_Qacc
                    Case %0010, %1011, %1101, %0100             ' rightward edge set
                        Inc S_Qacc
                EndSelect

                ' Commit exactly one step when we land on detent state (00)
                If B_Curr = 0 Then
                    If S_Qacc >= 2 Then                          ' net right
                        Inc W_EncoderPos
                        L_TimeoutRemain = B_Menu_Timeout * 1000  ' user activity -> reload timeout
                        b_MTimeout = 0
                    ElseIf S_Qacc <= -2 Then                     ' net left
                        Dec W_EncoderPos
                        L_TimeoutRemain = B_Menu_Timeout * 1000
                        b_MTimeout = 0
                    EndIf
                    S_Qacc = 0                                   ' reset for next detent
                EndIf
            EndIf

            B_LastState = B_Curr


            Clear B_RE_Count                                            ' next 10 ms window
        EndIf
    EndIf

    '--- Buzzer one-shot length in ms --------------------------------------------
    If B_BeepLen > 0 Then
        High _BUZZER                                                    ' drive buzzer while >0
        Dec B_BeepLen
    Else
        Low _BUZZER
    EndIf

    '--- Menu inactivity countdown in ms -----------------------------------------
    If L_TimeoutRemain > 0 Then
        Dec L_TimeoutRemain                                             ' 1 ms per ISR tick
        If L_TimeoutRemain = 0 Then
            Set b_MTimeout                                              ' set once when it reaches zero
        EndIf
    EndIf

    '--- Button long-press detection (1 ms tick, uses debounced state) ----------
    ' If the debounced button (RB6) is held low, count ms. When the hold time
    ' reaches 1500 ms, raise b_Long. b_Long is left set until main code clears it.
    If B_ButtonState = 0 Then                         ' button is being held
        If W_BtnHoldMS < 65535 Then Inc W_BtnHoldMS   ' saturate at Word max
        '--- Button long-press event (>=1500 ms) ---
        If W_BtnHoldMS >= 1500 Then
            If b_Long = 0 Then
                Set b_Long                ' latch long-press flag
                B_BeepLen = 255           ' long press beep
                L_TimeoutRemain = B_Menu_Timeout * 1000
                b_MTimeout = 0
                b_ReInitLCD = 1                         'flag to re initialise the LCD
            EndIf
        EndIf

    Else                                              ' button released
        W_BtnHoldMS = 0                               ' reset hold timer
        'Note: do NOT clear b_Long here so the main loop can see it.
        'Clear b_Long in your main code after you handle the long-press.
    EndIf




    Context Restore
over_Interrupt:

' === Boot =====================================================================
DelayMS 500
P_Startup()
DelayMS 100
P_RTC_Gettime()
HRSOut Dec2 B_Date,"/",Dec2 B_Month,"/",Dec2 B_Year,"  ",Dec2 B_Hour,":",Dec2 B_Minute,":",Dec2 B_Seconds,13

' === EEPROM helpers ===========================================================
Proc EEPROM_ReadByte(addr As Byte), Byte
    Result = ERead addr
EndProc
Proc EEPROM_ReadWord(addr As Byte), Word
    Result = ERead addr
EndProc
Proc EEPROM_ReadLong(addr As Byte), Long
    Result = ERead addr
EndProc
Proc EEPROM_ReadDouble(addr As Byte), Double
    Result = ERead addr
EndProc

Proc EEPROM_ReadSWord(addr As Byte), SWord
    Dim raw As Word
    raw = ERead addr
    If (raw & $8000) <> 0 Then          ' MSB set? -> negative
        Result = raw - $10000           ' 65536; maps 0x8000..0xFFFF to -32768..-1
    Else
        Result = raw                    ' 0..32767
    EndIf
EndProc


Proc EEPROM_WriteByte(addr As Byte, value As Byte)
    EWrite addr, [value]
EndProc
Proc EEPROM_WriteWord(addr As Byte, value As Word)
    EWrite addr, [value]
EndProc
Proc EEPROM_WriteLong(addr As Byte, value As Long)
    EWrite addr, [value]
EndProc
Proc EEPROM_WriteDouble(addr As Byte, value As Double)
    EWrite addr, [value]
EndProc

' Write a signed 16-bit (two’s-complement) to EEPROM
Proc EEPROM_WriteSWord(addr As Byte, value As SWord)
    Dim raw As Word
    If value < 0 Then
        raw = value + $10000
    Else
        raw = value
    EndIf
    EWrite addr, [raw]
EndProc
'------------------------------------------------------------
' Only write if different (reduces wear)
Proc EEPROM_Wrt_SWordIC(addr As Byte, value As SWord)
    Dim cur As SWord
    cur = EEPROM_ReadSWord(addr)
    If cur <> value Then
        EEPROM_WriteSWord(addr, value)
    EndIf
EndProc

'------------------------------------------------------------
' Map B_Type -> units string (max 5 chars)

' === Main =====================================================================
Main:
Cls
DelayMS 10

P_InitEEPROM()
P_LoadFromEEPROM()
P_LCD_SafeInit()
DS3231M_Enable1HzSQW()

HRSOut "Startup",13
P_LCD(1,6,"IRRISYS")
P_LCD(2,1,"FW Ver 1.0")
DelayMS 1000
Cls

Dim B_Option As Byte

' Idle screen loop
MAIN_SCREEN:
While 1 = 1
    If b_ReInitLCD = 1 Then Clear b_ReInitLCD : P_LCD_SafeInit()
    If b_ReadRTC = 1 Then P_RTC_Gettime()

    Print At 1,1,"Static ", Dec2 B_Hour, ":", Dec2 B_Minute, ":", Dec2 B_Seconds, "  "
    P_LCD(2,1,"000psi     No Flow")
    P_LCD(3,1,"Note - on this line")
    P_LCD(4,1,"READY")

    ' open top menu on short press; long-press re-inits LCD and stays here
    If B_ButtonState = 0 Then
        DelayMS 30
        While B_ButtonState = 0
            If b_Long = 1 Then Clear b_Long : b_ReInitLCD = 1 : GoTo MAIN_SCREEN
            DelayMS 10
        Wend

        P_Beep(2)
        Clear b_MTimeout : Clear b_Long

        ' New: list-based menu (IDs in any order; 0 = unused)
        ' Include BACK (20) here so user can exit via short press as well.
        B_Option = P_MenuList8("OPTIONS", 1,2,3,20, 0,0,0,0)

        If B_Option = 0 Or B_Option = 20 Then Cls : GoTo MAIN_SCREEN
        GoSub Menus
    EndIf
Wend
End

' === Menus ====================================================================
Menus:
    P_Debounce()
    Select B_Option

        Case 1     ' Main Menu (placeholder)
            ' Add items as needed with P_MenuList8(...)

        Case 2     ' Utility Menu
            Utility_Menu:
            B_Option = P_MenuList8("UTILITY MENU", 4,5,6,20,0,0,0,0)
            If B_Option = 0 Or B_Option = 20 Then
                Cls
                GoTo EXIT_Menus
            EndIf
            
            Select B_Option
                Case 4
                    P_SetDateTime()
                    If b_Escape = 1 Then Clear b_Escape : Cls : GoTo EXIT_Menus
                    GoTo Utility_Menu
                Case 5
                    ' View Log (TBD)
                Case 6
                    ' Clear Log (TBD)
            EndSelect

        Case 3     ' Setup Menu (8 items fit exactly; use long-press/timeout to exit)
            Setup_Menu:
            B_Option = P_MenuList8("SETUP MENU", 7,8,9,10,11,12,13,14)
            If B_Option = 0 Then Cls : GoTo EXIT_Menus

            Select B_Option
                Case 7                                  'MENU TIMEOUT
                    P_MenuTimeout()
                    GoTo Setup_Menu
                Case 8
                    P_SetContrast()                     'CONTRAST
                    GoTo Setup_Menu
                Case 9
                    P_SetPwrDelay()                     'PWR FAIL TEST TIME
                    GoTo Setup_Menu
                Case 10                                 'SETUP INPUT 1
                    'Decide on the type
                    B_Option = P_MenuList8("Input 1",27, 21, 16, 17, 18,20,0,0)                 'Not Used Dig, Pressure, temp, flow
                    Select B_Option                                                             'set the config register     
                        'DIGITAL setup
                         Clear W_In_1_Cnfg                                                      'start from scratch
                         
                         Case 27                                                                'case 27  'NOT used
                            'clear DF1/DF0 if not used
                            ClearBit W_In_1_Cnfg,5                         
                            GoTo Setup_Menu                                                     'back to the setup menu                           
                         Case 21
                            'clear S0/S1 for digital input
                             ClearBit W_In_1_Cnfg,0
                             ClearBit W_In_1_Cnfg,1                                              'set the digital                                                        
                             
                             B_Option = P_MenuList8("Input 1 Digital",22,23,27,0,0,0,0,0)        'if digital - fail high or fail low  
                             Select B_Option
                                Case 22                                                         'fail high
                                    'set DF1
                                    'clear DF0  for Fail high                                                                        
                                    SetBit W_In_1_Cnfg,5
                                    ClearBit W_In_1_Cnfg,6                                   
                                
                                Case 23                                                         'fail low
                                    'clear DF1
                                    'Set DF0 for fail low
                                    ClearBit W_In_1_Cnfg,6
                                    SetBit W_In_1_Cnfg,6
                                    ClearBit W_In_1_Cnfg,6
                                
                                Case Else
                                    GoTo EXIT_Menus
                            EndSelect                            
                            B_Option = P_MenuList8("Input 1 Digital",24,25,28,0,0,0,0,0)                'Fault Action                            
                            Select B_Option
                                Case 24                                                         'Pulse
                                    'set FAH0
                                    'clear FAH1 for PULSE
                                    SetBit W_In_1_Cnfg,07
                                    ClearBit W_In_1_Cnfg,08                                                                                                           
                                Case 25
                                                                                         'Latch
                                    'clear FAH0
                                    'set FAH1 for Latch
                                    SetBit W_In_1_Cnfg,08
                                    ClearBit W_In_1_Cnfg,07    
                                Case 28
                                                                                         'No Action
                                    'cleaf FAH0
                                    'clear FAH1 for NOT USED                                    
                                    ClearBit W_In_1_Cnfg,07
                                    ClearBit W_In_1_Cnfg,08                                    
                                Case Else
                                
                            EndSelect
                            'END DIGITAL SETUP FOR NOW                            

                        Case 16     'SETUP PRESSURE
                            'steps are - set scale, set output for fail high, primary low and secondary lo
                            'set scale 
                            P_Scale(0,1)                                        'values are set in the procedure
                            'set relay behaviour
                            HRSOut "Call P_Output()",13
                            P_Output("Pressure Output",0,1)                     'call with type (pressure) and input (1) in this case                                                                                     
                            
                        Case 17     'SETUP TEMPERATURE
                            'B_Option



                        Case 18     'SETUP FLOW

                        Case Else                                                               'escape
                        GoTo EXIT_Menus
                        
                    EndSelect


                Case 11                 'SETUP INPUT 2
 
                Case 12                 'SETUP INPUT 3

                Case 13                 ' End Runtime (TBD)

                Case 14
                    P_SetPulse()
                    GoTo Setup_Menu
            EndSelect


        Case 5,6,15,16,17,18,19
            ' Implement as needed                

        Case 20  ' BACK
            ' handled by caller

    EndSelect

    DelayMS 100
EXIT_Menus:
    Cls
    Clear b_MTimeout
    P_Debounce()
Return

' === Settings I/O =============================================================
Proc P_LoadFromEEPROM()
    Dim storedVer As Byte
    storedVer = EEPROM_ReadByte(EE_B_Version)

    If storedVer = CURRENT_VERSION Then
        B_Log_Pos       = EEPROM_ReadByte(EE_B_Log_Pos)
        B_Menu_Timeout  = EEPROM_ReadByte(EE_B_Menu_Timeout)
        B_Contrast      = EEPROM_ReadByte(EE_B_Contrast)
        B_System_Flags  = EEPROM_ReadByte(EE_B_System_Flags)
        B_HT            = EEPROM_ReadByte(EE_B_HT)
        B_LFlo          = EEPROM_ReadByte(EE_B_LFlo)
        B_PwrFailDelay  = EEPROM_ReadByte(EE_B_PwrFailDelay)

        W_In_1_Cnfg    = EEPROM_ReadWord(EE_W_In_1_Cnfg)
        W_In_2_Cnfg    = EEPROM_ReadWord(EE_W_In_2_Cnfg)
        W_In_3_Cnfg    = EEPROM_ReadWord(EE_W_In_3_Cnfg)

        ' --- Analogue scaling (signed), reuse existing addresses ---
        S_In_1_Scale_4  = EEPROM_ReadSWord(EE_W_Con_2_4ma)
        S_In_1_Scale_20 = EEPROM_ReadSWord(EE_W_Con_2_20ma)
        S_In_2_Scale_4  = EEPROM_ReadSWord(EE_W_Con_3_4ma)
        S_In_2_Scale_20 = EEPROM_ReadSWord(EE_W_Con_3_20ma)
        S_In_3_Scale_4  = EEPROM_ReadSWord(EE_W_Con_4_4ma)
        S_In_3_Scale_20 = EEPROM_ReadSWord(EE_W_Con_4_20ma)


        W_HP            = EEPROM_ReadWord(EE_W_HP)
        W_LP            = EEPROM_ReadWord(EE_W_LP)
        W_HP_BP         = EEPROM_ReadWord(EE_W_HP_BP)
        W_PLP_BP        = EEPROM_ReadWord(EE_W_PLP_BP)
        W_SLP_BP        = EEPROM_ReadWord(EE_W_SLP_BP)
        W_HTBP          = EEPROM_ReadWord(EE_W_HTBP)
        W_LFloBP        = EEPROM_ReadWord(EE_W_LFloBP)

        L_New_RunTime     = EEPROM_ReadLong(EE_L_New_RunTime)
        L_Current_RunTime = EEPROM_ReadLong(EE_L_Current_RunTime)
        L_Last_Run        = EEPROM_ReadLong(EE_L_Last_Run)

        If B_Menu_Timeout = 0 Then B_Menu_Timeout = 120
    Else
        P_InitEEPROM()
        B_Menu_Timeout = 120
    EndIf
EndProc

Proc P_InitEEPROM()
    Dim storedVer As Byte
    storedVer = EEPROM_ReadByte(EE_B_Version)

    If storedVer <> CURRENT_VERSION Then
        EEPROM_WriteByte(EE_B_Version,      0)
        EEPROM_WriteByte(EE_B_Log_Pos,      0)
        EEPROM_WriteByte(EE_B_Menu_Timeout, 120)
        EEPROM_WriteByte(EE_B_Contrast,     127)
        EEPROM_WriteByte(EE_B_System_Flags, 0)
        EEPROM_WriteByte(EE_B_HT,           40)
        EEPROM_WriteByte(EE_B_LFlo,         25)
        EEPROM_WriteByte(EE_B_PwrFailDelay, 5)

        EEPROM_WriteWord(EE_W_In_1_Cnfg,       444)
        EEPROM_WriteWord(EE_W_In_2_Cnfg,       1293)
        EEPROM_WriteWord(EE_W_In_3_Cnfg,       1026)
        
        ' Signed defaults (reuse same addresses)
        EEPROM_WriteSWord(EE_W_Con_2_4ma,     0)
        EEPROM_WriteSWord(EE_W_Con_2_20ma,  360)
        EEPROM_WriteSWord(EE_W_Con_3_4ma,     0)
        EEPROM_WriteSWord(EE_W_Con_3_20ma,  100)
        EEPROM_WriteSWord(EE_W_Con_4_4ma,     0)
        EEPROM_WriteSWord(EE_W_Con_4_20ma,  100)

        EEPROM_WriteWord(EE_W_HP,          300)
        EEPROM_WriteWord(EE_W_LP,           30)
        EEPROM_WriteWord(EE_W_HP_BP,        0)
        EEPROM_WriteWord(EE_W_PLP_BP,     120)
        EEPROM_WriteWord(EE_W_SLP_BP,      60)
        EEPROM_WriteWord(EE_W_HTBP,        60)
        EEPROM_WriteWord(EE_W_LFloBP,      60)
        EEPROM_WriteWord(EE_W_RelayPulseSec, 5)

        EEPROM_WriteDouble(EE_L_New_RunTime,     0)
        EEPROM_WriteDouble(EE_L_Current_RunTime, 0)
        EEPROM_WriteDouble(EE_L_Last_Run,        0)

        EEPROM_WriteSWord(EE_S_W_Con_2, 0)
        EEPROM_WriteSWord(EE_S_W_Con_3, 0)
        EEPROM_WriteSWord(EE_S_W_Con_4, 0)
        EEPROM_WriteSWord(EE_S_W_Word_1,  0)
        EEPROM_WriteSWord(EE_S_W_Word_2,  0)
        EEPROM_WriteSWord(EE_S_W_Word_3,  0)

        EEPROM_WriteByte(EE_B_Version, CURRENT_VERSION)
    EndIf
EndProc

' === UI helpers ===============================================================
Proc P_LCD(B_Ln As Byte, B_Pos As Byte, S_Data As String * 20)
    Print At B_ln, B_pos, S_data
EndProc

Proc P_Debounce()
    While B_ButtonState = 0 : DelayMS 10 : Wend
    DelayMS 100
EndProc

Proc P_Beep(B_Len As Byte)
    Select B_len
        Case 1
            B_BeepLen = 1
        Case 2
            B_BeepLen = 25
        Case 3
            B_BeepLen = 75
        Case 4
            B_BeepLen = 150
        Case 5
            B_BeepLen = 255
    EndSelect
EndProc

Proc P_Exit_OK()
    Dim B_Beeps As Byte
    For B_Beeps = 0 To 2
        P_Beep(2)
        DelayMS 200
    Next
EndProc

Proc P_Startup()
    Dim cycle As Byte
    For cycle = 1 To 5
        P_Beep(3)
        DelayMS 200
    Next
EndProc

Proc P_P_Timeout()
    Dim B_Cycle As Byte
    For B_cycle = 1 To 5
        P_Beep(2)
        DelayMS 400
    Next
EndProc

' === Menu: list-based (=8 IDs) ===============================================
' Build and show a menu from up to EIGHT absolute IDs (1..48).
' Pass 0 for unused slots. Returns selected ID, or 0 on cancel/timeout.
Proc P_MenuList8(S_Title As String * 18, I1 As Byte, I2 As Byte, I3 As Byte, I4 As Byte, I5 As Byte, I6 As Byte, I7 As Byte, I8 As Byte), Byte
    Dim B_IDs[8] As Byte
    Dim B_Count As Byte, B_I As Byte

    ' Collect valid IDs (1..48) in supplied order
    B_Count = 0
    If I1 >=1 And I1 <=48 Then B_IDs[B_Count]=I1: Inc B_Count
    If I2 >=1 And I2 <=48 Then B_IDs[B_Count]=I2: Inc B_Count
    If I3 >=1 And I3 <=48 Then B_IDs[B_Count]=I3: Inc B_Count
    If I4 >=1 And I4 <=48 Then B_IDs[B_Count]=I4: Inc B_Count
    If I5 >=1 And I5 <=48 Then B_IDs[B_Count]=I5: Inc B_Count
    If I6 >=1 And I6 <=48 Then B_IDs[B_Count]=I6: Inc B_Count
    If I7 >=1 And I7 <=48 Then B_IDs[B_Count]=I7: Inc B_Count
    If I8 >=1 And I8 <=48 Then B_IDs[B_Count]=I8: Inc B_Count

    ' Guards
    Clear b_MTimeout : Clear b_Long : Clear b_ReInitLCD
    L_TimeoutRemain = B_Menu_Timeout*1000
    P_Debounce()

    ' UI state
    Dim B_Index As Byte
    Dim B_First As Byte
    Dim B_PrevIndex As Byte
    Dim B_PrevFirst As Byte
    Dim B_Dirty As Byte
    Dim W_LastPos As Word
    Dim S_Line As String * 18
    Dim B_Len As Byte

    Cls
    Print At 1,1, S_Title

    B_Index=0 : B_PrevIndex=255 : B_PrevFirst=255 : B_Dirty=1
    W_LastPos = W_EncoderPos

    While 1 = 1
        ' 3-line window
        If B_Index < 2 Then
            B_First = 0
        Else
            B_First = B_Index - 2
        EndIf
        If B_Count >= 3 Then
            If B_First > (B_Count - 3) Then B_First = B_Count - 3
        Else
            B_First = 0
        EndIf

        If B_First <> B_PrevFirst Or B_Index <> B_PrevIndex Then B_Dirty = 1

        ' redraw
        If B_Dirty = 1 Then
            For B_I = 0 To 2
                Print At B_I + 2,1,"                    "
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
            B_PrevFirst = B_First : B_PrevIndex = B_Index : B_Dirty = 0
        EndIf

        ' encoder
        If W_EncoderPos > W_LastPos Then
            W_LastPos = W_EncoderPos
            If B_Index < (B_Count - 1) Then Inc B_Index : P_Beep(1)
        ElseIf W_EncoderPos < W_LastPos Then
            W_LastPos = W_EncoderPos
            If B_Index > 0 Then Dec B_Index : P_Beep(1)
        EndIf

        ' button (short=select, long=escape)
        If B_ButtonState = 0 Then
            While B_ButtonState = 0
                If b_Long = 1 Then
                    Clear b_Long : Clear b_ReInitLCD
                    If B_BeepLen = 0 Then P_Beep(5)
                    Result = 0
                    GoTo Exit_P_MenuList8
                EndIf
                DelayMS 10
            Wend
            P_Beep(2)
            Result = B_IDs[B_Index]
            GoTo Exit_P_MenuList8
        EndIf

        ' timeout
        If b_MTimeout = 1 Then
            Clear b_MTimeout : Clear b_ReInitLCD
            If B_BeepLen = 0 Then P_Beep(5)
            Result = 0
            GoTo Exit_P_MenuList8
        EndIf

        DelayMS 50
    Wend
Exit_P_MenuList8:
EndProc

' === Menu Strings =============================================================
' Base IDs 1..20
Menu_Item_List:
Dim S_String1_F   As Flash8 = "Main Menu", 0
Dim S_String2_F   As Flash8 = "Utility Menu", 0
Dim S_String3_F   As Flash8 = "Setup Menu", 0
Dim S_String4_F   As Flash8 = "Date and Time", 0
Dim S_String5_F   As Flash8 = "View Log", 0
Dim S_String6_F   As Flash8 = "Clear Log", 0
Dim S_String7_F   As Flash8 = "Menu Timeout", 0
Dim S_String8_F   As Flash8 = "Contrast", 0
Dim S_String9_F   As Flash8 = "Pwr Fail Delay", 0
Dim S_String10_F  As Flash8 = "Input 1", 0
Dim S_String11_F  As Flash8 = "Input 2", 0
Dim S_String12_F  As Flash8 = "Input 3", 0
Dim S_String13_F  As Flash8 = "End Runtime", 0
Dim S_String14_F  As Flash8 = "Pulse Duration", 0
Dim S_String15_F  As Flash8 = "RunTime", 0
Dim S_String16_F  As Flash8 = "Pressure", 0
Dim S_String17_F  As Flash8 = "Temperature", 0
Dim S_String18_F  As Flash8 = "Flow", 0
Dim S_String19_F  As Flash8 = "Vacuum", 0
Dim S_String20_F  As Flash8 = "BACK", 0
Dim S_String21_F  As Flash8 = "Digital", 0
Dim S_String22_F  As Flash8 = "Fail HIGH", 0
Dim S_String23_F  As Flash8 = "Fail LOW", 0
Dim S_String24_F  As Flash8 = "Pulse", 0
Dim S_String25_F  As Flash8 = "Latch", 0
Dim S_String26_F  As Flash8 = "Set Scale", 0
Dim S_String27_F  As Flash8 = "Not Used", 0
Dim S_String28_F  As Flash8 = "No Action", 0
Dim S_String29_F  As Flash8 = "Fail Primary Low", 0
Dim S_String30_F  As Flash8 = "Fail Secondary Low", 0
Dim S_String31_F  As Flash8 = "Extra Item 11", 0
Dim S_String32_F  As Flash8 = "Extra Item 12", 0
Dim S_String33_F  As Flash8 = "Extra Item 13", 0
Dim S_String34_F  As Flash8 = "Extra Item 14", 0
Dim S_String35_F  As Flash8 = "Extra Item 15", 0
Dim S_String36_F  As Flash8 = "Extra Item 16", 0
Dim S_String37_F  As Flash8 = "Extra Item 17", 0
Dim S_String38_F  As Flash8 = "Extra Item 18", 0
Dim S_String39_F  As Flash8 = "Extra Item 19", 0
Dim S_String40_F  As Flash8 = "Extra Item 20", 0

' Lookup by absolute ID (1..40)
Proc P_GetMenuString(B_ID As Byte), String * 18
    Select B_ID
        Case 1
            Result = S_String1_F
        Case 2
            Result = S_String2_F
        Case 3
            Result = S_String3_F
        Case 4
            Result = S_String4_F
        Case 5
            Result = S_String5_F
        Case 6
            Result = S_String6_F
        Case 7
            Result = S_String7_F
        Case 8
            Result = S_String8_F
        Case 9
            Result = S_String9_F
        Case 10
            Result = S_String10_F
        Case 11
            Result = S_String11_F
        Case 12
            Result = S_String12_F
        Case 13
            Result = S_String13_F
        Case 14
            Result = S_String14_F
        Case 15
            Result = S_String15_F
        Case 16
            Result = S_String16_F
        Case 17
            Result = S_String17_F
        Case 18
            Result = S_String18_F
        Case 19
            Result = S_String19_F
        Case 20
            Result = S_String20_F
        Case 21
            Result = S_String21_F
        Case 22
            Result = S_String22_F
        Case 23
            Result = S_String23_F
        Case 24
            Result = S_String24_F
        Case 25
            Result = S_String25_F
        Case 26
            Result = S_String26_F
        Case 27
            Result = S_String27_F
        Case 28
            Result = S_String28_F
        Case 29
            Result = S_String29_F
        Case 30
            Result = S_String30_F
        Case 31
            Result = S_String31_F
        Case 32
            Result = S_String32_F
        Case 33
            Result = S_String33_F
        Case 34
            Result = S_String34_F
        Case 35
            Result = S_String35_F
        Case 36
            Result = S_String36_F
        Case 37
            Result = S_String37_F
        Case 38
            Result = S_String38_F
        Case 39
            Result = S_String39_F
        Case 40
            Result = S_String40_F
        Case Else
            Result = " "
    End Select
EndProc

' === LCD robust init ==========================================================
Proc _LCD_SetHiNibble(B_Val As Byte)
    If B_Val.7 = 1 Then High LCD_D7_PIN: Else: Low LCD_D7_PIN: EndIf
    If B_Val.6 = 1 Then High LCD_D6_PIN: Else: Low LCD_D6_PIN: EndIf
    If B_Val.5 = 1 Then High LCD_D5_PIN: Else: Low LCD_D5_PIN: EndIf
    If B_Val.4 = 1 Then High LCD_D4_PIN: Else: Low LCD_D4_PIN: EndIf
EndProc

Proc _LCD_PulseE()
    High LCD_E_PIN : DelayUS 1
    Low  LCD_E_PIN : DelayUS 50
EndProc

Proc P_LCD_SafeInit()
    DelayMS 250
    Low PORTA.5
    Low LCD_RS_PIN : Low LCD_E_PIN
    Low LCD_D4_PIN : Low LCD_D5_PIN : Low LCD_D6_PIN : Low LCD_D7_PIN
    DelayMS 50
    _LCD_SetHiNibble($30) : _LCD_PulseE() : DelayMS 5
    _LCD_SetHiNibble($30) : _LCD_PulseE() : DelayMS 2
    _LCD_SetHiNibble($30) : _LCD_PulseE() : DelayMS 2
    _LCD_SetHiNibble($20) : _LCD_PulseE() : DelayMS 2
    _LCD_SetHiNibble($30) : _LCD_PulseE() : DelayMS 5
    _LCD_SetHiNibble($30) : _LCD_PulseE() : DelayMS 2
    _LCD_SetHiNibble($30) : _LCD_PulseE() : DelayMS 2
    _LCD_SetHiNibble($20) : _LCD_PulseE() : DelayMS 2
    Print $FE, $28 : DelayUS 80
    Print $FE, $08 : DelayUS 80
    Print $FE, $01 : DelayMS 3
    Print $FE, $06 : DelayUS 80
    Print $FE, $0C : DelayUS 80
EndProc

' === Value editors ============================================================
' Adjust a byte field with encoder; short press accept; long press returns original.
Proc P_SetField(B_Ln As Byte, B_col As Byte, B_Zero As Byte, B_Value As Byte, B_Min As Byte, B_Max As Byte, ByRef W_LastPos As Word), Word
    Dim B_Orig As Byte
    Dim B_Changed As Byte

    P_Debounce()
    B_Orig    = B_Value
    B_Changed = 1
    W_EncoderPos = W_LastPos

    While 1 = 1
        If W_EncoderPos > W_LastPos Then
            W_LastPos = W_EncoderPos
            If B_Value < B_Max Then Inc B_Value : P_Beep(1) : B_Changed = 1
        EndIf
        If W_EncoderPos < W_LastPos Then
            W_LastPos = W_EncoderPos
            If B_Value > B_Min Then Dec B_Value : P_Beep(1) : B_Changed = 1
        EndIf

        If B_Changed = 1 Then
            B_Changed = 0
            Select B_Zero
                Case 2
                    P_LCD(B_Ln, B_col, Str$(Dec2 B_Value))
                Case 3
                    P_LCD(B_Ln, B_col, Str$(Dec3 B_Value))
                Case 5
                    P_LCD(B_Ln, B_col, Str$(Dec5 B_Value))
            EndSelect
        EndIf

        If b_Long = 1 Then Result = B_Orig : GoTo Exit_P_SetField

        If B_ButtonState = 0 Then
            P_Beep(2)
            DelayMS 100
            While B_ButtonState = 0
                If b_Long = 1 Then Result = B_Orig : GoTo Exit_P_SetField
                DelayMS 20
            Wend
            DelayMS 50
            Result = B_Value
            GoTo Exit_P_SetField
        EndIf

        DelayMS 10
    Wend
Exit_P_SetField:
EndProc

' Signed word editor
Proc P_Signed(S_Current As SWord, S_Upper As SWord, S_Lower As SWord), SWord
    P_Beep(3)
    P_Debounce()

    Dim S_Value   As SWord
    Dim W_LastPos As Word
    Dim B_Changed As Byte

    Clear b_MTimeout
    If S_Upper < S_Lower Then Dim S_Tmp As SWord : S_Tmp = S_Upper : S_Upper = S_Lower : S_Lower = S_Tmp

    Cls
    S_Value = S_Current
    If S_Value < S_Lower Then S_Value = S_Lower
    If S_Value > S_Upper Then S_Value = S_Upper

    If S_Lower < 0 Then Print At 2,1,S_Current Else Print At 3,1,Dec5 S_Current

    W_LastPos = W_EncoderPos
    B_Changed = 0

    While 1 = 1
        If W_EncoderPos > W_LastPos Then
            W_LastPos = W_EncoderPos
            If S_Value < S_Upper Then Inc S_Value : P_Beep(1) : B_Changed = 1
        ElseIf W_EncoderPos < W_LastPos Then
            W_LastPos = W_EncoderPos
            If S_Value > S_Lower Then Dec S_Value : P_Beep(1) : B_Changed = 1
        EndIf

        If B_Changed = 1 Then
            If S_Lower < 0 Then Print At 2,1,S_Value Else Print At 3,1,Dec5 S_Value
            B_Changed = 0
        EndIf

        If B_ButtonState = 0 Then
            P_Exit_OK()
            P_Debounce()
            Result = S_Value
            GoTo Exit_P_Signed
        EndIf

        DelayMS 50
    Wend
Exit_P_Signed:
EndProc


'' Edit a signed value at a specific line/column, showing it in [     ].
'' - B_Ln, B_Col : where the numeric field starts (the '[' is drawn at B_Col-1)
'' - S_Current   : starting value (SWord)
'' - S_Min/Max   : allowed range (SWord)
'' - W_LastPos   : encoder position (ByRef) so the next field can continue smoothly
'' Returns the committed value on SHORT press.
'' On LONG press or timeout, it just returns immediately; the caller should
'' detect/handle via P_UserAborted() (as your P_Scale already does).
'Proc P_SignedAt(B_Ln As Byte, B_Col As Byte, _
'                S_Current As SWord, S_Min As SWord, S_Max As SWord, _
'                ByRef W_LastPos As Word), SWord

'    Dim S_Value   As SWord
'    Dim B_Changed As Byte
'    Dim W_Mag     As Word

'    ' Clamp start
'    S_Value = S_Current
'    If S_Value < S_Min Then S_Value = S_Min
'    If S_Value > S_Max Then S_Value = S_Max

'    ' Take over the encoder where the caller left it
'    W_LastPos = W_EncoderPos
'    B_Changed = 1

'    ' Helper to (re)draw the bracketed field with a 5-char number area
'    ' [xxxxx] where x is sign+digits (we write either "-dddd" or "ddddd")
'Draw_Field:
'    Print At B_Ln, B_Col-1, "[     ]"          ' clear the 5-char field between brackets
'    If S_Value < 0 Then
'        W_Mag = 0 - S_Value                     ' absolute magnitude
'        Print At B_Ln, B_Col, "-"
'        Print At B_Ln, B_Col+1, Dec4 W_Mag      ' 4 digits after the sign
'    Else
'        W_Mag = S_Value
'        Print At B_Ln, B_Col, Dec5 W_Mag        ' 5 digits, zero-padded
'    EndIf

'    While 1 = 1
'        ' Cancel paths: let caller react via P_UserAborted()
'        If b_Long = 1 Then
'            Result = S_Value
'            GoTo Exit_P_SignedAt
'        EndIf
'        If b_MTimeout = 1 Then
'            Result = S_Value
'            GoTo Exit_P_SignedAt
'        EndIf

'        ' Encoder up
'        If W_EncoderPos > W_LastPos Then
'            W_LastPos = W_EncoderPos
'            If S_Value < S_Max Then
'                Inc S_Value
'                P_Beep(1)
'                B_Changed = 1
'            EndIf
'        EndIf

'        ' Encoder down
'        If W_EncoderPos < W_LastPos Then
'            W_LastPos = W_EncoderPos
'            If S_Value > S_Min Then
'                Dec S_Value
'                P_Beep(1)
'                B_Changed = 1
'            EndIf
'        EndIf

'        ' Redraw only when changed
'        If B_Changed = 1 Then
'            B_Changed = 0
'            GoTo Draw_Field
'        EndIf

'        ' Short press = commit
'        If B_ButtonState = 0 Then
'            P_Beep(2)
'            ' wait for release, still allow long during hold
'            While B_ButtonState = 0
'                If b_Long = 1 Then
'                    Result = S_Value
'                    GoTo Exit_P_SignedAt
'                EndIf
'                DelayMS 10
'            Wend
'            Result = S_Value
'            GoTo Exit_P_SignedAt
'        EndIf

'        DelayMS 15
'    Wend

'Exit_P_SignedAt:
'EndProc











' Time editor (HH:MM or MM:SS) with bounds
Proc P_TEdit(B_Mode As Byte, L_Current As Long, L_Min As Long, L_Max As Long), Long
    Dim B_Big As Byte, B_Small As Byte
    Dim W_LastPos As Word
    Dim B_Changed As Byte
    Dim L_ScaleBig As Long, L_ScaleSmall As Long
    Dim B_BigMax As Byte
    Dim B_SmallMin As Byte, B_SmallMax As Byte
    Dim L_Tmp As Long, L_Total As Long
    Dim L_Q As Long
    Dim B_Ticks As Byte, B_Flash As Byte
    Dim B_Next As Byte

    If L_Current < L_Min Then L_Current = L_Min
    If L_Current > L_Max Then L_Current = L_Max

    If B_Mode = 0 Then
        L_ScaleBig   = 3600 : L_ScaleSmall = 60 : Print At 3,1,"HH:MM"
    Else
        L_ScaleBig   = 60   : L_ScaleSmall = 1  : Print At 3,1,"MM:SS"
    EndIf

    L_Tmp = L_Current / L_ScaleBig : If L_Tmp > 99 Then L_Tmp = 99
    B_Big = L_Tmp
    L_Tmp = B_Big : L_Tmp = L_Tmp * L_ScaleBig
    L_Tmp = L_Current - L_Tmp
    L_Tmp = L_Tmp / L_ScaleSmall : If L_Tmp > 59 Then L_Tmp = 59
    B_Small = L_Tmp

    Print At 4,1, Dec2 B_Big : Print At 4,3, ":" : Print At 4,4, Dec2 B_Small

    W_LastPos = W_EncoderPos
    B_Ticks = 0 : B_Flash = 1
    While 1 = 1
        If b_Long = 1 Then Clear b_Long : Result = L_Current : GoTo Exit_P_TEdit
        If b_MTimeout = 1 Then Clear b_MTimeout : P_P_Timeout() : Result = L_Current : GoTo Exit_P_TEdit

        L_Tmp = L_Max / L_ScaleBig : If L_Tmp > 99 Then L_Tmp = 99
        B_BigMax = L_Tmp

        If W_EncoderPos > W_LastPos Then
            W_LastPos = W_EncoderPos
            If B_Big < B_BigMax Then Inc B_Big : B_Changed = 1 : P_Beep(1)
        ElseIf W_EncoderPos < W_LastPos Then
            W_LastPos = W_EncoderPos
            If B_Big > 0 Then Dec B_Big : B_Changed = 1 : P_Beep(1)
        EndIf

        If B_Changed = 1 Then
            B_Changed = 0
            If B_Flash = 1 Then Print At 4,1, Dec2 B_Big
        EndIf

        Inc B_Ticks
        If B_Ticks >= 10 Then
            B_Ticks = 0 : B_Flash = ~B_Flash
            If B_Flash = 1 Then
                Print At 4,1, Dec2 B_Big
            Else
                Print At 4,1, "  "
            EndIf

        EndIf

        If B_ButtonState = 0 Then
            Print At 4,1, Dec2 B_Big
            While B_ButtonState = 0
                If b_Long = 1 Then Clear b_Long : Result = L_Current : GoTo Exit_P_TEdit
                If b_MTimeout = 1 Then Clear b_MTimeout : P_P_Timeout() : Result = L_Current : GoTo Exit_P_TEdit
                DelayMS 10
            Wend
            P_Beep(2)
            GoTo Edit_Small
        EndIf
        DelayMS 15
    Wend

Edit_Small:
    L_Tmp = B_Big : L_Tmp = L_Tmp * L_ScaleBig
    If L_Max > L_Tmp Then L_Tmp = L_Max - L_Tmp Else L_Tmp = 0
    L_Q = L_Tmp / L_ScaleSmall : If L_Q > 59 Then L_Q = 59
    B_SmallMax = L_Q

    L_Tmp = B_Big : L_Tmp = L_Tmp * L_ScaleBig
    If L_Min > L_Tmp Then
        L_Tmp = L_Min - L_Tmp
        L_Q = L_Tmp + (L_ScaleSmall - 1)
        L_Q = L_Q / L_ScaleSmall
        If L_Q > 59 Then L_Q = 59
        B_SmallMin = L_Q
    Else
        B_SmallMin = 0
    EndIf

    If B_Small < B_SmallMin Then B_Small = B_SmallMin
    If B_Small > B_SmallMax Then B_Small = B_SmallMax
    Print At 4,4, Dec2 B_Small

    W_LastPos = W_EncoderPos
    B_Ticks = 0 : B_Flash = 1
    While 1 = 1
        If b_Long = 1 Then Clear b_Long : Result = L_Current : GoTo Exit_P_TEdit
        If b_MTimeout = 1 Then Clear b_MTimeout : P_P_Timeout() : Result = L_Current : GoTo Exit_P_TEdit

        If W_EncoderPos > W_LastPos Then
            W_LastPos = W_EncoderPos
            B_Next = B_Small + 1
            If B_Next > B_SmallMax Then
                B_Small = B_SmallMax
            Else
                B_Small = B_Next : B_Changed = 1 : P_Beep(1)
            EndIf
        ElseIf W_EncoderPos < W_LastPos Then
            W_LastPos = W_EncoderPos
            If B_Small > B_SmallMin Then
                Dec B_Small : B_Changed = 1 : P_Beep(1)
            Else
                B_Small = B_SmallMin
            EndIf
        EndIf

        If B_Changed = 1 Then
            B_Changed = 0
            If B_Flash = 1 Then Print At 4,4, Dec2 B_Small
        EndIf

        Inc B_Ticks
        If B_Ticks >= 10 Then
            B_Ticks = 0 : B_Flash = ~B_Flash
            If B_Flash = 1 Then
                Print At 4,4, Dec2 B_Small
            Else
                Print At 4,4, "  "
            EndIf

        EndIf

        If B_ButtonState = 0 Then
            Print At 4,4, Dec2 B_Small
            While B_ButtonState = 0
                If b_Long = 1 Then Clear b_Long : Result = L_Current : GoTo Exit_P_TEdit
                If b_MTimeout = 1 Then Clear b_MTimeout : P_P_Timeout() : Result = L_Current : GoTo Exit_P_TEdit
                DelayMS 10
            Wend
            P_Beep(2)

            L_Total = B_Big : L_Total = L_Total * L_ScaleBig
            L_Tmp   = B_Small : L_Tmp = L_Tmp * L_ScaleSmall
            L_Total = L_Total + L_Tmp
            If L_Total < L_Min Then L_Total = L_Min
            If L_Total > L_Max Then L_Total = L_Max

            Result = L_Total
            GoTo Exit_P_TEdit
        EndIf

        DelayMS 15
    Wend
Exit_P_TEdit:
    Print At 4,1, Dec2 B_Big
    Print At 4,4, Dec2 B_Small
EndProc

' === Date/Time UI =============================================================
Proc P_SetDateTime()
    Cls : P_Beep(3) : P_Debounce() : Clear b_Escape

    Dim W_LastPos As Word
    Dim B_Date0   As Byte, B_Month0 As Byte, B_Year0 As Byte
    Dim B_Hour0   As Byte, B_Minute0 As Byte, B_Sec0 As Byte

    DelayMS 200
    P_LCD(1,1,"SET DATE AND TIME")

    P_RTC_Gettime()
    B_Date0   = B_Date   : B_Month0  = B_Month : B_Year0 = B_Year
    B_Hour0   = B_Hour   : B_Minute0 = B_Minute: B_Sec0  = B_Seconds

    P_LCD(3,1, Str$(Dec2 B_Date) + "/MM/YY HH:MM:SS")

    B_Date   = P_SetField(3,1,2,B_Date,  1,31,W_LastPos) : If b_Long = 1 Then GoTo Abort_NoCommit
    P_LCD(3,1, Str$(Dec2 B_Date) + "/" + Str$(Dec2 B_Month))

    B_Month  = P_SetField(3,4,2,B_Month, 1,12,W_LastPos) : If b_Long = 1 Then GoTo Abort_NoCommit
    P_LCD(3,4, Str$(Dec2 B_Month))

    B_Year   = P_SetField(3,7,2,B_Year, 25,99,W_LastPos) : If b_Long = 1 Then GoTo Abort_NoCommit
    P_LCD(3,7, Str$(Dec2 B_Year))

    B_Hour   = P_SetField(3,10,2,B_Hour, 0,23,W_LastPos) : If b_Long = 1 Then GoTo Abort_NoCommit
    P_LCD(3,10, Str$(Dec2 B_Hour))

    B_Minute = P_SetField(3,13,2,B_Minute,0,59,W_LastPos) : If b_Long = 1 Then GoTo Abort_NoCommit
    P_LCD(3,13, Str$(Dec2 B_Minute))

    B_Seconds= P_SetField(3,16,2,B_Seconds,0,59,W_LastPos) : If b_Long = 1 Then GoTo Abort_NoCommit
    P_LCD(3,16, Str$(Dec2 B_Seconds))

    P_RTC_Settime()
    GoTo Exit_P_SetDateTime

Abort_NoCommit:
    B_Date = B_Date0 : B_Month = B_Month0 : B_Year = B_Year0
    B_Hour = B_Hour0 : B_Minute= B_Minute0: B_Seconds = B_Sec0
    b_Escape = 1

Exit_P_SetDateTime:
    Clear b_Long
    Cls
EndProc

' === RTC helpers ==============================================================
Proc B2BCD(B_convert As Byte), Byte
    Dim temp1 As Byte, temp2 As Byte
    temp1 = Dig B_convert, 0
    temp2 = Dig B_convert, 1
    temp2 = temp2 << 4
    Result = temp2 ^ temp1
EndProc

Proc B2BIN(B_convert As Byte), Byte
    Dim t1 As Byte, t2 As Byte
    t1 = B_convert & $0F
    t2 = (B_convert & $F0) >> 4
    t2 = t2 * 10
    Result = t1 + t2
EndProc

Proc P_RTC_Settime()
    B_Seconds = B2BCD(B_Seconds)
    B_Minute  = B2BCD(B_Minute)
    B_Hour    = B2BCD(B_Hour)
    B_Day     = B_Day & $07
    B_Date    = B2BCD(B_Date)
    B_Month   = B2BCD(B_Month)
    B_Year    = B2BCD(B_Year)
    B_Hour    = B_Hour & $3F
    BusOut Write_To_3231, 0, [B_Seconds, B_Minute, B_Hour, B_Day, B_Date, B_Month, B_Year]
    P_RTC_Gettime()
EndProc

Proc P_RTC_Gettime()
    Clear b_ReadRTC
    Dim B_DayRaw As Byte, B_HourRaw As Byte
    BusIn Read_From_3231, 0, [B_Seconds, B_Minute, B_HourRaw, B_DayRaw, B_Date, B_Month, B_Year]
    B_Seconds = B_Seconds & $7F
    B_Minute  = B_Minute  & $7F
    B_HourRaw = B_HourRaw & $3F
    B_Month   = B_Month   & $1F
    B_Day     = B_DayRaw  & $07
    B_Seconds = B2BIN(B_Seconds)
    B_Minute  = B2BIN(B_Minute)
    B_Hour    = B2BIN(B_HourRaw)
    B_Date    = B2BIN(B_Date)
    B_Month   = B2BIN(B_Month)
    B_Year    = B2BIN(B_Year)
EndProc

Proc DS3231M_Enable1HzSQW()
    Dim B_Ctrl  As Byte
    Dim B_Stat  As Byte
    Dim B_Hraw  As Byte
    BusIn  Read_From_3231, $0E, [B_Ctrl]
    B_Ctrl = B_Ctrl & %00011000
    B_Ctrl = B_Ctrl + %01000000
    BusOut Write_To_3231, $0E, [B_Ctrl]
    BusIn  Read_From_3231, $0F, [B_Stat]
    B_Stat = B_Stat & %00001000
    BusOut Write_To_3231, $0F, [B_Stat]
    BusIn  Read_From_3231, $02, [B_Hraw]
    B_Hraw = B_Hraw & %10111111
    BusOut Write_To_3231, $02, [B_Hraw]
EndProc

' === Domain-specific editors ==================================================
' Menu Timeout (MM:SS, 30..240 s)
Proc P_MenuTimeout()
    Dim B_Stored As Byte
    Dim L_New    As Long

    Clear b_Long : Clear b_MTimeout
    Cls : Print At 1,1,"MENU TIMEOUT"

    B_Stored = EEPROM_ReadByte(EE_B_Menu_Timeout)
    If B_Stored < 30 Or B_Stored > 240 Then B_Stored = 120
    L_TimeoutRemain = B_Menu_Timeout * 1000

    L_New = P_TEdit(1, B_Stored, 30, 240)

    If b_Long = 1 Then Clear b_Long : Cls : GoTo EXIT_P_MenuTimeout
    If b_MTimeout = 1 Then Clear b_MTimeout : P_P_Timeout() : Cls : GoTo EXIT_P_MenuTimeout

    B_Menu_Timeout = L_New
    EEPROM_WriteByte(EE_B_Menu_Timeout, B_Menu_Timeout)
    L_TimeoutRemain = B_Menu_Timeout * 1000

    P_Exit_OK()
    Cls
EXIT_P_MenuTimeout:
EndProc

' Edit a signed value at a specific line/column, rendering inside [     ].
' Returns the committed value on SHORT press.
' Long-press/timeout are *not* consumed here—caller should check P_UserAborted().
Proc P_SignedAt(B_Ln As Byte, B_Col As Byte, S_Current As SWord, S_Min As SWord, S_Max As SWord, ByRef W_LastPos As Word), SWord
    Dim S_Value   As SWord
    Dim B_Changed As Byte
    Dim W_Mag     As Word

    ' Clamp start
    S_Value = S_Current
    If S_Value < S_Min Then S_Value = S_Min
    If S_Value > S_Max Then S_Value = S_Max

    ' Continue from caller's last encoder position
    W_LastPos = W_EncoderPos
    B_Changed = 1

Draw_Field:
    ' Draw a 5-char numeric field inside brackets at (B_Ln, B_Col-1)
    Print At B_Ln, B_Col-1, "[     ]"
    If S_Value < 0 Then
        W_Mag = 0 - S_Value      ' absolute magnitude
        Print At B_Ln, B_Col,   "-"
        Print At B_Ln, B_Col+1, Dec4 W_Mag
    Else
        W_Mag = S_Value
        Print At B_Ln, B_Col,   Dec5 W_Mag
    EndIf

    While 1 = 1
        ' Let caller detect cancel paths
        If b_Long = 1 Then Result = S_Value : GoTo Exit_P_SignedAt
        If b_MTimeout = 1 Then Result = S_Value : GoTo Exit_P_SignedAt

        ' Encoder up
        If W_EncoderPos > W_LastPos Then
            W_LastPos = W_EncoderPos
            If S_Value < S_Max Then
                Inc S_Value
                P_Beep(1)
                B_Changed = 1
            EndIf
        EndIf

        ' Encoder down
        If W_EncoderPos < W_LastPos Then
            W_LastPos = W_EncoderPos
            If S_Value > S_Min Then
                Dec S_Value
                P_Beep(1)
                B_Changed = 1
            EndIf
        EndIf

        If B_Changed = 1 Then
            B_Changed = 0
            GoTo Draw_Field
        EndIf

        ' Short press = commit and return
        If B_ButtonState = 0 Then
            P_Beep(2)
            While B_ButtonState = 0
                If b_Long = 1 Then Result = S_Value : GoTo Exit_P_SignedAt
                DelayMS 10
            Wend
            Result = S_Value
            GoTo Exit_P_SignedAt
        EndIf

        DelayMS 15
    Wend
Exit_P_SignedAt:
EndProc



















' LCD Contrast (0..10)
Proc P_SetContrast()
    Dim W_LastPos As Word
    Dim B_Val     As Byte

    Cls : Print At 1,1,"Set Contrast"
    Clear b_MTimeout : Clear b_Long
    L_TimeoutRemain = B_Menu_Timeout * 1000

    B_Val = B_Contrast : If B_Val > 10 Then B_Val = 10
    Print At 4,1, Dec2 B_Val
    W_LastPos = W_EncoderPos

    While 1 = 1
        If b_Long = 1 Then Clear b_Long : Cls : Return
        If b_MTimeout = 1 Then Clear b_MTimeout : P_P_Timeout() : Cls : Return

        If W_EncoderPos > W_LastPos Then
            W_LastPos = W_EncoderPos
            If B_Val < 10 Then Inc B_Val : P_Beep(1) : Print At 4,1, Dec2 B_Val
        ElseIf W_EncoderPos < W_LastPos Then
            W_LastPos = W_EncoderPos
            If B_Val > 0 Then Dec B_Val : P_Beep(1) : Print At 4,1, Dec2 B_Val
        EndIf

        If B_ButtonState = 0 Then
            While B_ButtonState = 0
                If b_Long = 1 Then Clear b_Long : Cls : GoTo EXIT_P_SetContrast
                If b_MTimeout = 1 Then Clear b_MTimeout : P_P_Timeout() : Cls : GoTo EXIT_P_SetContrast
                DelayMS 10
            Wend

            P_Beep(2)
            B_Contrast = B_Val
            EEPROM_WriteByte(EE_B_Contrast, B_Contrast)
            Cls
            Return
        EndIf
        DelayMS 15
    Wend
EXIT_P_SetContrast:
EndProc

' Power-fail delay (MM:SS, 1..60 s) stored as Byte
Proc P_SetPwrDelay()
    Dim L_New As Long

    Cls : Print At 1,1,"Pwr Fail Delay"
    Clear b_Long : Clear b_MTimeout
    L_TimeoutRemain = B_Menu_Timeout * 1000

    L_New = P_TEdit(1, B_PwrFailDelay, 1, 60)

    If b_Long = 1 Then Clear b_Long : Cls : Return
    If b_MTimeout = 1 Then Clear b_MTimeout : P_P_Timeout() : Cls : Return

    P_Beep(2)
    B_PwrFailDelay = L_New
    EEPROM_WriteByte(EE_B_PwrFailDelay, B_PwrFailDelay)
    Cls
EndProc

' Relay pulse duration (MM:SS, 1..600 s) stored as WORD
Proc P_SetPulse()
    Dim W_Cur As Word
    Dim L_New As Long

    W_Cur = EEPROM_ReadWord(EE_W_RelayPulseSec)
    If W_Cur < 1 Or W_Cur > 600 Then W_Cur = 5

    Cls : Print At 1,1,"Set Pulse Duration"
    L_New = P_TEdit(1, W_Cur, 1, 600)

    If L_New = 0 Then
        Clear b_Long : Clear b_MTimeout : Cls : GoTo Exit_P_SetPulse
    EndIf

    If L_New <> W_Cur Then EEPROM_WriteWord (EE_W_RelayPulseSec, L_New)
Exit_P_SetPulse:
EndProc
'------------------------------------------------------------------------------------
' Simplified P_Scale:
' - Call: P_Scale(B_Type, B_Inpt)
' - B_Type: 0=PSI (-500..+500), 1=DegC (-50..+150), 2=L/PS (0..500)
' - B_Inpt: 1..3  (selects Input 1/2/3)
' - Loads current values from EEPROM, clamps to type limits.
' - Edits 4mA (flash) ? short press ? 20mA (flash) ? short press ? save+exit.
' - Long press / timeout ? cancel (no save).
'
Proc P_Scale(B_Type As Byte, B_Inpt As Byte)
    Dim S_Min As SWord, S_Max As SWord
    Dim S_Units As String * 6
    Dim EE_Addr4 As Byte, EE_Addr20 As Byte

    Dim S4 As SWord, S20 As SWord
    Dim S4_Orig As SWord, S20_Orig As SWord

    Dim W_LastPos As Word
    Dim B_Ticks As Byte, B_Flash As Byte, B_Changed As Byte

    ' --- Units and bounds by type ---
    Select B_Type
        Case 0
            S_Min = -500 : S_Max = 500 : S_Units = "PSI"
        Case 1
            S_Min =  -50 : S_Max = 150 : S_Units = "DegC"
        Case Else
            S_Min =    0 : S_Max = 500 : S_Units = "L/PS"
    EndSelect

    ' --- EEPROM addresses by input index ---
    Select B_Inpt
        Case 1
            EE_Addr4  = EE_W_Con_2_4ma
            EE_Addr20 = EE_W_Con_2_20ma
        Case 2
            EE_Addr4  = EE_W_Con_3_4ma
            EE_Addr20 = EE_W_Con_3_20ma
        Case Else
            EE_Addr4  = EE_W_Con_4_4ma
            EE_Addr20 = EE_W_Con_4_20ma
    EndSelect

    ' --- Load current values from EEPROM ---
    S4  = EEPROM_ReadSWord(EE_Addr4)
    S20 = EEPROM_ReadSWord(EE_Addr20)

    ' --- Clamp to type limits ---
    If S4  < S_Min Then S4  = S_Min
    If S4  > S_Max Then S4  = S_Max
    If S20 < S_Min Then S20 = S_Min
    If S20 > S_Max Then S20 = S_Max

    S4_Orig  = S4
    S20_Orig = S20

    ' --- Guards & header ---
    Clear b_Long : Clear b_MTimeout : Clear b_ReInitLCD
    L_TimeoutRemain = B_Menu_Timeout * 1000

    Cls
    Print At 1,1,"Input ",Dec B_Inpt," Scale"

    ' Static labels and units
    Print At 3,1," 4mA"
    Print At 4,1,"20mA"
    Print At 3,16,"     " : Print At 3,16,S_Units
    Print At 4,16,"     " : Print At 4,16,S_Units

    ' Initial numeric values (steady)
    Print At 3,9,"      " : Print At 3,9,SDec S4
    Print At 4,9,"      " : Print At 4,9,SDec S20

    ' =========================
    ' Phase 1: Edit 4 mA value
    ' =========================
    W_LastPos = W_EncoderPos
    B_Ticks   = 0
    B_Flash   = 1       ' 1=visible, 0=hidden
    B_Changed = 1       ' force initial paint

Edit_4mA:
    While 1 = 1
        ' Cancel (long press or timeout)
        If b_Long = 1 Or b_MTimeout = 1 Then
            Clear b_Long : b_MTimeout = 0
            S4  = S4_Orig : S20 = S20_Orig
            Cls
            GoTo EXIT_P_Scale
        EndIf

        ' Encoder on 4 mA
        If W_EncoderPos > W_LastPos Then
            W_LastPos = W_EncoderPos
            If S4 < S_Max Then Inc S4 : B_Changed = 1 : P_Beep(1)
        ElseIf W_EncoderPos < W_LastPos Then
            W_LastPos = W_EncoderPos
            If S4 > S_Min Then Dec S4 : B_Changed = 1 : P_Beep(1)
        EndIf

        ' Blink at ~2 Hz (33 * 15ms ˜ 495ms)
        Inc B_Ticks
        If B_Ticks >= SCALE_BLINK_TICKS Then
            B_Ticks = 0
            B_Flash = ~B_Flash
            B_Changed = 1
        EndIf

        ' Update numeric field only when needed
        If B_Changed = 1 Then
            B_Changed = 0
            If B_Flash = 1 Then
                Print At 3,9,"      " : Print At 3,9,SDec S4
            Else
                Print At 3,9,"      "
            EndIf
        EndIf

        ' Short press ? accept 4 mA and move to 20 mA
        If B_ButtonState = 0 Then
            While B_ButtonState = 0
                If b_Long = 1 Or b_MTimeout = 1 Then
                    Clear b_Long : b_MTimeout = 0
                    S4  = S4_Orig : S20 = S20_Orig
                    Cls
                    GoTo EXIT_P_Scale
                EndIf
                DelayMS 10
            Wend
            P_Beep(2)
            ' make 4 mA steady (ensure visible)
            Print At 3,9,"      " : Print At 3,9,SDec S4
            GoTo Edit_20mA
        EndIf

        DelayMS 15
    Wend

    ' =========================
    ' Phase 2: Edit 20 mA value
    ' =========================
Edit_20mA:
    W_LastPos = W_EncoderPos
    B_Ticks   = 0
    B_Flash   = 1
    B_Changed = 1

    While 1 = 1
        ' Cancel (long press or timeout)
        If b_Long = 1 Or b_MTimeout = 1 Then
            Clear b_Long : b_MTimeout = 0
            S4  = S4_Orig : S20 = S20_Orig
            Cls
            GoTo EXIT_P_Scale
        EndIf

        ' Encoder on 20 mA
        If W_EncoderPos > W_LastPos Then
            W_LastPos = W_EncoderPos
            If S20 < S_Max Then Inc S20 : B_Changed = 1 : P_Beep(1)
        ElseIf W_EncoderPos < W_LastPos Then
            W_LastPos = W_EncoderPos
            If S20 > S_Min Then Dec S20 : B_Changed = 1 : P_Beep(1)
        EndIf

        ' Blink at ~2 Hz
        Inc B_Ticks
        If B_Ticks >= SCALE_BLINK_TICKS Then
            B_Ticks = 0
            B_Flash = ~B_Flash
            B_Changed = 1
        EndIf

        ' Update numeric field only when needed
        If B_Changed = 1 Then
            B_Changed = 0
            If B_Flash = 1 Then
                Print At 4,9,"      " : Print At 4,9,SDec S20
            Else
                Print At 4,9,"      "
            EndIf
        EndIf

        ' Short press ? commit to globals + EEPROM
        If B_ButtonState = 0 Then
            While B_ButtonState = 0
                If b_Long = 1 Or b_MTimeout = 1 Then
                    Clear b_Long : b_MTimeout = 0
                    S4  = S4_Orig : S20 = S20_Orig
                    Cls
                    GoTo EXIT_P_Scale
                EndIf
                DelayMS 10
            Wend
            P_Beep(2)

            Select B_Inpt
                Case 1
                    S_In_1_Scale_4  = S4
                    S_In_1_Scale_20 = S20
                    EEPROM_WriteSWord(EE_W_Con_2_4ma,  S4)
                    EEPROM_WriteSWord(EE_W_Con_2_20ma, S20)
                Case 2
                    S_In_2_Scale_4  = S4
                    S_In_2_Scale_20 = S20
                    EEPROM_WriteSWord(EE_W_Con_3_4ma,  S4)
                    EEPROM_WriteSWord(EE_W_Con_3_20ma, S20)
                Case Else
                    S_In_3_Scale_4  = S4
                    S_In_3_Scale_20 = S20
                    EEPROM_WriteSWord(EE_W_Con_4_4ma,  S4)
                    EEPROM_WriteSWord(EE_W_Con_4_20ma, S20)
            EndSelect

            Cls
            Return
        EndIf

        DelayMS 15
    Wend

EXIT_P_Scale:
EndProc



'---------------------------------------------------
' Print a signed value as [sign + 4 digits with leading zeros]
' Example: " 0036", "-0016"
Proc P_PrintS4Z(B_Ln As Byte, B_Col As Byte, S_Val As SWord)
    Dim W_Abs As Word
    If S_Val < 0 Then
        W_Abs = -S_Val
        Print At B_Ln, B_Col, "-"
    Else
        W_Abs = S_Val
        Print At B_Ln, B_Col, " "
    EndIf
    Print At B_Ln, B_Col + 1, Dec4 W_Abs
EndProc

' Map type -> units text used on the screen
' 0=PSI, 1=Deg C, 2=L/PS, else "Units"
Proc P_GetUnits(B_Type As Byte), String * 5
    Select B_Type
        Case 0
            Result = " PSI"
        Case 1
            Result = " DegC"
        Case 2
            Result = " L/PS"
        Case Else
            Result = "Units"
    End Select
EndProc
'---------------------------------------------------
' Simplified output configuration editor.
' Call:  P_Output(S_Title, B_Type, B_Inpt)
'   S_Title : shown on line 1
'   B_Type  : 0=Pressure, 1=Temperature, 2=Flow  (selects which fields are edited)
'   B_Inpt  : 1..3 selects which input’s config word to edit
'
' Edits (by type):
'  Pressure (0):  ME (bit2), High (7..8), Primary Low (9..10), Secondary Low (11..12), Display (13..14)
'  Temperature(1): ME (bit2), High (7..8)
'  Flow (2):      ME (bit2), High (7..8), Primary Low (9..10), Secondary Low (11..12), Display (13..14)
'
' EEPROM words: EE_W_Con_2_Cnfg / _3_ / _4_
' Globals:      W_Con_2_Cnfg     / _3_ / _4_
' 2-bit fields encoding: 00=NoAct, 01=Pulse, 10=Latch  (values 0..2)
'
Proc P_Output(S_Title As String * 18, B_Type As Byte, B_Inpt As Byte)
    ' --- locals ---
    Dim EE_Addr As Byte
    Dim W_Cfg As Word, W_Orig As Word
    Dim W_LastPos As Word
    Dim B_Val As Byte          ' working selection for current field (0/1 or 0..2)
    Dim B_Step As Byte         ' current step index
    Dim B_LastVal As Byte      ' to avoid unnecessary redraws
    Dim S_Line As String * 18  ' (unused temp; kept to preserve your structure)

    ' --- select which input’s config word ---
    Select B_Inpt
        Case 1
            EE_Addr = EE_W_In_1_Cnfg
            W_Cfg   = W_In_1_Cnfg
        Case 2
            EE_Addr = EE_W_In_2_Cnfg
            W_Cfg   = W_In_2_Cnfg
        Case Else
            EE_Addr = EE_W_In_3_Cnfg
            W_Cfg   = W_In_3_Cnfg
    EndSelect
    ' authoritative read from EEPROM
    W_Cfg  = EEPROM_ReadWord(EE_Addr)
    W_Orig = W_Cfg

    ' >>> Jump over helper labels so we don't execute them <<<
    GoTo CodeStart

    ' ===== helper subs (label gosubs) =======================================
Label_GetBit:
    ' uses: B_Val=bit# input; returns B_Val=0/1
    Dim W_M As Word, B_I As Byte
    W_M = 1
    For B_I = 1 To B_Val
        W_M = W_M * 2
    Next
    If W_Cfg & W_M <> 0 Then
        B_Val = 1
    Else
        B_Val = 0
    EndIf
    Return

Label_SetBit:
    ' uses: B_Val=bit#, B_LastVal=new bit value (0/1)
    Dim W_M2 As Word, B_J As Byte
    W_M2 = 1
    For B_J = 1 To B_Val
        W_M2 = W_M2 * 2
    Next
    If B_LastVal = 0 Then
        If W_Cfg & W_M2 <> 0 Then W_Cfg = W_Cfg - W_M2
    Else
        If W_Cfg & W_M2 = 0 Then W_Cfg = W_Cfg + W_M2
    EndIf
    Return

GetPair:
    ' uses: B_Val=startBit (LSB of the pair). Returns B_Val in 0..3
    Dim W_M3 As Word, B_K As Byte, B_Lo As Byte, B_Hi As Byte
    W_M3 = 1
    For B_K = 1 To B_Val
        W_M3 = W_M3 * 2
    Next
    If W_Cfg & W_M3 <> 0 Then
        B_Lo = 1
    Else
        B_Lo = 0
    EndIf
    W_M3 = W_M3 * 2
    If W_Cfg & W_M3 <> 0 Then
        B_Hi = 1
    Else
        B_Hi = 0
    EndIf
    B_Val = B_Lo + (B_Hi * 2)
    Return

SetPair:
    ' uses: B_Val=startBit, B_LastVal=new value (0..3)
    Dim W_M4 As Word, B_T As Byte
    Dim B_LoN As Byte, B_HiN As Byte
    W_M4 = 1
    For B_T = 1 To B_Val
        W_M4 = W_M4 * 2
    Next
    ' clear existing two bits
    If W_Cfg & W_M4 <> 0 Then W_Cfg = W_Cfg - W_M4
    If W_Cfg & (W_M4 * 2) <> 0 Then W_Cfg = W_Cfg - (W_M4 * 2)
    ' add new (no shifts; Positron-safe)
    B_LoN = 0 : If B_LastVal & 1 <> 0 Then B_LoN = 1
    B_HiN = 0 : If B_LastVal & 2 <> 0 Then B_HiN = 1
    If B_LoN = 1 Then W_Cfg = W_Cfg + W_M4
    If B_HiN = 1 Then W_Cfg = W_Cfg + (W_M4 * 2)
    Return

DrawTwoChoice_ME:
    ' Line 2 already has the field title.
    ' Line 3 blank to avoid ghosting.
    Print At 3,1,"                    "
    ' Line 4 shows choices with the current selection bracketed.
    Print At 4,1,"                    "
    If B_Val = 0 Then
        Print At 4,1,"[Disabled] Enabled"
    Else
        Print At 4,2,"Disabled [Enabled]"
    EndIf
    Return

DrawTriAction:
    ' For 2-bit action fields: NoAct / Pulse / Latch
    Print At 3,1,"                    "
    Print At 4,1,"                    "
    Select B_Val
        Case 0
            Print At 4,1,"[No]  Pulse   Latch"
        Case 1
            Print At 4,1," No  [Pulse]  Latch"
        Case Else
            Print At 4,1," No   Pulse  [Latch]"
    EndSelect
    Return

DrawTriDisp:
    ' For Display field: NoDisp / Always / OnEn
    Print At 3,1,"                    "
    Print At 4,1,"                    "
    Select B_Val
        Case 0
            Print At 4,1,"[No]  Yes  When En"
        Case 1
            Print At 4,1," No  [Yes] When En"
        Case Else
            Print At 4,1," No   Yes [When En]"
    EndSelect
    Return

DrawFieldTitle:
    ' Print line 2 title based on B_Type and B_Step
    Print At 2,1,"                    "
    If B_Type = 0 Then             ' Pressure
        Select B_Step
            Case 0
                Print At 2,1,"Master Enable"
            Case 1
                Print At 2,1,"High Pressure"
            Case 2
                Print At 2,1,"Primary LP"
            Case 3
                Print At 2,1,"Secondary LP"
            Case 4
                Print At 2,1,"Display"
        EndSelect
    ElseIf B_Type = 1 Then         ' Temperature
        Select B_Step
            Case 0
                Print At 2,1,"Master Enable"
            Case 1
                Print At 2,1,"High Temp"
        EndSelect
    Else                           ' Flow
        Select B_Step
            Case 0
                Print At 2,1,"Master Enable"
            Case 1
                Print At 2,1,"High Flow"
            Case 2
                Print At 2,1,"Primary Low Flow"
            Case 3
                Print At 2,1,"Secondary Low Flow"
            Case 4
                Print At 2,1,"Display"
        EndSelect
    EndIf
    Return
    ' ===== end helpers ======================================================

CodeStart:
    ' UI guards & header (after skipping helpers)
    Clear b_Long
    Clear b_MTimeout
    Clear b_ReInitLCD
    L_TimeoutRemain = B_Menu_Timeout * 1000

    Cls
    Print At 1,1, S_Title
    P_Debounce()                       ' swallow the launching short-press
    Clear b_Long                       ' re-arm after debounce
    Clear b_MTimeout
    L_TimeoutRemain = B_Menu_Timeout * 1000

    ' determine number of steps for this type (for reference)
    Dim B_Total As Byte
    If B_Type = 1 Then
        B_Total = 2          ' Temp: ME + High
    Else
        B_Total = 5          ' Pressure/Flow
    EndIf

    ' ---- Step 0: Master Enable (bit 2) ----
    B_Step = 0
Step_ME:
    GoSub DrawFieldTitle
    ' load current bit 2
    B_Val = 2 : GoSub Label_GetBit           ' returns B_Val = 0/1
    B_LastVal = 255                           ' force first draw
    GoSub DrawTwoChoice_ME
    W_LastPos = W_EncoderPos
ME_Loop:
    ' cancel?
    If b_Long = 1 Or b_MTimeout = 1 Then
        Clear b_Long : b_MTimeout = 0
        W_Cfg = W_Orig
        Cls
        Return
    EndIf
    ' encoder
    If W_EncoderPos > W_LastPos Then
        W_LastPos = W_EncoderPos
        If B_Val < 1 Then
            Inc B_Val : P_Beep(1)
        EndIf
    ElseIf W_EncoderPos < W_LastPos Then
        W_LastPos = W_EncoderPos
        If B_Val > 0 Then
            Dec B_Val : P_Beep(1)
        EndIf
    EndIf
    ' redraw if changed
    If B_Val <> B_LastVal Then
        B_LastVal = B_Val
        GoSub DrawTwoChoice_ME
    EndIf
    ' short press -> accept and next
    If B_ButtonState = 0 Then
        While B_ButtonState = 0
            If b_Long = 1 Or b_MTimeout = 1 Then
                Clear b_Long : b_MTimeout = 0
                W_Cfg = W_Orig : Cls : Return
            EndIf
            DelayMS 10
        Wend
        P_Beep(2)
        ' write bit 2
        B_LastVal = B_Val
        B_Val = 2 : GoSub Label_SetBit
        ' next step selection by type
        If B_Type = 1 Then
            B_Step = 1 : GoSub DrawFieldTitle
            GoTo Step_HighOnly
        Else
            GoTo Step_High
        EndIf
    EndIf
    DelayMS 15
    GoTo ME_Loop

    ' ---- Step 1: High action (bits 7..8) ----
Step_High:
    B_Step = 1 : GoSub DrawFieldTitle
Step_HighOnly:
    B_Val = 7 : GoSub GetPair
    If B_Val > 2 Then B_Val = 2
    B_LastVal = 255
    GoSub DrawTriAction
    W_LastPos = W_EncoderPos
HIGH_Loop:
    If b_Long = 1 Or b_MTimeout = 1 Then
        Clear b_Long : b_MTimeout = 0
        W_Cfg = W_Orig : Cls : Return
    EndIf
    If W_EncoderPos > W_LastPos Then
        W_LastPos = W_EncoderPos
        If B_Val < 2 Then Inc B_Val : P_Beep(1)
    ElseIf W_EncoderPos < W_LastPos Then
        W_LastPos = W_EncoderPos
        If B_Val > 0 Then Dec B_Val : P_Beep(1)
    EndIf
    If B_Val <> B_LastVal Then
        B_LastVal = B_Val
        GoSub DrawTriAction
    EndIf
    If B_ButtonState = 0 Then
        While B_ButtonState = 0
            If b_Long = 1 Or b_MTimeout = 1 Then
                Clear b_Long : b_MTimeout = 0
                W_Cfg = W_Orig : Cls : Return
            EndIf
            DelayMS 10
        Wend
        P_Beep(2)
        B_LastVal = B_Val : B_Val = 7 : GoSub SetPair
        If B_Type = 1 Then GoTo Commit_Save
        GoTo Step_PL
    EndIf
    DelayMS 15
    GoTo HIGH_Loop

    ' ---- Step 2: Primary Low action (bits 9..10) ----
Step_PL:
    B_Step = 2 : GoSub DrawFieldTitle
    B_Val = 9 : GoSub GetPair
    If B_Val > 2 Then B_Val = 2
    B_LastVal = 255 : GoSub DrawTriAction
    W_LastPos = W_EncoderPos
PL_Loop:
    If b_Long = 1 Or b_MTimeout = 1 Then
        Clear b_Long : b_MTimeout = 0
        W_Cfg = W_Orig : Cls : Return
    EndIf
    If W_EncoderPos > W_LastPos Then
        W_LastPos = W_EncoderPos
        If B_Val < 2 Then Inc B_Val : P_Beep(1)
    ElseIf W_EncoderPos < W_LastPos Then
        W_LastPos = W_EncoderPos
        If B_Val > 0 Then Dec B_Val : P_Beep(1)
    EndIf
    If B_Val <> B_LastVal Then
        B_LastVal = B_Val : GoSub DrawTriAction
    EndIf
    If B_ButtonState = 0 Then
        While B_ButtonState = 0
            If b_Long = 1 Or b_MTimeout = 1 Then
                Clear b_Long : b_MTimeout = 0
                W_Cfg = W_Orig : Cls : Return
            EndIf
            DelayMS 10
        Wend
        P_Beep(2)
        B_LastVal = B_Val : B_Val = 9 : GoSub SetPair
        GoTo Step_SL
    EndIf
    DelayMS 15
    GoTo PL_Loop

    ' ---- Step 3: Secondary Low action (bits 11..12) ----
Step_SL:
    B_Step = 3 : GoSub DrawFieldTitle
    B_Val = 11 : GoSub GetPair
    If B_Val > 2 Then B_Val = 2
    B_LastVal = 255 : GoSub DrawTriAction
    W_LastPos = W_EncoderPos
SL_Loop:
    If b_Long = 1 Or b_MTimeout = 1 Then
        Clear b_Long : b_MTimeout = 0
        W_Cfg = W_Orig : Cls : Return
    EndIf
    If W_EncoderPos > W_LastPos Then
        W_LastPos = W_EncoderPos
        If B_Val < 2 Then Inc B_Val : P_Beep(1)
    ElseIf W_EncoderPos < W_LastPos Then
        W_LastPos = W_EncoderPos
        If B_Val > 0 Then Dec B_Val : P_Beep(1)
    EndIf
    If B_Val <> B_LastVal Then
        B_LastVal = B_Val : GoSub DrawTriAction
    EndIf
    If B_ButtonState = 0 Then
        While B_ButtonState = 0
            If b_Long = 1 Or b_MTimeout = 1 Then
                Clear b_Long : b_MTimeout = 0
                W_Cfg = W_Orig : Cls : Return
            EndIf
            DelayMS 10
        Wend
        P_Beep(2)
        B_LastVal = B_Val : B_Val = 11 : GoSub SetPair
        GoTo Step_Disp
    EndIf
    DelayMS 15
    GoTo SL_Loop

    ' ---- Step 4: Display (bits 13..14) ----
Step_Disp:
    B_Step = 4 : GoSub DrawFieldTitle
    B_Val = 13 : GoSub GetPair
    If B_Val > 2 Then B_Val = 2
    B_LastVal = 255
    GoSub DrawTriDisp
    W_LastPos = W_EncoderPos
DISP_Loop:
    If b_Long = 1 Or b_MTimeout = 1 Then
        Clear b_Long : b_MTimeout = 0
        W_Cfg = W_Orig : Cls : Return
    EndIf
    If W_EncoderPos > W_LastPos Then
        W_LastPos = W_EncoderPos
        If B_Val < 2 Then Inc B_Val : P_Beep(1)
    ElseIf W_EncoderPos < W_LastPos Then
        W_LastPos = W_EncoderPos
        If B_Val > 0 Then Dec B_Val : P_Beep(1)
    EndIf
    If B_Val <> B_LastVal Then
        B_LastVal = B_Val
        GoSub DrawTriDisp
    EndIf
    If B_ButtonState = 0 Then
        While B_ButtonState = 0
            If b_Long = 1 Or b_MTimeout = 1 Then
                Clear b_Long : b_MTimeout = 0
                W_Cfg = W_Orig : Cls : Return
            EndIf
            DelayMS 10
        Wend
        P_Beep(2)
        B_LastVal = B_Val : B_Val = 13 : GoSub SetPair
        GoTo Commit_Save
    EndIf
    DelayMS 15
    GoTo DISP_Loop

Commit_Save:
    ' commit to globals and EEPROM
    Select B_Inpt
        Case 1
            W_In_1_Cnfg = W_Cfg
        Case 2
            W_In_2_Cnfg = W_Cfg
        Case Else
            W_In_3_Cnfg = W_Cfg
    EndSelect
    EEPROM_WriteWord(EE_Addr, W_Cfg)

    Cls
    Return
EndProc



'---------------------------------------------------------------------------
' Shared helper: returns 1 if user cancelled (long-press or timeout).
' - Clears b_Long / b_MTimeout
' - Sets b_Escape
' - Beeps (long) if buzzer idle
Proc P_UserAborted(), Bit
    If b_Long = 1 Then
        Clear b_Long
        b_Escape = 1
        If B_BeepLen = 0 Then P_Beep(5)
        Result = 1
        GoTo Exit_P_UserAborted
    EndIf

    If b_MTimeout = 1 Then
        b_MTimeout = 0
        b_Escape = 1
        If B_BeepLen = 0 Then P_Beep(5)
        Result = 1
        GoTo Exit_P_UserAborted
    EndIf

    Result = 0
Exit_P_UserAborted:
EndProc


