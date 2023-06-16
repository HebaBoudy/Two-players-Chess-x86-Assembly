printtime MACRO
    PUSHA
mov ah, 2ch   ;get time
int 21h       ;

mov CURRENTHOUR, ch  
mov CURRENTMINUTE, cl
mov CURRENTSECOND,dh

mov ah, 0     ;
mov al, CURRENTHOUR  ;divide by 10
mov bl, 10    ;
div bl        ;

mov hour1, al 
mov hour2, ah 

mov ah, 2     ;
mov dl, hour1 ;  
add dl, 30h   ;
int 21h       ;
              ;print hour
mov ah, 2     ;
mov dl, hour2 ;
add dl, 30h   ;
int 21h       ;

mov ah, 2     
mov dl, ':'   
int 21h       

mov ah, 0     ;
mov al, CURRENTMINUTE  ;divide by 10
mov bl, 10    ;
div bl        ;

mov min1, al  
mov min2, ah  

mov ah, 2     ;
mov dl, min1  ; 
add dl, 30h   ;
int 21h       ;
              ;print minuntes
mov ah, 2     ;
mov dl, min2  ;
add dl, 30h   ;
int 21h       ;

mov ah, 2     
mov dl, ':'   
int 21h       

mov ah, 0     ;
mov al, CURRENTSECOND  ;divide by 10
mov bl, 10    ;
div bl        ;

mov sec1, al  
mov sec2, ah  

mov ah, 2     ;
mov dl, sec1  ; 
add dl, 30h   ;
int 21h       ;
              ;print seconds
mov ah, 2     ;
mov dl, sec2  ;
add dl, 30h   ;
int 21h       ;
POPA
endM printtime

getTime macro
PUSHA

;Get System Time
mov ah,2ch
int 21h ; return seconds in dh
MOV DL,0
XCHG DH,DL
mov CURRENTTIME, dX
cmp DX,57 
jG GREATER_THAN
JMP SKIP_LESS_THAN
GREATER_THAN:
SUB CURRENTTIME,57
SKIP_LESS_THAN:
POPA

endm getTime

p2_getTime macro
PUSHA

;Get System Time
mov ah,2ch
int 21h ; return seconds in dh
MOV DL,0
XCHG DH,DL
mov p2_CURRENTTIME, dX

POPA

endm p2_getTime

EnterTextMode MACRO  

PUSH AX
                 MOV AH,0
                 MOV AL,03
                 INT  10H
                 POP  AX


endm EnterTextMode


WelcomeWindow macro

EnterTextMode
changeMainAndTextScreenColors 7EH
MovCursorToLocation 3,1
DisplayString MSG_WELCOME
; MovCursorToLocation 3,5
; DisplayString MSG_WELCOME

entername1:
MovCursorToLocation 3,3
ReadString player1name
MovCursorToLocation 3,10
displaystring MSG_CLEAR
cmp player1name+2[1],41H
jb printerror
cmp player1name+2[1],7AH
ja printerror
jmp endname

printerror:
MovCursorToLocation 3,10
displaystring NameErrorMessage
jmp entername1


endname:
MovCursorToLocation 3,15
DisplayString MSG_CONT





ENDM WelcomeWindow
UpdateCursorLocationDown Macro x
PUSHA
 mov ah,3h
 mov bh,0 
 int 10h
 
 inc dh
 MovCursorToLocation X,dh
 POPA
 endm UpdateCursorLocationDown

UPDATECURSORLOCATIONRIGHT MACRO update
  PUSHA
  mov ah,3h
  mov bh,0 
  int 10h

ADD DL,update

MovCursorToLocation DL,DH
POPA
ENDM UPDATECURSORLOCATIONRIGHT

ToMainScreenWindow macro

                      EnterTextMode
                      changeMainAndTextScreenColors 7EH        ; 7 FOR GREY BACKGROUND AND E FOR YELLOW COLOR
    
                    
                      MovCursorToLocation           25,8
                      DisplayString                 MSG_CHAT
                      MovCursorToLocation           25,10
                      DisplayString                 MSG_GAME
                      MovCursorToLocation           25,12
                      DisplayString                 MSG_END
                      
                       MovCursorToLocation           0,15
                      displaystring chatline
                      MovCursorToLocation 0,16
 
ENDM ToMainScreenWindow
ShowNotificationFORP1 Macro
 
 
 ;YELLOW PIECE IS KILLED
 CMP KILLEDPIECE,17
 JNZ NOT_PAWNY
 MovCursorToLocation 32,YNOTIFICATIONYELLOW
 INC YNOTIFICATIONYELLOW
 displaystring  PAWN
 NOT_PAWNY:
 CMP KILLEDPIECE,25
 JNZ NOT_ROOKY
 MovCursorToLocation 32,YNOTIFICATIONYELLOW
 INC YNOTIFICATIONYELLOW
 DisplayString Rook
 NOT_ROOKY:
 CMP KILLEDPIECE,26
 JNZ NOT_KNIGHTY
 MovCursorToLocation 32,YNOTIFICATIONYELLOW
 INC YNOTIFICATIONYELLOW
 DisplayString Knight
 NOT_KNIGHTY:
 CMP KILLEDPIECE,27
 JNZ NOT_BISHOPY
 MovCursorToLocation 32,YNOTIFICATIONYELLOW
 INC YNOTIFICATIONYELLOW
 DisplayString Bishop
 NOT_BISHOPY:
 CMP KILLEDPIECE,28
 JNZ NOT_QUEENY
 MovCursorToLocation 32,YNOTIFICATIONYELLOW
 INC YNOTIFICATIONYELLOW
 DisplayString Queen
 NOT_QUEENY:
 CMP KILLEDPIECE,29
 JNZ NOT_KINGY
 MovCursorToLocation 32,YNOTIFICATIONYELLOW
 INC YNOTIFICATIONYELLOW
 DisplayString King
 MovCursorToLocation 23,254
 DisplayString YELLOWKINGDEAD
 WaitKeyPress
 JMP WELCOMESCREEN
 ; JMP ENDGAME
 NOT_KINGY:
 MovCursorToLocation 0,18
 DisplayString MSG_CLEAR_TIMER
 CALL InitPiecesPROC
 
 ;EnterGraphicsMode
 Endm ShowNotificationFORP1
ClearNotificationBar MACRO
 
 push ax
 push cx
 push dx
 
 mov cx,320
 cleartext:
 mov dl,' '
 mov ah,2
 int 21h
 dec cx
 jnz cleartext
 
 pop dx
 pop cx
 pop ax
 

  ENDM ClearNotificationBar
ShowNotificationFORP2 Macro
 
 CMP KILLEDPIECE,1
 JNZ NOT_PAWN
 
 MovCursorToLocation 0,YNOTIFICATIONBLACK
 INC YNOTIFICATIONBLACK
 displaystring  PAWN
 NOT_PAWN:
 CMP KILLEDPIECE,9
 JNZ NOT_ROOK
 MovCursorToLocation 0,YNOTIFICATIONBLACK
 INC YNOTIFICATIONBLACK
 DisplayString Rook
 NOT_ROOK:
 CMP KILLEDPIECE,10
 JNZ NOT_KNIGHT
 MovCursorToLocation 0,YNOTIFICATIONBLACK
 INC YNOTIFICATIONBLACK
 DisplayString Knight
 NOT_KNIGHT:
 CMP KILLEDPIECE,11
 JNZ NOT_BISHOP
 MovCursorToLocation 0,YNOTIFICATIONBLACK
 INC YNOTIFICATIONBLACK
 DisplayString Bishop
 NOT_BISHOP:
 CMP KILLEDPIECE,12
 JNZ NOT_QUEEN
 MovCursorToLocation 0,YNOTIFICATIONBLACK
 INC YNOTIFICATIONBLACK
 DisplayString Queen
 NOT_QUEEN:
 CMP KILLEDPIECE,13
 JNZ NOT_KING
 MovCursorToLocation 0,YNOTIFICATIONBLACK
 INC YNOTIFICATIONBLACK
 DisplayString King
 MovCursorToLocation 23,254
 DisplayString BLACKKINGDEAD
 WaitKeyPress
 JMP WELCOMESCREEN
 ;JMP ENDGAME
 NOT_KING:
 
 MovCursorToLocation 32,18
 displaystring MSG_CLEAR_TIMER 
 
 CALL InitPiecesPROC
 
 ;EnterGraphicsMode
 Endm ShowNotificationFORP2


P2_DrawSquare MACRO  P2_color
              LOCAL P2_square , P2_back
              PUSH  AX
              PUSH  BX
              PUSH  CX
              PUSH  DX
   ;ATTEMPT DO DRAW SQUARE AT LOCATION
              mov   cx,P2_GLOBAL_COLUMN       ;Column

              mov   dx,P2_GLOBAL_ROW       ;Row

              mov   al,BYTE PTR P2_color        ;Pixel color
              mov   ah,0ch          ;Draw Pixel Command
              mov   bh,22
   P2_square:    

   ;***************
              mov   cx,P2_GLOBAL_COLUMN      ;Column
              mov   bl,22
   P2_back:      
              int   10h
              inc   cx              ;inc column
              dec   bl
              jnz   P2_back
              inc   dx
              mov   cx,0
              dec   bh
              jnz   P2_square

 ;**********************************************
              POP   DX
              POP   CX
              POP   BX
              POP   AX
endm P2_DrawSquare


;include myMacros.asmBORDE
Push_Cell_Into_Highlighted_Array MACRO 


INC Number_Of_Stored_Highlighted_Cells   ;EACH TIME I ADD 2 TO THE SIZE OF THE ARRAY BECAUSE EACH CELL PUSH ITS X AND Y ELEMENTS 
;ANY CELL THAT IS HIGHLIGHTED IS PUSHED TO THE ARRAY OF HIGHLIGHTED CELLS X1 , Y1 , X2 , Y2,......
 MemoryToMemoryMovWord [si],X
 add SI,2 
 MemoryToMemoryMovWord [si],y
 add SI,2 
  
ENDM Push_Cell_Into_Highlighted_Array

P2_Push_Cell_Into_Highlighted_Array MACRO 

INC P2_Number_Of_Stored_Highlighted_Cells   ;EACH TIME I ADD 2 TO THE SIZE OF THE ARRAY BECAUSE EACH CELL PUSH ITS X AND Y ELEMENTS 
;ANY CELL THAT IS HIGHLIGHTED IS PUSHED TO THE ARRAY OF HIGHLIGHTED CELLS X1 , Y1 , X2 , Y2,......
 MemoryToMemoryMovWord [si],P2_X
 add SI,2 
 MemoryToMemoryMovWord [si],P2_y
 add SI,2 
  
ENDM P2_Push_Cell_Into_Highlighted_Array

;display string
 DisplayNumberAX MACRO 
   LOCAL lp1,lbl1,lbl2
	  push ax
	  mov cl,4  ;shifting 8 to get the number wanted  
	  mov dh,4  ; loop 2 times
	  
	  
 lp1:	  
          pop ax ; to get the value of ax that we will use every iteration 
          push ax; save value of ax for next shifting 
	      and ax,0ff00h;to make the al = 0 
	      xchg al,ah; exchange otherwise div didnt work
	      mov bh,10h;  put 10h for division (10h) to be able to seperate the 2 digits apart (most significant in al),(less significant in ah)
	      div bh
	      CMP Al,0Ah; if number entered was (A-F)
	      JAE lbl1;(skip adding zero for A-F)
	      add al,'0' ; conv num---> char
	      jmp lbl2 ;(skip adding 37h for numbers)
lbl1: 	      add al,37h ;(convert A-F into strings) 
lbl2:	      mov dl,al  ; char to be printed(more significant)
	      mov ah,2 ; for interupt
	      int 21h 
	      pop ax ;(get the last value in ax that was shifted)
	      shl ax,cl ;(shift left 4 to get the number everytime)
	      push ax;(store value for shifting)
	      dec dh; loop counter decrement
          jnz lp1	      	
EndM DisplayNumberAX


displaystring macro BufferData
                 mov  ah,9
                 mov  dx,offset BufferData
                 int  21h
                 endm displaystring

   ; readstring

readstring macro InputString

                    PUSH AX
                    PUSH DX
                    mov  ah,0Ah
                    mov  dx,offset InputString
                    int  21h
                    POP  DX
                    POP  AX
                                  endm readstring


      ;display number

DisplayNumber MACRO num                                ;this variable to put the number in to save it if i changes AX later , msh mohem awi bas ehtyaty
                 MOV  CX, 0
                 MOV  BL, 10

   ;AX/BL --> AH:REMAINDER(WANTED DIGIT) AL:QUOTIENT
   LOOP_1_START: 
   ;CX stores the number of digits you have popped to the stack.
                 INC  CX

   ;Divide AL by 10
                 DIV  BL

   ;Store the remainder (wanted digit) in DX
                 MOV  DL, AH
                 MOV  DH, 0

   ;Check if the quotient is equal to zero (tou have finished)
                 CMP  AL, 0

   ;Now AX has the quotient stored in AL
                 MOV  AH, 0

   ;Push the remainder
                 PUSH DX
                 JNZ  LOOP_1_START


   LOOP_2_START: 
                 POP  DX
                 MOV  AH, 2
                 ADD  DL, '0'
                 INT  21H
                 DEC  CX
                 JNZ  LOOP_2_START
                 mov  ax,Num
ENDM
MovCursorToLocation MACRO x,y

                     mov bh,0
                       mov ah,2
                       mov dl, x
                       MOV dh,y
                       int 10h
endm MovCursorToLocation

changeMainAndTextScreenColors MACRO  color

                                 PUSH DX
                                 PUSH AX
                                 PUSH BX
                                 MOV  AH, 06h     ; Scroll up function
                                 XOR  AL, AL      ; Clear entire screen
                                 XOR  CX, CX      ; Upper left corner CH=row, CL=column
                                 MOV  DX, 184FH   ; lower right corner DH=row, DL=column
                                 MOV  BH, color   ;
                                 INT  10H

                                 POP  BX
                                 POP  AX
                                 POP  DX
ENDM changeMainAndTextScreenColors





DrawSquare MACRO  color
              LOCAL square , back
              PUSH  AX
              PUSH  BX
              PUSH  CX
              PUSH  DX
   ;ATTEMPT DO DRAW SQUARE AT LOCATION
              mov   cx,GLOBAL_COLUMN       ;Column

              mov   dx,GLOBAL_ROW       ;Row

              mov   al,BYTE PTR color        ;Pixel color
              mov   ah,0ch          ;Draw Pixel Command
              mov   bh,22
   square:    

   ;***************
              mov   cx,GLOBAL_COLUMN      ;Column
              mov   bl,22
   back:      
              int   10h
              inc   cx              ;inc column
              dec   bl
              jnz   back
              inc   dx
              mov   cx,0
              dec   bh
              jnz   square

   ;**************
              POP   DX
              POP   CX
              POP   BX
              POP   AX
endm DrawSquare

EnterGraphicsMode MACRO
                     MOV AL,13h
                     MOV AH,0
                     INT 10H
ENDM EnterGraphicsMode
;********************************
exchange MACRO X,Y
            PUSH AX
            MOV  AL,X
            MOV  AH,Y
            MOV  X,AH
            MOV  Y,AL
            POP  AX
ENDM exchange

 ;*******************
; DrawBoard MACRO color1,color2

;                        EnterGraphicsMode

;    DrawFullGrid:       
;    DrawOneRowOfSquares:

;                        DrawSquare        column,row,color1
;                        ADD               column,22
;                        CMP               column,248
;                        exchange          color1,color2         ;MEMORY TO MEMORY EXCHANGE

;                        JNZ               DrawOneRowOfSquares
;                        ADD               row,22
;                        MOV               column,72
;                        exchange          color1,color2
;                        CMP               row,180
;                        jnz               DrawFullGrid

; ENDM DrawBoard
;************************************

Draw_Square_Macro_X_Y_COLOR MACRO  xstart1,ystart1,c
              LOCAL square1 , back1
              PUSH  AX
              PUSH  BX
              PUSH  CX
              PUSH  DX
   ;ATTEMPT DO DRAW SQUARE AT LOCATION
              mov   cx,WORD PTR xstart1       ;Column

              mov   dx,WORD PTR ystart1       ;Row

              mov   al,BYTE PTR c        ;Pixel color
              mov   ah,0ch          ;Draw Pixel Command
              mov   bh,22
   square1:    

   ;***************
              mov   cx,WORD PTR xstart1      ;Column
              mov   bl,22
   back1:      
              int   10h
              inc   cx              ;inc column
              dec   bl
              jnz   back1
              inc   dx
              mov   cx,0
              dec   bh
              jnz   square1

   ;**************
              POP   DX
              POP   CX
              POP   BX
              POP   AX
endm Draw_Square_Macro_X_Y_COLOR



OpenFile MACRO  Filename

   ; Open file

            MOV AH, 3Dh
            MOV AL, 0              ; read only
            LEA DX, [Filename]
            INT 21h
    
   ; you should check carry flag to make sure it worked correctly
   ; carry = 0 -> successful , file handle -> AX
   ; carry = 1 -> failed , AX -> error code
     
            MOV [Filehandle], AX
    
   ;RET

ENDM OpenFile


ReadData MACRO
    
            push ax
            push BX
            push cx
            push dx
            MOV  AH,3Fh
            MOV  BX, [Filehandle]
            MOV  CX, Widthh*Height   ; number of bytes to read
            LEA  DX, PData
            INT  21h
            pop  DX
            pop  CX
            pop  BX
            pop  AX
ENDM ReadData


CloseFile MACRO
   
             push ax
             push BX
   
             MOV  AH, 3Eh
             MOV  BX, [Filehandle]
             INT  21h
             pop  BX
             pop  AX
ENDM CloseFile






;************************KEYBOARD MACROS*************************
;Wait key press: AH=scan_code, AL=ASCII_code
WaitKeyPress MACRO
    MOV AH, 00H
    INT 16H
ENDM WaitKeyPress

;Get key press: AH=scan_code, AL=ASCII_code
GetKeyPress MACRO
    MOV AH, 01H
    INT 16H
ENDM GetKeyPress

;Get key press and flush it: AH=scan_code, AL=ASCII_code
GetKeyPressAndFlush MACRO
    LOCAL KeyNotPressed
    GetKeyPress
    JZ KeyNotPressed
    WaitKeyPress
    KeyNotPressed:
ENDM GetKeyPressAndFlush

MemoryToMemoryMovByte MACRO destination,source
push bx
mov bh,source 
mov destination,bh
pop bx
ENDM MemoryToMemoryMovByte

MemoryToMemoryMovWord MACRO destination,source
push bx
mov bx,source 
mov destination,bx
pop bx
ENDM MemoryToMemoryMovWord

;Send character through serial port
SendChar MACRO MyChar
    LOCAL Send
    Send:
    MOV DX, 3FDH    ;Line Status Register
    IN AL, DX
    AND AL, 00100000B       ;Check transmitter holding register status: 1 ready, 0 otherwise
    JZ Send                 ;Transmitter is not ready
    MOV DX, 3F8H
    MOV AL, MyChar
    OUT DX, AL
ENDM SendChar

ReceiveChar MACRO
    LOCAL Return
    MOV AL, 0
    MOV DX, 3FDH    ;Line Status Register
    IN AL, DX
    AND AL, 00000001B       ;Check for data ready
    JZ Return               ;No character received
    MOV DX, 03F8H      ;Receive data register
    IN AL, DX
    Return:
ENDM ReadPortChar

EmptyKeyQueue MACRO
    LOCAL Back, Return
    Back:
    GetKeyPress
    JZ Return
    WaitKeyPress
    JMP Back
    Return:
ENDM EmptyKeyQueue
.386
.286
.model huge 
.stack 400 
.data
   ;GRID COLORS
   BLACK                 EQU 0
   BLUE                  EQU 1
   GREEN                 EQU 2
   AQUA                  EQU 3
   RED                   EQU 4
   PURPLE                EQU 5
   YELLOW                EQU 6
   WHITE                 EQU 15
   GRAY                  EQU 8
   LIGHT_BLUE            EQU 9
   LIFHT_GREEN           EQU 0AH
   LIGHT_AQUA            EQU 0BH
   LIGHT_RED             EQU 0CH
   LIGHT_PURPLE          EQU 0DH
   LIGHT_YELLOw          EQU 0EH
 
   ;*****************************
   WINDOW_WIDTH          EQU 80
   WINDOW_HIGHT          EQU 25
   MAX_USER_NAME_SIZE    EQU 20
   Widthh                EQU 20
   Height                EQU 20
   ;*****************************
   ;Keys codes
   ESC_ScanCode          EQU 01H
   ESC_AsciiCode         EQU 1BH
   Enter_ScanCode        EQU 1CH
   Enter_AsciiCode       EQU 0DH
   Back_ScanCode         EQU 0EH
   Back_AsciiCode        EQU 08H
   F1_ScanCode           EQU 3BH
   F2_ScanCode           EQU 3CH
   F3_ScanCode           EQU 3DH
   F4_ScanCode           EQU 3EH
   UP_AsciiCode          EQU 'W'                                          ;Application defined
   DOWN_AsciiCode        EQU 'S'                                          ;Application defined
   Left_AsciiCode        EQU 'A'
   Right_AsciiCode       EQU 'D'
   Q_AsciiCode           EQU 'Q'
   P1_IS_SELECTED        DB 0
   ;*****************************PLAYER 2 CONSTANTS**********

   P2_UP_AsciiCode          EQU "=";48H                                        ;Application defined
   P2_DOWN_AsciiCode        EQU "]";50H                                        ;Application defined
   P2_Left_AsciiCode        EQU "[";4BH
   P2_Right_AsciiCode       EQU "\";4DH
   P2_M_AsciiCode           EQU 9d ;"m"
   P2_IS_SELECTED        DB 0
   MY_PIECE_VARIABLE    DW ?
   ENEMY_PIECE_VARIABLE DW ?
   ;*************CHATTING VARIABLES**********************
    SX db 0d
    SY db 1d
    RX db 0d
    RY db 13d
    tempSX db 0d
    tempSY db 0d
    tempRX db 0d
    tempRY db 0d



   ;******MAIN SCREEN VARIABLES*******



   S_FirstTimeFlag          db 0
   R_FirstTimeFlag          db 0
   S_GAMEINVITE             db 0
   R_GAMEINVITE             db 0
    MSG_WELCOME           DB  "Please Enter Your Name: $"
   MSG_CONT              DB  "Press Enter Key to Continue$"
   MSG_CHAT              DB  "To start chatting press F1$"
   MSG_CLEAR             DB "                       $"
   MSG_CLEAR2             DB "                                 $"
   MSG_CLEAR_TIMER             DB "         $"
   MSG_GAME              DB  "To start the game press F2$"
   MSG_END               DB  "To end the program press ESC$"
   MSG_GAME_Choose_Color DB  "0->Black\n1->Blue , 2->Green , 3->Aqua ,"
   NameErrorMessage        DB "Please Enter Valid Name $"
   user_pressed          db  3 dup('?')
   MSG_CHATTING          DB  " Invited you to chat, press F1 to accept$"
   MSG_TOGAME            DB  " Invited you to game, Press F2 to accept$"
   MSG_TOGAME2           db  "You Sent a Game Invitation to $"
   MSG_CHATTING2         db  "You sent a Chat Invitation to $"
   mes_check             db  "Check !$"
  NameEntryConstrainsMsg db 'Max length 15 characters and must starts with a letter$'
   actual_p1_name_size   db  15,?
   player1name           DB  15 dup('$'),"$"
   actual_p2_name_size   db  15,?
   player2name           DB  15 dup('$'),"$"
   chatline              db "--------------------------------------------------------------------------------$"
   chatchar              db ":$"
   chatclear             db " .$"
   MSG_TERMINATE         DB "Ending Program, GoodBye!$"
   KILLEDPIECE           DW ?;20,?,20 DUP('$')
   PAWN                   DB  "Pawn $"
  Rook                    DB  "Rook $"
    Knight                DB  "Knight $"
    Bishop                DB  "Bishop $"
    Queen                 DB  "Queen $"
    King                  DB  "King $"
  YELLOWPLAYER            DB "YELLOW $"
  BLACKPALYER             DB "BLACK $"
  YNOTIFICATIONBLACK      DB 2
  YNOTIFICATIONYELLOW     DB 2
  YELLOWKINGDEAD          DB "BLACK WINS$"
  BLACKKINGDEAD           DB "YELLOW WINS$"
;***********************************time variables*************************************************
 CURRENTTIME DW 0
 counter_for_timer_test DW 0

 CURRENTSECOND DB  0,'$'
 CURRENTMINUTE DB 0,'$'
 CURRENTHOUR DB 0,'$'
 InlineP1    db 0
 InlineP2    db 0
 hour1 db 0
  hour2 db 0
   min1 db 0
   min2 db 0
    sec1 db 0
     sec2 db 0
 ;P2_CURRENTTIME DW 0
   ;******DRAWING GRID AND PIECES VARIABLES****
   GLOBAL_FILE_NAME      DB  7 DUP("$")                                   ; WE USE THIS IN DRAWCOMPLETECELL MACRO
   First                 DB  00,0,0
   BlackPawn1            DB  01,'BP.bin',0                                ;black pawn
   BlackPawn2            DB  01,'BP.bin',0                                ;black pawn
   BlackPawn3            DB  01,'BP.bin',0                                ;black pawn
   BlackPawn4            DB  01,'BP.bin',0                                ;black pawn
   BlackPawn5            DB  01,'BP.bin',0                                ;black pawn
   BlackPawn6            DB  01,'BP.bin',0                                ;black pawn
   BlackPawn7            DB  01,'BP.bin',0                                ;black pawn
   BlackPawn8            DB  01,'BP.bin',0                                ;black pawn
   BlackRook1            DB  09,'BR.bin',0
   BlackKnight1          DB  10,'BN.bin',0
   BlackBishop1          DB  11,'BF.bin',0
   BlackQueen            DB  12,'BQ.bin',0
   BlackKing             DB  13,'BK.bin',0
   BlackBishop2          DB  11,'BF.bin',0
   BlackKnight2          DB  10,'BN.bin',0
   BlackRook2            DB  09,'BR.bin',0
   
   ;*******YELLOW******;
   YellowPawn1           DB  17,'YP.bin',0
   YellowPawn2           DB  17,'YP.bin',0
   YellowPawn3           DB  17,'YP.bin',0
   YellowPawn4           DB  17,'YP.bin',0
   YellowPawn5           DB  17,'YP.bin',0
   YellowPawn6           DB  17,'YP.bin',0
   YellowPawn7           DB  17,'YP.bin',0
   YellowPawn8           DB  17,'YP.bin',0 ;ID KAN 24 5LETO 8 
   YellowRook1           DB  25,'YR.bin',0
   YellowKnight1         DB  26,'YN.bin',0
   YellowBishop1         DB  27,'YB.bin',0
   YellowQueen           DB  28,'YQ.bin',0
   YellowKing            DB  29,'YK.bin',0
   YellowBishop2         DB  27,'YB.bin',0
   YellowKnight2         DB  26,'YN.bin',0
   YellowRook2           DB  25,'YR.bin',0



   
   color1                db  ?
   color2                db  ?
   COLOR3 DW ?
   row                   dw  0004H
   column                dw  0048H
   Widthh                EQU 20
   Height                EQU 20

   Filehandle            DW  ?
   mes_Timer              db "Timer On$"
   ;*****PROCEDURES VARIABLES*********
   PData                 DB  Widthh*Height dup(0)
   ;********GRID CELLS ATTRUBUTES***************************
   ;cell_x_y   x , y ,isEmpty ,color , ID, TIMER
   ;Divide AL by 10
   GLOBAL_ROW             DW  ?
   GLOBAL_COLUMN          DW  ?
   GLOBAL_CELL            DW  6 DUP(?)
    Cell_1_1              Dw  72,     4  ,1,  8,   09,0
    Cell_1_2              DW  94,     4  ,1,  15,  10,0
    Cell_1_3              DW  116,    4  ,1,  8,   11,0
    Cell_1_4              DW  138,    4  ,1,  15,  12,0
    Cell_1_5              DW  160,    4  ,1,  8,   13,0
    Cell_1_6              DW  182,    4  ,1,  15,  11,0
    Cell_1_7              DW  204,    4  ,1,  8,   10,0
    Cell_1_8              DW  226,    4  ,1,  15,  09,0
    Cell_2_1              DW  72,    26  ,1,  15,  01,0
    Cell_2_2              DW  94,    26  ,1,  8,   01,0
    Cell_2_3              DW  116,   26,  1,  15,  01,0
    Cell_2_4              DW  138,   26,  1,  8,   01,0
    Cell_2_5              DW  160,   26,  1,  15,  01,0
    Cell_2_6              DW  182,   26,  1,  8,   01,0
    Cell_2_7              DW  204,   26,  1,  15,  01,0
    Cell_2_8              DW  226,   26,  1,  8,   01,0
    Cell_3_1              DW  72,    48,  0,  8,   00,0
    Cell_3_2              DW  94,    48,  0,  15,  00,0
    Cell_3_3              DW  116,   48,  0,  8,   00,0
    Cell_3_4              DW  138,   48,  0,  15,  00,0
    Cell_3_5              DW  160,   48,  0,  8,   00,0
    Cell_3_6              DW  182,   48,  0,  15,  00,0
    Cell_3_7              DW  204,   48,  0,  8,   00,0
    Cell_3_8              DW  226,   48,  0,  15,  00,0
    Cell_4_1              Dw  72,    70  ,0,  15,  00,0
    Cell_4_2              DW  94,    70  ,0,  8,   00,0
    Cell_4_3              DW  116,   70  ,0,  15,  00,0 
    Cell_4_4              DW  138,   70  ,0,  8,   00,0
    Cell_4_5              DW  160,   70  ,0,  15,  00,0
    Cell_4_6              DW  182,   70  ,0,  8,   00,0 
    Cell_4_7              DW  204,   70  ,0,  15,  00,0
    Cell_4_8              DW  226,   70  ,0,  8,   00,0
    Cell_5_1              DW  72,    92  ,0,  8,   00,0
    Cell_5_2              DW  94,    92  ,0,  15,  00,0
    Cell_5_3              DW  116,   92,  0,  8,   00,0
    Cell_5_4              DW  138,   92,  0,  15,  00,0
    Cell_5_5              DW  160,   92,  0,  8,   00,0
    Cell_5_6              DW  182,   92,  0,  15,  00,0
    Cell_5_7              DW  204,   92,  0,  8,   00,0
    Cell_5_8              DW  226,   92,  0,  15,  00,0
    Cell_6_1              DW  72,    114, 0,  15,  00,0
    Cell_6_2              DW  94,    114, 0,  8,   00,0
    Cell_6_3              DW  116,   114, 0,  15,  00,0
    Cell_6_4              DW  138,   114, 0,  8,   00,0
    Cell_6_5              DW  160,   114, 0,  15,  00,0 
    Cell_6_6              DW  182,   114, 0,  8,   00,0
    Cell_6_7              DW  204,   114, 0,  15,  00,0
    Cell_6_8              DW  226,   114, 0,  8,   00,0
    Cell_7_1              DW  72,    136 ,2,  8,   17,0
    Cell_7_2              DW  94,    136 ,2,  15,  17,0
    Cell_7_3              DW  116,   136, 2,  8,   17,0
    Cell_7_4              DW  138,   136, 2,  15,  17,0
    Cell_7_5              DW  160,   136, 2,  8,   17,0
    Cell_7_6              DW  182,   136, 2,  15,  17,0
    Cell_7_7              DW  204,   136, 2,  8,   17,0
    Cell_7_8              DW  226,   136, 2,  15,  17,0
    Cell_8_1              DW  72,    158, 2,  15,  25,0
    Cell_8_2              DW  94,    158, 2,  8,   26,0
    Cell_8_3              DW  116,   158, 2,  15,  27,0
    Cell_8_4              DW  138,   158, 2,  8,   28,0
    Cell_8_5              DW  160,   158, 2,  15,  29,0
    Cell_8_6              DW  182,   158, 2,  8,   27,0
    Cell_8_7              DW  204,   158, 2,  15,  26,0
    Cell_8_8              DW  226,   158, 2,  8,   25,0
   ID_PARAMETER           DW  ?       ;FOR SET FILE NAME                                    ;SETFILE NAME
   ID_PARAMETER2          DW  ?       ;FOR SET CELL IN P1_Q
   ID_PARAMETER3          DW  ?  
   FlagParameter DW ? 
   WaitingPlayerMsg            DB      'Waiting other player to connect...                          $'
  Number_Of_Stored_Highlighted_Cells dw  0
  temp_Number_Of_Stored_Cells_In_Highlighted_Cells_Array  DW 0;the function SET_Is_Allowed_To_Move_Flag USES THIS  
  Array_Of_Highlighted_X_Y     dw  64 DUP (?)
  

  index dw 0
  temp_global_row dw ?
  temp_global_column dw ?
  temp_global_cell dw ?
  GLOBAL_COUNTER_PAWN DW ?
  ;Temp_Selected_Cell_Index dw ? 
   ;*************FOR HIHGLIGHTING AND DEHIGHLITING CELLS WHILE THE USER SELECT A PIECE**************
   CurrentXPosition      Dw  226
   CurrentYPosition      Dw  4
   PreviousXPosition     Dw  226
   PreviousYPosition     Dw  4
   Color                 DW  ?

   x                     dw  ?                                          
   y                     dw  ?
   Selected_cell_Index   dw ?
   Temp_Selected_Cell_Index dw ? ;the function SET_Is_Allowed_To_Move_Flag USES THIS 
   Is_Allowed_To_Move_Flag dw 0 ;initially no cell allowed to mov ,until this is set to 1
   

Temp_Selected_Cell_Index2 DW ?
temp_global_column2 DW ?
temp_global_row2 DW ?

temp_global_row4 DW ?
temp_global_column4 DW ?  
;******************************************PLAYER 2 VARIABLES**************************
;******************************************PLAYER 2 VARIABLES**************************
   P2_color1                db  ?
   P2_color2                db  ?
   P2_COLOR3 DW ?

  P2_temp_global_column4 dw ?
  P2_temp_global_row4 dw ?
   P2_CurrentXPosition      Dw  72
   P2_CurrentYPosition      Dw  158
   P2_PreviousXPosition     Dw  72
   P2_PreviousYPosition     Dw  158
   P2_Color                 DW  ?
   P2_x                     dw  ?                                            
   P2_y                     dw  ?
   P2_Selected_cell_Index   dw ?
   P2_Temp_Selected_Cell_Index dw ? ;the function SET_Is_Allowed_To_Move_Flag USES THIS 
   P2_Is_Allowed_To_Move_Flag dw 0 ;initially no cell allowed to mov ,until this is set to 1
   P2_ID_PARAMETER           DW  ?       ;FOR SET FILE NAME                                    ;SETFILE NAME
   P2_ID_PARAMETER2          DW  ?       ;FOR SET CELL IN P1_Q
   P2_ID_PARAMETER3          DW  ?  
   P2_FlagParameter DW ? 
  P2_Number_Of_Stored_Highlighted_Cells dw  0
  P2_temp_Number_Of_Stored_Cells_In_Highlighted_Cells_Array  DW 0;the function SET_Is_Allowed_To_Move_Flag USES THIS  
  P2_Array_Of_Highlighted_X_Y     dw  64 DUP (?)
  P2_index dw 0
  P2_temp_global_row dw ?
  P2_temp_global_column dw ?
  P2_temp_global_cell dw ?
  P2_GLOBAL_COUNTER_PAWN DW ?
 P2_GLOBAL_ROW             DW  ?
 P2_GLOBAL_COLUMN          DW  ?
 P2_GLOBAL_CELL            DW  6 DUP (?)

 P2_Temp_Selected_Cell_Index2 DW ?
 P2_temp_global_column2 DW ?
 P2_temp_global_row2 DW ?
 Array_Of_Possible_Moves_For_Check_P1    dw  272 DUP (?) ;max possible moves for the enemy 
  CHECK_FLAG  dw 0
  Number_of_Cells_In_Array_Of_Check_p1 dw ?
  totalCurrentTime dw ? ;used in p1 pressed q 
;******************************************END PLAYER 2 VARIABLES**************************
;*********************************SERIAL***************
ISBLACKPLAYER         DB   0
p1_checks_p2_msg db "Black player cheks yellow$"
p2_checks_p1_msg db "yellow player cheks black$"
.CODE
DrawPiece PROC FAR
             ;local drawLoop , white1
             push  ax
             push  cx
             push  dx
             PUSH SI
             PUSH DI 
            
                                              LEA                   BX , PData                                   ; BL contains index at the current drawn pixel
             MOV   CX,GLOBAL_COLUMN
             MOV   SI,CX
             ADD   SI,Widthh
             MOV   DX,GLOBAL_ROW
             MOV   DI,DX
             ADD   DI,Height
             MOV   AH,0ch
   ; Drawing loop
   drawLoop: 
             MOV   AL,[BX]
             cmp   al,0fh
             jz    white1
             INT   10h
   white1:   
             INC   CX
             INC   BX
             CMP   CX,SI
             JNE   drawLoop

             MOV   CX ,GLOBAL_COLUMN
             INC   DX
             CMP   DX ,DI
             JNE   drawLoop
             
             
             POP DI
             POP SI 
             pop   dx
             pop   cx
             pop   ax
             RET
DrawPiece ENDP 
GET_CELL_FROM_XY PROC FAR
    ;LOCAL SET_GLOBAL_CELL
   
                                 PUSH                  AX
                                 PUSH                  BX
                                 PUSH                  CX
                                 PUSH                  DX
                                 PUSH                  SI
                                 PUSH                  DI
                                 MOV                   DX,0
                                 MOV                   AX,GLOBAL_ROW
                                 SUB                   AX,4                                         ;ROW-4
                                 MOV                   BX,22
                                 DIV                   BX                                           ;AX HAS (ROW-4)/22
                                 MOV                   CX,96
                                 MUL                   CX                                           ;AX HAS (ROW-4)/22 *8*10
                                 MOV                   CX,AX                                        ;CX HAS (ROW-4)/22 *8*10
                                 MOV                   DX,0
                                 MOV                   AX,GLOBAL_COLUMN
                                 SUB                   AX,72                                        ;COLUMN - 72
                                 MOV                   BX,22
                                 DIV                   BX                                           ;AX HAS (COLUMN - 72)/22
                                 MOV                   BX,12
                                 MUL                   BX                                           ;AX HAS (COLUMN - 72)/22 *10
                                 ADD                   AX,CX
   ;AX HAS INDEX OF NEEDED CELL
                                 MOV                   BX,AX
                                 MOV                   SI,0
                                 MOV                   CX,6
   SET_GLOBAL_CELL:              
                                 MOV                   AX,Cell_1_1[BX]
                                 MOV                   GLOBAL_CELL[SI],AX
                                 ADD                   SI,2
                                 ADD                   BX,2
                                 DEC                   CX
                                 JNZ                   SET_GLOBAL_CELL

                                 POP                   DI
                                 POP                   SI
                                 POP                   DX
                                 POP                   CX
                                 POP                   BX
                                 POP                   AX
                                 RET
GET_CELL_FROM_XY ENDP
CLEAR_CELL PROC ;ID , X,Y ,CELL
  push si   
   MOV SI,Selected_cell_Index 
   MemoryToMemoryMovWord ID_PARAMETER3, Cell_1_1[SI+8]
   MemoryToMemoryMovWord FlagParameter, Cell_1_1[SI+4] 

   MOV Cell_1_1[SI+4],00 
   MOV Cell_1_1[SI+8],0 
   Draw_Square_Macro_X_Y_COLOR Cell_1_1[si],Cell_1_1[si+2],Cell_1_1[si+6]
   pop si

RET
   CLEAR_CELL ENDP


setfilename PROC FAR

                                 PUSH                  AX
                                 PUSH                  BX
                                 PUSH                  CX
                                 PUSH                  DX
                                 PUSH                  SI
                                 PUSH                  DI

                                 MOV                   BL,8
                                 MOV                   AL,BYTE PTR ID_PARAMETER
                                 MUL                   BL                                           ;HERE AX HAS 8N WHERE N IS THE PIECE INDEX
                                 SUB                   AX,4                                         ;HERE AX HAS 8N-4 WHERE N IS THE PIECE INDEX
                                 MOV                   SI,AX
                                 MOV                   DL,7                                         ;7 BECAUSE THE FILE NAME IS 6 CHARS PLUS NULL TERMINATOR
                                 MOV                   DI,0
   MOVE_FILE_NAME_TO_GLOBAL_VAR: 

                                 MOV                   BL,FIRST[SI]
                                 MOV                   GLOBAL_FILE_NAME[DI],BL
                                 INC                   SI
                                 INC                   DI
                                 DEC                   DL
                                 JNZ                   MOVE_FILE_NAME_TO_GLOBAL_VAR
                                

                                 POP                   DI
                                 POP                   SI
                                 POP                   DX
                                 POP                   CX
                                 POP                   BX
                                 POP                   AX
                                 ret
setfilename ENDP

SET_Selected_cell_Index  PROC ;ID , X,Y ,CEL
PUSH ax
PUSH BX
PUSH CX
PUSH DX
PUSH SI

                                 MOV                   DX,0
                                 MOV                   AX,GLOBAL_ROW
                                 SUB                   AX,4                                         ;ROW-4
                                 MOV                   BX,22
                                 DIV                   BX                                           ;AX HAS (ROW-4)/22  ,DX :REMAINDER
                                 MOV                   CX,96
                                 MUL                   CX                                           ;AX HAS (ROW-4)/22 *8*10 ;DX:MOST SIGNFICANT 
                                 MOV                   CX,AX                                        ;CX HAS (ROW-4)/22 *8*10
                                 MOV                   DX,0
                                 MOV                   AX,GLOBAL_COLUMN
                                 SUB                   AX,72                                        ;COLUMN - 72
                                 MOV                   BX,22
                                 DIV                   BX                                           ;AX HAS (COLUMN - 72)/22
                                 MOV                   BX,12
                                 MUL                   BX                                           ;AX HAS (COLUMN - 72)/22 *10
                                 ADD                   AX,CX
   ;AX HAS INDEX OF NEEDED CELL
                                 MOV Selected_cell_Index,AX   ;STORES INDEX OF SELECTED CELL BEFORE MOVEMENT
                                
POP SI                               
POP DX
POP CX
POP BX
POP AX  

RET
   SET_Selected_cell_Index ENDP
Draw_Complete_Cell_Proc PROC FAR
push si
MOV SI,6
DrawSquare GLOBAL_CELL[SI]

MOV SI,8
                                 cmp                   GLOBAL_CELL[SI],0;word ptr [si+8],0                            ;Compares celll[8] : ID of the empty cell _> if empty , draw the square of the same color
                                 jz                    FARJUMP_DrawEmptyCell
                                 JMP                   SKIPFARJUMP_DrawEmptyCell
                                  
   FARJUMP_DrawEmptyCell:        
                                 JMP                   FAR PTR DrawEmptyCell
   SKIPFARJUMP_DrawEmptyCell:    

                                 MemoryToMemoryMovWord ID_PARAMETER ,GLOBAL_CELL[SI] 
                                 call                  setfilename

                                 OpenFile              GLOBAL_FILE_NAME
                                 ReadData


                                ; LEA                   BX , PData                                   ; BL contains index at the current drawn pixel
                                 MOV SI,0
                                 ;DrawPieceMacro 138,26
                                 call DrawPiece          ;   GLOBAL_COLUMN,GLOBAL_ROW   ;138,26                                    ;word ptr [si],word ptr[si+2]
    

                                 CloseFile
   DrawEmptyCell:                
                             pop si
                                 RET
Draw_Complete_Cell_Proc ENDP

 DeHighlightCells PROC FAR

 PUSH AX
 push DI

MemoryToMemoryMovWord temp_global_row,GLOBAL_ROW
MemoryToMemoryMovWord temp_global_column,GLOBAL_COLUMN

mov DI,0 
MOV AX ,Number_Of_Stored_Highlighted_Cells
CMP AX,0

JZ END_TRAVERSAL 

 Traversing_array:
   ;****************************************************************

 MemoryToMemoryMovWord GLOBAL_COLUMN,Array_Of_Highlighted_X_Y[DI]
 add DI,2 

 MemoryToMemoryMovWord GLOBAL_ROW,Array_Of_Highlighted_X_Y[DI]
 add DI,2 
 
 
  call GET_CELL_FROM_XY
  call Draw_Complete_Cell_Proc
  
  ;****************************************************************


 dec Number_Of_Stored_Highlighted_Cells 
 jnz Traversing_array

END_TRAVERSAL:

MemoryToMemoryMovWord GLOBAL_ROW,temp_global_row
MemoryToMemoryMovWord GLOBAL_COLUMN,temp_global_column
 CALL GET_CELL_FROM_XY


 pop DI
 POP AX
 RET
 DeHighlightCells ENDP  
 



 SET_CELL PROC FAR
 push si   

   CALL SET_Selected_cell_Index
   MOV SI,Selected_cell_Index
   MemoryToMemoryMovWord KILLEDPIECE,Cell_1_1[SI+8]   ;SET CELL 1 AND 2
   MemoryToMemoryMovWord Cell_1_1[SI+8],ID_PARAMETER3 ;ID_PARAMETER2 ;UPDATES ID OF NEW CELL 
  ; MOV Cell_1_1[SI+4],1 ;UPDATE STATUS OF THIS CELL :HOLDS PIECE RELATED TO PLAYER 1 
  MemoryToMemoryMovWord Cell_1_1[SI+4],FlagParameter
 
   MemoryToMemoryMovWord counter_for_timer_test, CURRENTTIME    
   ADD counter_for_timer_test,3                       ;NEW
   MemoryToMemoryMovWord Cell_1_1[si+10], counter_for_timer_test          ;NEW SET CELL 1 W 2

 CALL GET_CELL_FROM_XY ;CHECK
 CALL Draw_Complete_Cell_Proc
 
   POP SI

RET
SET_CELL ENDP


Draw_Border_Proc  PROC     ; x,y,color ; CURRENT X CX , CURRENT Y : DX,COLOR:WHITE COLOR1
;local draw_vertical_line ,draw_Two_vertical_lines, draw_Two_Horizontal_linessss , draw_Horizontal_linesss
PUSH CX
PUSH DX
push bx
push ax



Mov cx ,X
;INC Y
mov dx,Y
MOV BH,2 
draw_Two_Horizontal_linessss:
mov bl,22 ;counter
mov   al,byte ptr color        ;Pixel color
mov   ah,0ch
;Draw Pixel 

draw_Horizontal_linesss:
int 10h  
INC cx
dec bl
jnz draw_Horizontal_linesss
ADD dX,21 ;DY KANY SUB 5LTHA
MOV cX,x
dec bh
jnz draw_Two_Horizontal_linessss



mov cx ,x
;SUB Y,2
mov dx,y
MOV BH,2 
draw_Two_vertical_lines:
mov bl,21 ;
mov   al,byte ptr color        ;Pixel color
mov   ah,0ch
;Draw Pixel 

draw_vertical_line:
int 10h  
INC dx
dec bl ;DY KANT INC ANA LSA M8YRAHA
jnz draw_vertical_line
ADD CX,21
MOV DX,y
dec bh
jnz draw_Two_vertical_lines

Mov cx ,X
mov dx,Y

pop ax
pop bx
POP DX
POP CX
RET
Draw_Border_Proc ENDP 
UP_PRESSED_PROC PROC FAR
PUSH CX
PUSH DX
MOV CX,CurrentXPosition
MOV DX,CurrentYPosition
     mov PreviousYPosition,dx  
     mov PreviousXPosition,cx 
                 
                                 CMP                   DX,4
                                 
                                 JE                    UPPER_BORDER_REACHED1                         ;Store Previous Position to de Highlight the cell after selecting
                                
                               JMP SKIP_FAR_UPPER_BORDER_JUMP
                                UPPER_BORDER_REACHED1:
                                JMP FAR PTR UPPER_BORDER_REACHED
                                
                                SKIP_FAR_UPPER_BORDER_JUMP:
                             
                                 sub                   DX,22                                        ;DX storing the current y position -> increment it by 22 upwards  while x is constant
                                 MOV                   CurrentYPosition,DX                          ;INITIALLY 158
                                 push ax
                                 mov ax,00
                                 mov al,color2
                                 mov color3,ax
                                 pop ax
                                MemoryToMemoryMovWord x,PreviousXPosition
                                MemoryToMemoryMovWord y,PreviousYPosition
                                MemoryToMemoryMovWord color,color3
                                call  Draw_Border_Proc           ;PreviousXPosition,PreviousYPosition,color1   ;unHighlight the current cell to highlight the new one after pressing up button (drwaing above it white or grey)
                                MemoryToMemoryMovWord x,CurrentXPosition
                                MemoryToMemoryMovWord y,CurrentYPosition
                                ;IF THE CELL IS SELECTED (Q IS PRESSED ) AND ALSO THE MOV IS LEGAL , HIGHLIGHT THE CELL WITH ANOTHER COLOR UNTIL ITS DROPPED
                                CMP P1_IS_SELECTED,1
                                JE change_color
                                jmp skip_change_color
                                change_color:
                              MemoryToMemoryMovWord color,RED 
                              JMP Dont_Change_Color
                               skip_change_color:
                               MemoryToMemoryMovWord color,LIGHT_BLUE
                               Dont_Change_Color:
                                 call Draw_Border_Proc            ;CurrentXPosition,CurrentYPosition,0EH        ;Highlight the new cell with yellow color
                   
                                 exchange              color1,color2
                                 jmp                   END_UP                                       ;Because initially we are starting from cell_8_1 (its color is white which is stored in variable color2)
   UPPER_BORDER_REACHED:
   END_UP:                                                                             ;at any mov , regardless how many times , we will exchange grey and white stored in color1,color2
  MOV CurrentXPosition,CX
MOV CurrentYPosition,DX
POP DX
POP CX
    RET
UP_PRESSED_PROC ENDP


RIGHT_PRESSED_PROC PROC FAR
PUSH CX
PUSH DX
MOV CX,CurrentXPosition
MOV DX,CurrentYPosition
 mov PreviousYPosition,dx  
 mov PreviousXPosition,cx 
                 
                                 CMP                   CX,226
                               JE BORDER_REACHED1 

                               JMP SKIP_BORDER_REACHED 
                               BORDER_REACHED1:
                               JMP FAR PTR RIGHT_BORDER_REACHED
                               SKIP_BORDER_REACHED:
                                 ADD                   CX,22                                        ;DX storing the current y position -> increment it by 22 upwards  while x is constant
                                 MOV                   CurrentXPosition,CX                          ;INITIALLY 158
                                 push ax
                                 mov ax,00
                                 mov al,color2
                                 mov color3,ax
                                 pop ax
                                MemoryToMemoryMovWord x,PreviousXPosition
                                MemoryToMemoryMovWord y,PreviousYPosition
                                MemoryToMemoryMovWord color,color3
                                call  Draw_Border_Proc           ;PreviousXPosition,PreviousYPosition,color1   ;unHighlight the current cell to highlight the new one after pressing up button (drwaing above it white or grey)
                                 MemoryToMemoryMovWord x,CurrentXPosition
                               
                                MemoryToMemoryMovWord y,CurrentYPosition
                                
                                CMP P1_IS_SELECTED,1
                                JE change_color_Right
                                jmp skip_change_color_Right
                                change_color_Right:
                                MemoryToMemoryMovWord color,RED 
                                JMP Dont_Change_Color_Right
                                skip_change_color_Right:
                                MemoryToMemoryMovWord color,LIGHT_BLUE
                                Dont_Change_Color_Right:
                                call Draw_Border_Proc            ;CurrentXPosition,CurrentYPosition,0EH        ;Highlight the new cell with yellow color
                   
                                 exchange              color1,color2
                                 jmp                   END_RIGHT                                       ;Because initially we are starting from cell_8_1 (its color is white which is stored in variable color2)
   RIGHT_BORDER_REACHED:
   END_RIGHT:                                                                             ;at any mov , regardless how many times , we will exchange grey and white stored in color1,color2
  
MOV CurrentXPosition,CX
MOV CurrentYPosition,DX
POP DX
POP CX

RET 
RIGHT_PRESSED_PROC ENDP

DOWN_PRESSED_PROC PROC FAR
PUSH CX
PUSH DX
MOV CX,CurrentXPosition
MOV DX,CurrentYPosition
     mov PreviousYPosition,dx  
     mov PreviousXPosition,cx 
                 
                                 CMP                   DX,158
                               
                                JE DOWN_REACHED1
                                JMP SKIP_LOWER_FAR_JUMPPP
                                DOWN_REACHED1:

                                JMP FAR PTR LOWER_BORDER_REACHED
                               SKIP_LOWER_FAR_JUMPPP:
                                ADD               DX,22                                        ;DX storing the current y position -> increment it by 22 upwards  while x is constant
                                 MOV                   CurrentYPosition,DX                          ;INITIALLY 158
                                 push ax
                                 mov ax,00
                                 mov al,color2
                                 mov color3,ax
                                 pop ax
                                MemoryToMemoryMovWord x,PreviousXPosition
                                MemoryToMemoryMovWord y,PreviousYPosition
                                MemoryToMemoryMovWord color,color3
                                call  Draw_Border_Proc           ;PreviousXPosition,PreviousYPosition,color1   ;unHighlight the current cell to highlight the new one after pressing up button (drwaing above it white or grey)
                                 MemoryToMemoryMovWord x,CurrentXPosition
                                MemoryToMemoryMovWord y,CurrentYPosition
                              
                               CMP P1_IS_SELECTED,1
                                JE change_color_Down
                                jmp skip_change_color_Down
                                change_color_Down:
                                MemoryToMemoryMovWord color,RED 
                                JMP Dont_Change_Color_Down
                                skip_change_color_Down:
                                MemoryToMemoryMovWord color,LIGHT_BLUE
                                Dont_Change_Color_Down:
                                 call Draw_Border_Proc            ;CurrentXPosition,CurrentYPosition,0EH        ;Highlight the new cell with yellow color
                   
                                 exchange              color1,color2
                                 jmp                   END_DOWN                                       ;Because initially we are starting from cell_8_1 (its color is white which is stored in variable color2)
   LOWER_BORDER_REACHED:
   END_DOWN:                                                                             ;at any mov , regardless how many times , we will exchange grey and white stored in color1,color2
    MOV CurrentXPosition,CX
MOV CurrentYPosition,DX
POP DX
POP CX
    RET
DOWN_PRESSED_PROC ENDP

LEFT_PRESSED_PROC PROC FAR
PUSH CX
PUSH DX
MOV CX,CurrentXPosition
MOV DX,CurrentYPosition
 mov PreviousYPosition,dx  
 mov PreviousXPosition,cx 
                 
                                 CMP                   CX,72

                                 JE                    LEFT_BORDER_REACHED1
                                 JMP SKIP_FAR_LEFT_BORDER_REACHED
                                 LEFT_BORDER_REACHED1:
                              
                               JMP FAR PTR LEFT_BORDER_REACHED
                               
                         SKIP_FAR_LEFT_BORDER_REACHED:

                                 SUB                   CX,22                                        ;DX storing the current y position -> increment it by 22 upwards  while x is constant
                                 MOV                   CurrentXPosition,CX                          ;INITIALLY 158
                                 push ax
                                 mov ax,00
                                 mov al,color2
                                 mov color3,ax
                                 pop ax
                                MemoryToMemoryMovWord x,PreviousXPosition
                                MemoryToMemoryMovWord y,PreviousYPosition
                                MemoryToMemoryMovWord color,color3
                                call  Draw_Border_Proc           ;PreviousXPosition,PreviousYPosition,color1   ;unHighlight the current cell to highlight the new one after pressing up button (drwaing above it white or grey)
                                 MemoryToMemoryMovWord x,CurrentXPosition
             
                                MemoryToMemoryMovWord y,CurrentYPosition
                               
                               CMP P1_IS_SELECTED,1
                                JE change_color_Left
                                jmp skip_change_color_Left
                                change_color_Left:
                                MemoryToMemoryMovWord color,RED 
                                JMP Dont_Change_Color_Left
                                skip_change_color_Left:
                                MemoryToMemoryMovWord color,LIGHT_BLUE
                                Dont_Change_Color_Left:
                                 call Draw_Border_Proc            ;CurrentXPosition,CurrentYPosition,0EH        ;Highlight the new cell with yellow color
                   
                                 exchange              color1,color2
                                 jmp                   END_LEFT                                       ;Because initially we are starting from cell_8_1 (its color is white which is stored in variable color2)
   LEFT_BORDER_REACHED:
   END_LEFT:                                                                             ;at any mov , regardless how many times , we will exchange grey and white stored in color1,color2
  
MOV CurrentXPosition,CX
MOV CurrentYPosition,DX
POP DX
POP CX

RET 
LEFT_PRESSED_PROC ENDP



;***************************************PLAYER 2 FUNCTIONS*****************************

P2_Draw_Border_Proc  PROC     ; x,y,color ; CURRENT X CX , CURRENT Y : DX,COLOR:WHITE COLOR1
;local draw_vertical_line ,draw_Two_vertical_lines, draw_Two_Horizontal_linessss , draw_Horizontal_linesss
 PUSH CX
 PUSH DX
 push bx
 push ax



Mov cx ,P2_X
;INC Y
mov dx,P2_Y
MOV BH,2 
P2_draw_Two_Horizontal_linessss:
mov bl,22 ;counter
mov   al,byte ptr P2_color        ;Pixel color
mov   ah,0ch
;Draw Pixel 

P2_draw_Horizontal_linesss:
int 10h  
INC cx
dec bl
jnz P2_draw_Horizontal_linesss
ADD dX,21 ;DY KANY SUB 5LTHA
MOV cX,P2_x
dec bh
jnz P2_draw_Two_Horizontal_linessss



mov cx ,P2_x
;SUB Y,2
mov dx,P2_y
MOV BH,2 
P2_draw_Two_vertical_lines:
mov bl,21 
mov   al,byte ptr P2_color        ;Pixel color
mov   ah,0ch
;Draw Pixel 

P2_draw_vertical_line:
int 10h  
INC dx
dec bl ;DY KANT INC ANA LSA M8YRAHA
jnz P2_draw_vertical_line
ADD CX,21
MOV DX,P2_y
dec bh
jnz P2_draw_Two_vertical_lines

Mov cx ,P2_X
mov dx,P2_Y

pop ax
pop bx
POP DX
POP CX
RET
P2_Draw_Border_Proc ENDP 

P2_UP_PRESSED_PROC PROC FAR
 ;PUSH CX
 ;PUSH DX
 PUSH CX
 PUSH DX
   MOV CX,P2_CurrentXPosition
   MOV DX,P2_CurrentYPosition

     mov P2_PreviousYPosition,dx  
     mov P2_PreviousXPosition,cx 
     
                 
                                 CMP                   DX,4
                                 
                                 JE                    P2_UPPER_BORDER_REACHED1                         ;Store Previous Position to de Highlight the cell after selecting
                                
                               JMP P2_SKIP_FAR_UPPER_BORDER_JUMP
                                P2_UPPER_BORDER_REACHED1:
                                JMP FAR PTR P2_UPPER_BORDER_REACHED
                                
                                P2_SKIP_FAR_UPPER_BORDER_JUMP:
                             
                                 sub                   DX,22                                        ;DX storing the current y position -> increment it by 22 upwards  while x is constant
                                 MOV                   P2_CurrentYPosition,DX                          ;INITIALLY 158
                                 push ax
                                 mov ax,00
                                 mov al,P2_color2
                                 mov P2_color3,ax
                                 pop ax
                                MemoryToMemoryMovWord P2_x,P2_PreviousXPosition ;M8YRNASH X,Y
                                MemoryToMemoryMovWord P2_y,P2_PreviousYPosition
                                MemoryToMemoryMovWord P2_color,P2_color3
                                call  P2_Draw_Border_Proc           ;PreviousXPosition,PreviousYPosition,color1   ;unHighlight the current cell to highlight the new one after pressing up button (drwaing above it white or grey)
                                MemoryToMemoryMovWord P2_x,P2_CurrentXPosition
                                MemoryToMemoryMovWord P2_y,P2_CurrentYPosition
                                ;IF THE CELL IS SELECTED (Q IS PRESSED ) AND ALSO THE MOV IS LEGAL , HIGHLIGHT THE CELL WITH ANOTHER COLOR UNTIL ITS DROPPED
                                CMP P2_IS_SELECTED,1
                                JE P2_change_color
                                jmp P2_skip_change_color
                                P2_change_color:
                               MemoryToMemoryMovWord P2_color,RED 
                               JMP P2_Dont_Change_Color
                               P2_skip_change_color:
                               MemoryToMemoryMovWord P2_color,AQUA
                               P2_Dont_Change_Color:
                               call P2_Draw_Border_Proc            ;CurrentXPosition,CurrentYPosition,0EH        ;Highlight the new cell with yellow color
                   
                                 exchange              P2_color1,P2_color2
                                 jmp                   P2_END_UP                                       ;Because initially we are starting from cell_8_1 (its color is white which is stored in variable color2)
   P2_UPPER_BORDER_REACHED:
   P2_END_UP:                                                                             ;at any mov , regardless how many times , we will exchange grey and white stored in color1,color2
  ;POP DX
  ;POP CX 
  MOV P2_CurrentXPosition,CX
 MOV P2_CurrentYPosition,DX
 POP DX
 POP CX
    RET
P2_UP_PRESSED_PROC ENDP

P2_DOWN_PRESSED_PROC PROC FAR
  PUSH CX
 PUSH DX
   MOV CX,P2_CurrentXPosition
   MOV DX,P2_CurrentYPosition
     mov P2_PreviousYPosition,dx  
     mov P2_PreviousXPosition,cx 
                 
                                 CMP                   DX,158
                               
                                JE P2_DOWN_REACHED1
                                JMP P2_SKIP_LOWER_FAR_JUMPPP
                                P2_DOWN_REACHED1:

                                JMP FAR PTR P2_LOWER_BORDER_REACHED
                               P2_SKIP_LOWER_FAR_JUMPPP:
                                ADD               DX,22                                        ;DX storing the current y position -> increment it by 22 upwards  while x is constant
                                 MOV                   P2_CurrentYPosition,DX                          ;INITIALLY 158
                                 push ax
                                 mov ax,00
                                 mov al,P2_color2;
                                 mov P2_color3,ax
                                 pop ax
                                MemoryToMemoryMovWord P2_x,P2_PreviousXPosition
                                MemoryToMemoryMovWord P2_y,P2_PreviousYPosition
                                MemoryToMemoryMovWord P2_color,P2_color3
                                call  P2_Draw_Border_Proc           ;PreviousXPosition,PreviousYPosition,color1   ;unHighlight the current cell to highlight the new one after pressing up button (drwaing above it white or grey)
                                 MemoryToMemoryMovWord P2_x,P2_CurrentXPosition
                                MemoryToMemoryMovWord  P2_y, P2_CurrentYPosition
                              
                               CMP P2_IS_SELECTED,1
                                JE P2_change_color_Down
                                jmp P2_skip_change_color_Down
                                P2_change_color_Down:
                                MemoryToMemoryMovWord P2_color,RED 
                                JMP P2_Dont_Change_Color_Down
                                P2_skip_change_color_Down:
                                MemoryToMemoryMovWord P2_color,AQUA
                                P2_Dont_Change_Color_Down:
                                 call P2_Draw_Border_Proc            ;CurrentXPosition,CurrentYPosition,0EH        ;Highlight the new cell with yellow color
                   
                                 exchange              P2_color1,P2_color2
                                 jmp                   P2_END_DOWN                                       ;Because initially we are starting from cell_8_1 (its color is white which is stored in variable color2)
   P2_LOWER_BORDER_REACHED:
   P2_END_DOWN:                                                                             ;at any mov , regardless how many times , we will exchange grey and white stored in color1,color2
  MOV P2_CurrentXPosition,CX
 MOV P2_CurrentYPosition,DX
 POP DX
 POP CX
    RET
P2_DOWN_PRESSED_PROC ENDP


P2_LEFT_PRESSED_PROC PROC FAR
 PUSH CX
 PUSH DX
    MOV CX,P2_CurrentXPosition
    MOV DX,P2_CurrentYPosition
  mov P2_PreviousYPosition,dx  
  mov P2_PreviousXPosition,cx 
                 
                                 CMP                   CX,72

                                 JE                    P2_LEFT_BORDER_REACHED1
                                 JMP P2_SKIP_FAR_LEFT_BORDER_REACHED
                                 P2_LEFT_BORDER_REACHED1:
                              
                               JMP FAR PTR P2_LEFT_BORDER_REACHED
                               
                         P2_SKIP_FAR_LEFT_BORDER_REACHED:

                                 SUB                   CX,22                                        ;DX storing the current y position -> increment it by 22 upwards  while x is constant
                                 MOV                   P2_CurrentXPosition,CX                          ;INITIALLY 158
                                 push ax
                                 mov ax,00
                                 mov al,P2_color2
                                 mov P2_color3,ax
                                 pop ax
                                MemoryToMemoryMovWord P2_x,P2_PreviousXPosition
                                MemoryToMemoryMovWord P2_y,P2_PreviousYPosition
                                MemoryToMemoryMovWord P2_color,P2_color3
                                call  P2_Draw_Border_Proc           ;PreviousXPosition,PreviousYPosition,color1   ;unHighlight the current cell to highlight the new one after pressing up button (drwaing above it white or grey)
                                 MemoryToMemoryMovWord P2_x,P2_CurrentXPosition
                                MemoryToMemoryMovWord  P2_y,P2_CurrentYPosition
                               
                               CMP P2_IS_SELECTED,1
                                JE P2_change_color_Left
                                jmp P2_skip_change_color_Left
                                P2_change_color_Left:
                                MemoryToMemoryMovWord P2_color,RED 
                                JMP P2_Dont_Change_Color_Left
                                P2_skip_change_color_Left:
                                MemoryToMemoryMovWord P2_color,AQUA
                                P2_Dont_Change_Color_Left:
                                 call P2_Draw_Border_Proc            ;CurrentXPosition,CurrentYPosition,0EH        ;Highlight the new cell with yellow color
                   
                                 exchange              P2_color1,P2_color2
                                 jmp                   P2_END_LEFT                                       ;Because initially we are starting from cell_8_1 (its color is white which is stored in variable color2)
   P2_LEFT_BORDER_REACHED:
   P2_END_LEFT:                                                                             ;at any mov , regardless how many times , we will exchange grey and white stored in color1,color2
  
  MOV P2_CurrentXPosition,CX
 MOV P2_CurrentYPosition,DX
 POP DX
 POP CX

 RET 
P2_LEFT_PRESSED_PROC ENDP


P2_RIGHT_PRESSED_PROC PROC FAR
 PUSH CX
 PUSH DX
   MOV CX,P2_CurrentXPosition
   MOV DX,P2_CurrentYPosition
 mov P2_PreviousYPosition,dx  
 mov P2_PreviousXPosition,cx 
                 
                                 CMP                   CX,226
                                 JE P2_BORDER_REACHED1 

                                 JMP P2_SKIP_BORDER_REACHED 
                                 P2_BORDER_REACHED1:
                                 JMP FAR PTR P2_RIGHT_BORDER_REACHED
                                 P2_SKIP_BORDER_REACHED:
                                 ADD                   CX,22                                        ;DX storing the current y position -> increment it by 22 upwards  while x is constant
                                 MOV                   P2_CurrentXPosition,CX                          ;INITIALLY 158
                                 push ax
                                 mov ax,00
                                 mov al,P2_color2
                                 mov P2_color3,ax
                                 pop ax
                                MemoryToMemoryMovWord P2_x,P2_PreviousXPosition
                                MemoryToMemoryMovWord P2_y,P2_PreviousYPosition
                                MemoryToMemoryMovWord P2_color,P2_color3
                                call  P2_Draw_Border_Proc           ;PreviousXPosition,PreviousYPosition,color1   ;unHighlight the current cell to highlight the new one after pressing up button (drwaing above it white or grey)
                                 MemoryToMemoryMovWord P2_x,P2_CurrentXPosition
                               
                                MemoryToMemoryMovWord P2_y,P2_CurrentYPosition
                                
                                CMP P2_IS_SELECTED,1
                                JE P2_change_color_Right
                                jmp P2_skip_change_color_Right
                                P2_change_color_Right:
                              MemoryToMemoryMovWord P2_color,RED 
                              JMP P2_Dont_Change_Color_Right
                               P2_skip_change_color_Right:
                               MemoryToMemoryMovWord P2_color,AQUA
                               P2_Dont_Change_Color_Right:
                                 call P2_Draw_Border_Proc            ;CurrentXPosition,CurrentYPosition,0EH        ;Highlight the new cell with yellow color
                   
                                 exchange              P2_color1,P2_color2
                                 jmp                   P2_END_RIGHT                                       ;Because initially we are starting from cell_8_1 (its color is white which is stored in variable color2)
   P2_RIGHT_BORDER_REACHED:
   P2_END_RIGHT:                                                                             ;at any mov , regardless how many times , we will exchange grey and white stored in color1,color2
  
  MOV P2_CurrentXPosition,CX
 MOV P2_CurrentYPosition,DX
 POP DX
 POP CX
 
 RET
 P2_RIGHT_PRESSED_PROC ENDP


P2_GET_CELL_FROM_XY PROC FAR
   ;LOCAL SET_GLOBAL_CELL
   
                                 PUSH                  AX
                                 PUSH                  BX
                                 PUSH                  CX
                                 PUSH                  DX
                                 PUSH                  SI
                                 PUSH                  DI
                                 MOV                   DX,0
                                 MOV                   AX,P2_GLOBAL_ROW
                                 SUB                   AX,4                                         ;ROW-4
                                 MOV                   BX,22
                                 DIV                   BX                                           ;AX HAS (ROW-4)/22
                                 MOV                   CX,96
                                 MUL                   CX                                           ;AX HAS (ROW-4)/22 *8*10
                                 MOV                   CX,AX                                        ;CX HAS (ROW-4)/22 *8*10
                                 MOV                   DX,0
                                 MOV                   AX,P2_GLOBAL_COLUMN
                                 SUB                   AX,72                                        ;COLUMN - 72
                                 MOV                   BX,22
                                 DIV                   BX                                           ;AX HAS (COLUMN - 72)/22
                                 MOV                   BX,12
                                 MUL                   BX                                           ;AX HAS (COLUMN - 72)/22 *10
                                 ADD                   AX,CX
   ;AX HAS INDEX OF NEEDED CELL
                                 MOV                   BX,AX
                                 MOV                   SI,0
                                 MOV                   CX,6
   P2_SET_GLOBAL_CELL:              
                                 MOV                   AX,Cell_1_1[BX]
                                 MOV                   P2_GLOBAL_CELL[SI],AX
                                 ADD                   SI,2
                                 ADD                   BX,2
                                 DEC                   CX
                                 JNZ                   P2_SET_GLOBAL_CELL

                                 POP                   DI
                                 POP                   SI
                                 POP                   DX
                                 POP                   CX
                                 POP                   BX
                                 POP                   AX
                                 RET
P2_GET_CELL_FROM_XY ENDP

P2_SET_Selected_cell_Index  PROC ;ID , X,Y ,CEL
 PUSH ax
 PUSH BX
 PUSH CX
 PUSH DX
 PUSH SI
 
                                  MOV                   DX,0
                                  MOV                   AX,P2_GLOBAL_ROW
                                  SUB                   AX,4                                         ;ROW-4
                                  MOV                   BX,22
                                  DIV                   BX                                           ;AX HAS (ROW-4)/22  ,DX :REMAINDER
                                  MOV                   CX,96
                                  MUL                   CX                                           ;AX HAS (ROW-4)/22 *8*10 ;DX:MOST SIGNFICANT 
                                  MOV                   CX,AX                                        ;CX HAS (ROW-4)/22 *8*10
                                  MOV                   DX,0
                                  MOV                   AX,P2_GLOBAL_COLUMN
                                  SUB                   AX,72                                        ;COLUMN - 72
                                  MOV                   BX,22
                                  DIV                   BX                                           ;AX HAS (COLUMN - 72)/22
                                  MOV                   BX,12
                                  MUL                   BX                                           ;AX HAS (COLUMN - 72)/22 *10
                                  ADD                   AX,CX
    ;AX HAS INDEX OF NEEDED CELL
                                  MOV P2_Selected_cell_Index,AX   ;STORES INDEX OF SELECTED CELL BEFORE MOVEMENT
                                  ; EnterTextMode ;TEST MOHEM
                                  ; DisplayNumberAX 
                                  ; MovCursorToLocation 0,5
                                  ; mov ax,GLOBAL_COLUMN
                                  ; DisplayNumberAX
                                  ;  MovCursorToLocation 0,10
                                  ; mov ax,GLOBAL_ROW
                                  ; DisplayNumberAX
 
                                 
 POP SI                               
 POP DX
 POP CX
 POP BX
 POP AX  
 
 RET
   P2_SET_Selected_cell_Index ENDP


P2_SET_Is_Allowed_To_Move_Flag PROC FAR
 push si
 push di
 PUSH AX
 mov si,0   ;will hold x 
 mov di ,2  ; will hold y
 
 MOV P2_Is_Allowed_To_Move_Flag,0 
 MemoryToMemoryMovWord P2_temp_Number_Of_Stored_Cells_In_Highlighted_Cells_Array,P2_Number_Of_Stored_Highlighted_Cells
 MemoryToMemoryMovWord P2_GLOBAL_COLUMN, P2_CurrentXPosition
 MemoryToMemoryMovWord P2_GLOBAL_ROW, P2_CurrentYPosition 
  
 
  MemoryToMemoryMovWord  P2_Temp_Selected_Cell_Index,P2_Selected_Cell_Index
 
  
 ;CALL SET_Selected_cell_Index ;SELECTED_CELL_INDEX VARIABLE NOW HOLDS THE INDEX OF THE CELL I WANT TO MOVE TO
                             
 ; MOV THIS INDEX TO A REGISTER BEFOR THE LOOP -> FOR THE CMP INSTRUCTION 
 
 ;NOW GET THIS INDEX FOR EACH PAIR IN HIGHLIGHTED ARRAY ,COMPARE IT WITH TEMP INDEX, IF FOUND THEN THE MOV IS POSSIBLE
 CMP P2_Number_Of_Stored_Highlighted_Cells ,0 
 JE  P2_NOT_FOUND 
 P2_Search_for_cell_in_array_of_highlighted_cells:
 
 MemoryToMemoryMovWord P2_GLOBAL_COLUMN, P2_Array_Of_Highlighted_X_Y[si] ;starting from 0
 MemoryToMemoryMovWord P2_GLOBAL_ROW,    P2_Array_Of_Highlighted_X_Y[di] ;starting from 1
 call P2_SET_Selected_cell_Index 
 
 
 CALL P2_GET_CELL_FROM_XY
 MOV AX,P2_Selected_cell_Index

 cmp AX , P2_Temp_Selected_Cell_Index
 jne P2_SEARCH_AGAIN  
 
 JMP P2_DONT_SEARCH_AGAIN
 P2_SEARCH_AGAIN:
 ADD SI,4
 ADD DI,4 
 dec P2_temp_Number_Of_Stored_Cells_In_Highlighted_Cells_Array 
 JNZ P2_Search_for_cell_in_array_of_highlighted_cells
 JMP P2_NOT_FOUND
 
 P2_DONT_SEARCH_AGAIN:
 
 ;if equal : set the flag is found to 1
 MOV  P2_Is_Allowed_To_Move_Flag ,1 
 
 P2_NOT_FOUND:
 MemoryToMemoryMovWord P2_GLOBAL_COLUMN,P2_CurrentXPosition
 MemoryToMemoryMovWord P2_GLOBAL_ROW,   P2_CurrentYPosition
 MemoryToMemoryMovWord P2_Selected_Cell_Index,P2_Temp_Selected_Cell_Index
 CALL P2_GET_CELL_FROM_XY 
 
 POP AX
 pop di 
 pop si
 RET
 P2_SET_Is_Allowed_To_Move_Flag ENDP 



P2_CLEAR_CELL PROC ;ID , X,Y ,CELL
  push si   
   MOV SI,P2_Selected_cell_Index 
   MemoryToMemoryMovWord P2_ID_PARAMETER3, Cell_1_1[SI+8]
   MemoryToMemoryMovWord P2_FlagParameter, Cell_1_1[SI+4] ;????

 ;  NA2ES ENENA N MOV 1 LE EL FLAG BTA3 EL PLAYER , E3MLELO VARIABLE B2A ZY ID_PARAMETER***********TODO***********                           
  
   MOV Cell_1_1[SI+4],00 ;DY MESH BTETSAFAR 
   MOV Cell_1_1[SI+8],0 ;???????
   Draw_Square_Macro_X_Y_COLOR Cell_1_1[si],Cell_1_1[si+2],Cell_1_1[si+6]
   pop si

RET
   P2_CLEAR_CELL ENDP

P2_DrawPiece PROC FAR
             ;local drawLoop , white1
             push  ax
             push  cx
             push  dx
             PUSH SI
             PUSH DI 
            
                                              LEA                   BX , PData                                   ; BL contains index at the current drawn pixel
             MOV   CX,P2_GLOBAL_COLUMN
             MOV   SI,CX
             ADD   SI,Widthh
             MOV   DX,P2_GLOBAL_ROW
             MOV   DI,DX
             ADD   DI,Height
             MOV   AH,0ch
   ; Drawing loop
   P2_drawLoop: 
             MOV   AL,[BX]
             cmp   al,0fh
             jz    P2_white1
             INT   10h
   P2_white1:   
             INC   CX
             INC   BX
             CMP   CX,SI
             JNE   P2_drawLoop

             MOV   CX ,P2_GLOBAL_COLUMN
             INC   DX
             CMP   DX ,DI
             JNE   P2_drawLoop
             
             
             POP DI
             POP SI 
             pop   dx
             pop   cx
             pop   ax
             RET
P2_DrawPiece ENDP 

P2_Draw_Complete_Cell_Proc PROC FAR
  push si
  MOV SI,6
 P2_DrawSquare P2_GLOBAL_CELL[SI] 

 MOV SI,8
                                 cmp                   P2_GLOBAL_CELL[SI],0;word ptr [si+8],0                            ;Compares celll[8] : ID of the empty cell _> if empty , draw the square of the same color
                                 jz                    P2_FARJUMP_DrawEmptyCell
                                 JMP                   P2_SKIPFARJUMP_DrawEmptyCell
                                  
   P2_FARJUMP_DrawEmptyCell:        
                                 JMP                   FAR PTR P2_DrawEmptyCell
   P2_SKIPFARJUMP_DrawEmptyCell:    

                                 MemoryToMemoryMovWord ID_PARAMETER ,P2_GLOBAL_CELL[SI] ;P2_ID
                                 call                  setfilename

                                 OpenFile              GLOBAL_FILE_NAME
                                 ReadData


                                ; LEA                   BX , PData                                   ; BL contains index at the current drawn pixel
                                 MOV SI,0
                                 ;DrawPieceMacro 138,26
                                 call P2_DrawPiece          ;   GLOBAL_COLUMN,GLOBAL_ROW   ;138,26                                    ;word ptr [si],word ptr[si+2]
    

                                 CloseFile
   P2_DrawEmptyCell:                
   ; DrawSquare  word ptr [si] , word ptr [si+2] , word ptr [si+6]
                             pop si
                                 RET
P2_Draw_Complete_Cell_Proc ENDP
 P2_SET_CELL PROC FAR
 push si   
   
   CALL P2_SET_Selected_cell_Index
   MOV SI,P2_Selected_cell_Index
   MemoryToMemoryMovWord KILLEDPIECE,Cell_1_1[SI+8]   ;SET CELL 1 AND 2
   MemoryToMemoryMovWord Cell_1_1[SI+8],P2_ID_PARAMETER3 ;ID_PARAMETER2 ;UPDATES ID OF NEW CELL 
  ; MOV Cell_1_1[SI+4],1 ;UPDATE STATUS OF THIS CELL :HOLDS PIECE RELATED TO PLAYER 1 
  MemoryToMemoryMovWord Cell_1_1[SI+4],P2_FlagParameter

  MemoryToMemoryMovWord counter_for_timer_test, CURRENTTIME    
   ADD counter_for_timer_test,3                       ;NEW
   MemoryToMemoryMovWord Cell_1_1[si+10], counter_for_timer_test          ;NEW SET CELL 1 W 2

 CALL P2_GET_CELL_FROM_XY ;CHECK
 CALL P2_Draw_Complete_Cell_Proc
 
   POP SI

 RET
 P2_SET_CELL ENDP


 P2_DeHighlightCells PROC FAR

 PUSH AX
 push DI

MemoryToMemoryMovWord P2_temp_global_row,   P2_GLOBAL_ROW
MemoryToMemoryMovWord P2_temp_global_column,P2_GLOBAL_COLUMN

mov DI,0 
MOV AX ,P2_Number_Of_Stored_Highlighted_Cells
CMP AX,0

JZ P2_END_TRAVERSAL 

 P2_Traversing_array:
   ;****************************************************************

 MemoryToMemoryMovWord P2_GLOBAL_COLUMN,P2_Array_Of_Highlighted_X_Y[DI]
 add DI,2 

 MemoryToMemoryMovWord P2_GLOBAL_ROW,P2_Array_Of_Highlighted_X_Y[DI]
 add DI,2 
 ;CALL SET_Selected_cell_Index ;kda m3aya el index f selected_cell_index
 
  call P2_GET_CELL_FROM_XY
  call P2_Draw_Complete_Cell_Proc
  ;DrawSquare GLOBAL_CELL[6]
  ;****************************************************************


 dec P2_Number_Of_Stored_Highlighted_Cells 
 jnz P2_Traversing_array

P2_END_TRAVERSAL:

MemoryToMemoryMovWord P2_GLOBAL_ROW,   P2_temp_global_row
MemoryToMemoryMovWord P2_GLOBAL_COLUMN,P2_temp_global_column
 CALL P2_GET_CELL_FROM_XY


 pop DI
 POP AX
 RET
 P2_DeHighlightCells ENDP 

P2_Highlight_Possible_Rook_Moves PROC FAR
 ;*down***
   push ax
   push bx
   
   MemoryToMemoryMovWord P2_x,P2_GLOBAL_COLUMN
   MemoryToMemoryMovWord P2_y,P2_GLOBAL_ROW
     MOV AX,P2_GLOBAL_COLUMN
     MOV BX,P2_GLOBAL_ROW
  
     mov si, offset P2_Array_Of_Highlighted_X_Y 
     mov P2_Number_Of_Stored_Highlighted_Cells,0 
   P2_DOWNLOOP:
   CMP P2_Y,158
     jE P2_LOWERLIMIT1
   JMP P2_SKIP_FAR_LOWER_LIMIT
     P2_LOWERLIMIT1:
  
     jmp FAR PTR  P2_LOWERLIMIT
  P2_SKIP_FAR_LOWER_LIMIT:
    ADD P2_y,22
      MemoryToMemoryMovWord P2_GLOBAL_ROW, P2_y
     MemoryToMemoryMovWord P2_GLOBAL_COLUMN,P2_X
    CALL P2_GET_CELL_FROM_XY
  ;**************
   
  
    CMP P2_GLOBAL_CELL[4],2
    JE P2_MY_PIECE_ROOK_DOWN1
  
   
  JMP P2_SKIP_FAR_ROOK_DOWN
    P2_MY_PIECE_ROOK_DOWN1:
    
    ;**************
  JMP P2_MY_PIECE_ROOK_DOWN
  P2_SKIP_FAR_ROOK_DOWN:
  
    CMP P2_GLOBAL_CELL[4],1
    
  JE P2_ENEMY_PIECE_ROOK_DOWN
    mov P2_color,purple
   CALL P2_Draw_Border_Proc
   P2_Push_Cell_Into_Highlighted_Array ;MACRO
  CMP P2_Y, 158
   JNE P2_DOWNLOOP
  
  jmp P2_LOWERLIMIT ; 
   P2_ENEMY_PIECE_ROOK_DOWN:
  
    
    mov P2_color,black
   CALL P2_Draw_Border_Proc
  
   P2_Push_Cell_Into_Highlighted_Array
  
  P2_MY_PIECE_ROOK_DOWN:
   P2_LOWERLIMIT:
   
   MOV P2_GLOBAL_COLUMN,ax
  MOV P2_GLOBAL_ROW,BX
  
  ;******UP**********
  
   MemoryToMemoryMovWord P2_x,P2_GLOBAL_COLUMN
   MemoryToMemoryMovWord P2_y,P2_GLOBAL_ROW
     MOV AX,P2_GLOBAL_COLUMN
     MOV BX,P2_GLOBAL_ROW
  P2_UPLOOP:
     CMP P2_Y,4
     jE P2_UPPERLIMIT1 

     JMP SKIP_P2_UPPERLIMIT1
     P2_UPPERLIMIT1:
     JMP FAR PTR  P2_UPPERLIMIT
SKIP_P2_UPPERLIMIT1:
  
    SUB P2_y,22
      MemoryToMemoryMovWord P2_GLOBAL_ROW, P2_y
     MemoryToMemoryMovWord P2_GLOBAL_COLUMN,P2_X
    CALL P2_GET_CELL_FROM_XY
    CMP P2_GLOBAL_CELL[4],2
    JE P2_MY_PIECE
    CMP P2_GLOBAL_CELL[4],1
  JE P2_ENEMY_PIECE_ROOK
    mov P2_color,purple
   CALL P2_Draw_Border_Proc
   P2_Push_Cell_Into_Highlighted_Array
  CMP P2_Y,4
   JNE P2_UPLOOP
   
  JMP P2_UPPERLIMIT
   P2_ENEMY_PIECE_ROOK:
    
    mov P2_color,black
   CALL P2_Draw_Border_Proc
   P2_Push_Cell_Into_Highlighted_Array
  
  P2_MY_PIECE:
  P2_UPPERLIMIT:
  
  MOV P2_GLOBAL_COLUMN,ax
  MOV P2_GLOBAL_ROW,BX
  
  ;*******RIGHT******
  
  MemoryToMemoryMovWord P2_x,P2_GLOBAL_COLUMN
    MemoryToMemoryMovWord P2_y,P2_GLOBAL_ROW
    MOV AX,P2_GLOBAL_COLUMN
     MOV BX,P2_GLOBAL_ROW
  P2_RIGHTLOOP:
  CMP P2_X,226
     jE P2_RIGHTLIMIT1 
     JMP SKIP_P2_RIGHTLIMIT1
P2_RIGHTLIMIT1:
JMP FAR PTR P2_RIGHTLIMIT
 SKIP_P2_RIGHTLIMIT1:
  
    ADD P2_X,22
      MemoryToMemoryMovWord P2_GLOBAL_ROW, P2_y
     MemoryToMemoryMovWord P2_GLOBAL_COLUMN,P2_X
    CALL P2_GET_CELL_FROM_XY
    CMP P2_GLOBAL_CELL[4],2
    JE P2_MY_PIECE_ROOK_RIGHT
    CMP P2_GLOBAL_CELL[4],1
  JE P2_ENEMY_PIECE_ROOK_RIGHT
    mov P2_color,purple
   CALL P2_Draw_Border_Proc
  P2_Push_Cell_Into_Highlighted_Array
  
  
  CMP P2_X,226
   JNE P2_RIGHTLOOP
   JMP P2_RIGHTLIMIT
   P2_ENEMY_PIECE_ROOK_RIGHT:
  
    mov P2_color,black
   CALL P2_Draw_Border_Proc
  P2_Push_Cell_Into_Highlighted_Array
  
  
  P2_MY_PIECE_ROOK_RIGHT:
   P2_RIGHTLIMIT:
  MOV P2_GLOBAL_COLUMN,ax
  MOV P2_GLOBAL_ROW,BX
  
  ;*********LEFT******
  MemoryToMemoryMovWord P2_x,P2_GLOBAL_COLUMN
    MemoryToMemoryMovWord P2_y,P2_GLOBAL_ROW
     MOV AX,P2_GLOBAL_COLUMN
     MOV BX,P2_GLOBAL_ROW
  P2_LEFTLOOP:
  CMP P2_X,72
     jE P2_LEFTLIMIT1 

JMP SKIP_P2_LEFTLIMIT1
  P2_LEFTLIMIT1:

  JMP FAR PTR P2_LEFTLIMIT
   SKIP_P2_LEFTLIMIT1:
    SUB P2_X,22
      MemoryToMemoryMovWord P2_GLOBAL_ROW, P2_y
     MemoryToMemoryMovWord P2_GLOBAL_COLUMN,P2_X
    CALL P2_GET_CELL_FROM_XY
    CMP P2_GLOBAL_CELL[4],2
    JE P2_MY_PIECE_ROOK_LEFT
    CMP P2_GLOBAL_CELL[4],1
  JE P2_ENEMY_PIECE_ROOK_LEFT
    mov P2_color,purple 
   CALL P2_Draw_Border_Proc
   P2_Push_Cell_Into_Highlighted_Array
  CMP P2_X,72
   JNE P2_LEFTLOOP
   JMP P2_LEFTLIMIT
   P2_ENEMY_PIECE_ROOK_LEFT:
    mov P2_color,black
   CALL P2_Draw_Border_Proc
   P2_Push_Cell_Into_Highlighted_Array
  P2_MY_PIECE_ROOK_LEFT:
   P2_LEFTLIMIT:
  MOV P2_GLOBAL_COLUMN,ax
  MOV P2_GLOBAL_ROW,BX
  call P2_GET_CELL_FROM_XY
  
  
  
  
  
  ;pop si
  ;POP DX
  pop bx
  pop ax
  
   Ret
  P2_Highlight_Possible_Rook_Moves ENDP


P2_Highlight_Possible_Bishop_Moves PROC FAR
;****down********
 push ax
 push bx
 ;push si
 ;mov si, offset Array_Of_Highlighted_X_Y
    ;mov Number_Of_Stored_Highlighted_Cells,0 ;this should be generalized in mov in p1 pressed q not here

  MemoryToMemoryMovWord P2_x,P2_GLOBAL_COLUMN
  MemoryToMemoryMovWord P2_y,P2_GLOBAL_ROW
   MOV AX,P2_GLOBAL_COLUMN
   MOV BX,P2_GLOBAL_ROW
   P2_DOWNLEFTLOOP:
   CMP P2_Y,158
   jE  P2_LOWERLEFTLIMIT1
   CMP P2_X,72
   jE  P2_LOWERLEFTLIMIT1
   JMP P2_SKIP_FAR_LOWER_LEFT_LIMIT
   P2_LOWERLEFTLIMIT1:


   jmp FAR PTR P2_LOWERLEFTLIMIT
    P2_SKIP_FAR_LOWER_LEFT_LIMIT:
  ADD P2_y,22
  SUB P2_X,22
    MemoryToMemoryMovWord P2_GLOBAL_ROW,   P2_y
    MemoryToMemoryMovWord  P2_GLOBAL_COLUMN,P2_X
  CALL P2_GET_CELL_FROM_XY

  
  CMP P2_GLOBAL_CELL[4],2
  JE  P2_MY_PIECE_Bishop_DOWNLEFT
  CMP P2_GLOBAL_CELL[4],1
  
  JE    P2_ENEMY_PIECE_Bishop_DOWNLEFT
  ;CMP P2_GLOBAL_CELL[4],2
  
;JA    P2_ENEMY_PIECE_Bishop_DOWNLEFT
  mov P2_color,PURPLE
 CALL P2_Draw_Border_Proc 
    P2_Push_Cell_Into_Highlighted_Array
CMP P2_Y,158
 JNE P2_DOWNLEFTLOOP


jmp  P2_LOWERLEFTLIMIT ; dy ana zwdtha 3shn kda ht5ls loop fo2 w tnzl tnafez code el enemy w howa mfesh enemy aslan 
     P2_ENEMY_PIECE_Bishop_DOWNLEFT:


 mov   P2_color,BLACK
 CALL  P2_Draw_Border_Proc
       P2_Push_Cell_Into_Highlighted_Array
       P2_MY_PIECE_Bishop_DOWNLEFT:
       P2_LOWERLEFTLIMIT:

 MOV    P2_GLOBAL_COLUMN,ax
 MOV    P2_GLOBAL_ROW,BX       


;  ;****UP********
 MemoryToMemoryMovWord P2_x,P2_GLOBAL_COLUMN
 MemoryToMemoryMovWord P2_y,P2_GLOBAL_ROW
   MOV AX,P2_GLOBAL_COLUMN
   MOV BX,P2_GLOBAL_ROW
P2_UPLEFTLOOP:
   CMP P2_Y,4
   jE P2_UPPERLEFTLIMIT1
   CMP P2_X,72
   jE P2_UPPERLEFTLIMIT1
  SUB P2_y,22
  SUB P2_X,22
    MemoryToMemoryMovWord P2_GLOBAL_ROW,   P2_y
    MemoryToMemoryMovWord  P2_GLOBAL_COLUMN,P2_X
  CALL P2_GET_CELL_FROM_XY
  CMP  P2_GLOBAL_CELL[4],2
  JE   P2_MY_PIECE_LEFT_BISHOP
  CMP  P2_GLOBAL_CELL[4],1
 JE     P2_ENEMY_PIECE_LEFT_Bishop
  mov  P2_color,PURPLE
 CALL  P2_Draw_Border_Proc
 P2_Push_Cell_Into_Highlighted_Array
 CMP P2_Y,4
 JNE P2_UPLEFTLOOP
     P2_UPPERLEFTLIMIT1:
 JMP  P2_UPPERLEFTLIMIT
     P2_ENEMY_PIECE_LEFT_Bishop:
 mov  P2_color,BLACK
 CALL P2_Draw_Border_Proc
      P2_Push_Cell_Into_Highlighted_Array
      P2_MY_PIECE_LEFT_BISHOP:
      P2_UPPERLEFTLIMIT:

 MOV   P2_GLOBAL_COLUMN,ax
 MOV   P2_GLOBAL_ROW,BX

; ;****RIGHT********

  MemoryToMemoryMovWord   P2_x,p2_GLOBAL_COLUMN
  MemoryToMemoryMovWord P2_y,p2_GLOBAL_ROW
  MOV AX, p2_GLOBAL_COLUMN
  MOV BX, p2_GLOBAL_ROW
  p2_UPRIGHTLOOP:
  CMP   P2_X,226
   jE   P2_UPRIGHTLIMIT1
   CMP  P2_Y,4
   jE   P2_UPRIGHTLIMIT1
   ADD  P2_X,22
   SUB  P2_Y,22
   MemoryToMemoryMovWord  P2_GLOBAL_ROW,   P2_y
   MemoryToMemoryMovWord  P2_GLOBAL_COLUMN,P2_X
  CALL P2_GET_CELL_FROM_XY
  CMP  P2_GLOBAL_CELL[4],2
  JE   P2_MY_PIECE_Bishop_UPRIGHT
  CMP  P2_GLOBAL_CELL[4],1
JE    P2_ENEMY_PIECE_Bishop_UPRIGHT
  mov  P2_color,PURPLE
 CALL  P2_Draw_Border_Proc
       P2_Push_Cell_Into_Highlighted_Array
CMP  P2_X,226
 JNE P2_UPRIGHTLOOP
     P2_UPRIGHTLIMIT1:
 JMP P2_UPRIGHTLIMIT
     P2_ENEMY_PIECE_Bishop_UPRIGHT:

 mov  P2_color,BLACK
 CALL P2_Draw_Border_Proc
 P2_Push_Cell_Into_Highlighted_Array
    P2_MY_PIECE_Bishop_UPRIGHT:
    P2_UPRIGHTLIMIT:
 MOV P2_GLOBAL_COLUMN,ax
 MOV P2_GLOBAL_ROW,BX

; ;****left********
  MemoryToMemoryMovWord P2_x,P2_GLOBAL_COLUMN
  MemoryToMemoryMovWord P2_y,P2_GLOBAL_ROW
  MOV AX, P2_GLOBAL_COLUMN
   MOV BX,P2_GLOBAL_ROW
     P2_DOWNRIGHTLOOP:
CMP P2_X,226
   jE P2_DOWNRIGHTLIMIT1
   CMP P2_Y,158
   jE P2_DOWNRIGHTLIMIT1
  ADD P2_X,22
  ADD P2_Y,22
    MemoryToMemoryMovWord P2_GLOBAL_ROW, P2_y
    MemoryToMemoryMovWord P2_GLOBAL_COLUMN,P2_X
  CALL P2_GET_CELL_FROM_XY
  CMP  P2_GLOBAL_CELL[4],2
  JE   P2_MY_PIECE_Bishop_DOWNRIGHT
  CMP  P2_GLOBAL_CELL[4],1
 JE     P2_ENEMY_PIECE_Bishop_DOWNRIGHT
  mov   P2_color,PURPLE
 CALL P2_Draw_Border_Proc
      P2_Push_Cell_Into_Highlighted_Array
 CMP   P2_X,226
 JNE P2_DOWNRIGHTLOOP
     P2_DOWNRIGHTLIMIT1:
 JMP P2_DOWNRIGHTLIMIT
 P2_ENEMY_PIECE_Bishop_DOWNRIGHT:

  mov P2_color,BLACK
 CALL P2_Draw_Border_Proc
      P2_Push_Cell_Into_Highlighted_Array
      P2_MY_PIECE_Bishop_DOWNRIGHT:
      P2_DOWNRIGHTLIMIT:
MOV   P2_GLOBAL_COLUMN,ax
MOV   P2_GLOBAL_ROW,BX
call  P2_GET_CELL_FROM_XY
; EnterTextMode ;TEST MOHEM\  
;                                 PUSH AX
;                                 MOV AX,P2_Number_Of_Stored_Highlighted_Cells
;                                  DisplayNumberAX 
;                                  POP AX
                                ;  MovCursorToLocation 0,5
                                ;  mov ax,GLOBAL_COLUMN
                                ;  DisplayNumberAX
                                ;   MovCursorToLocation 0,10
                                ;  mov ax,GLOBAL_ROW
                                ;  DisplayNumberAX
pop bx
pop ax

 Ret
 P2_Highlight_Possible_Bishop_Moves ENDP


P2_Highlight_Possible_Queen_Moves PROC FAR
 
  call P2_Highlight_Possible_Rook_Moves
  call P2_Highlight_Possible_Bishop_Moves
  
  RET
  P2_Highlight_Possible_Queen_Moves ENDP

 P2_Highlight_Possible_Knight_Moves PROC FAR
 PUSH AX
 PUSH BX
 PUSH SI 
   
    mov si, offset P2_Array_Of_Highlighted_X_Y
    mov P2_Number_Of_Stored_Highlighted_Cells,0 ;this should be generalized in mov in p1 pressed q not here
    
  ;*******UPPERLEFT************
  
   P2_UPPERLEFT_KNIGHT_MOVES:
   MemoryToMemoryMovWord P2_x,P2_GLOBAL_COLUMN
   MemoryToMemoryMovWord P2_y,P2_GLOBAL_ROW
    MOV AX,P2_GLOBAL_COLUMN
    MOV BX,P2_GLOBAL_ROW
  cmp P2_y,4
  jz P2_SKIP_UPPERLEFT_KNIGHT_MOVES
  cmp P2_y,26
  jz P2_SKIP_UPPERLEFT_KNIGHT_MOVES
  cmp P2_x,72
  jz P2_SKIP_UPPERLEFT_KNIGHT_MOVES
   sub P2_y,44
   SUB P2_X,22
   MemoryToMemoryMovWord P2_GLOBAL_ROW, P2_y
   MemoryToMemoryMovWord P2_GLOBAL_COLUMN,P2_X
   
   CALL P2_GET_CELL_FROM_XY
   CMP  P2_GLOBAL_CELL[4],2
   JE   P2_MY_PIECE_KNIGHT_UPLEFT
   mov  P2_color,PURPLE
  CALL P2_Draw_Border_Proc 
  P2_Push_Cell_Into_Highlighted_Array
 
 P2_SKIP_UPPERLEFT_KNIGHT_MOVES:
 P2_MY_PIECE_KNIGHT_UPLEFT:
  MOV P2_GLOBAL_COLUMN,ax
 MOV P2_GLOBAL_ROW,BX     
 
 ;*******UPPERRIGHT************
 
  P2_UPPERRIGHT_KNIGHT_MOVES:
   MemoryToMemoryMovWord P2_x,P2_GLOBAL_COLUMN
  MemoryToMemoryMovWord P2_y, P2_GLOBAL_ROW
    MOV AX,P2_GLOBAL_COLUMN
    MOV BX,P2_GLOBAL_ROW
  cmp P2_y,4
  jz P2_SKIP_UPPERRIGHT_KNIGHT_MOVES
  cmp P2_y,26
  jz P2_SKIP_UPPERRIGHT_KNIGHT_MOVES
  cmp P2_X,226
  jz P2_SKIP_UPPERRIGHT_KNIGHT_MOVES
   sub P2_y,44
   ADD P2_x,22
   MemoryToMemoryMovWord P2_GLOBAL_ROW, P2_y
   MemoryToMemoryMovWord P2_GLOBAL_COLUMN,P2_X
   CALL P2_GET_CELL_FROM_XY
  CMP P2_GLOBAL_CELL[4],2
   JE P2_MY_PIECE_KNIGHT_UPRIGHT
   mov P2_color,PURPLE
  CALL P2_Draw_Border_Proc 
  P2_Push_Cell_Into_Highlighted_Array
 
 P2_SKIP_UPPERRIGHT_KNIGHT_MOVES:
 P2_MY_PIECE_KNIGHT_UPRIGHT:
 
 MOV P2_GLOBAL_COLUMN,ax
 MOV P2_GLOBAL_ROW,BX
 
 ;*******DOWNLEFT************
 P2_DOWNLEFT_KNIGHT_MOVES:
 MemoryToMemoryMovWord P2_x,P2_GLOBAL_COLUMN
  MemoryToMemoryMovWord P2_y,P2_GLOBAL_ROW
    MOV AX,P2_GLOBAL_COLUMN
    MOV BX,P2_GLOBAL_ROW
  cmp P2_y,158
  jz P2_SKIP_DOWNLEFT_KNIGHT_MOVES
  cmp P2_y,136
  jz P2_SKIP_DOWNLEFT_KNIGHT_MOVES
  cmp P2_x,72
  jz P2_SKIP_DOWNLEFT_KNIGHT_MOVES
   ADD P2_y,44
   SUB P2_X,22
   MemoryToMemoryMovWord P2_GLOBAL_ROW,   P2_y
   MemoryToMemoryMovWord P2_GLOBAL_COLUMN,P2_X
   
   CALL P2_GET_CELL_FROM_XY
   CMP  P2_GLOBAL_CELL[4],2
   JE  P2_MY_PIECE_KNIGHT_DOWNLEFT
   mov P2_color,PURPLE
  CALL P2_Draw_Border_Proc 
  P2_Push_Cell_Into_Highlighted_Array
 
 P2_SKIP_DOWNLEFT_KNIGHT_MOVES:
 P2_MY_PIECE_KNIGHT_DOWNLEFT:
  MOV P2_GLOBAL_COLUMN,ax
 MOV  P2_GLOBAL_ROW,BX    
 
 ;*******DOWNRIGHT************
 P2_DOWNRIGHT_KNIGHT_MOVES:
 MemoryToMemoryMovWord  P2_x,P2_GLOBAL_COLUMN
  MemoryToMemoryMovWord P2_y,P2_GLOBAL_ROW
    MOV AX,P2_GLOBAL_COLUMN
    MOV BX,P2_GLOBAL_ROW
  cmp P2_y,158
  jz P2_SKIP_DOWNRIGHT_KNIGHT_MOVES
  cmp P2_y,136
  jz P2_SKIP_DOWNRIGHT_KNIGHT_MOVES
  cmp P2_x,226
  jz P2_SKIP_DOWNRIGHT_KNIGHT_MOVES
   ADD P2_y,44
   ADD P2_X,22
   MemoryToMemoryMovWord P2_GLOBAL_ROW, P2_y
   MemoryToMemoryMovWord P2_GLOBAL_COLUMN,P2_X
   
   CALL P2_GET_CELL_FROM_XY
   CMP  P2_GLOBAL_CELL[4],2
   JE  P2_MY_PIECE_KNIGHT_DOWNRIGHT
   mov P2_color,purple
  CALL P2_Draw_Border_Proc 
  P2_Push_Cell_Into_Highlighted_Array
 
 P2_SKIP_DOWNRIGHT_KNIGHT_MOVES:
 P2_MY_PIECE_KNIGHT_DOWNRIGHT:
  MOV P2_GLOBAL_COLUMN,ax
 MOV  P2_GLOBAL_ROW,BX    
 
 ;*******RIGHTDOWN************
 P2_RIGHTDOWN_KNIGHT_MOVES:
 MemoryToMemoryMovWord  P2_x,P2_GLOBAL_COLUMN
  MemoryToMemoryMovWord P2_y,P2_GLOBAL_ROW
    MOV AX,P2_GLOBAL_COLUMN
    MOV BX,P2_GLOBAL_ROW
  cmp P2_x,226
  jz P2_SKIP_RIGHTDOWN_KNIGHT_MOVES
  cmp P2_x,204
  jz P2_SKIP_RIGHTDOWN_KNIGHT_MOVES
  cmp P2_y,158
  jz P2_SKIP_RIGHTDOWN_KNIGHT_MOVES
   add P2_x,44
   ADD P2_y,22
   MemoryToMemoryMovWord P2_GLOBAL_ROW, P2_y
   MemoryToMemoryMovWord P2_GLOBAL_COLUMN,P2_X
   
   CALL P2_GET_CELL_FROM_XY
   CMP  P2_GLOBAL_CELL[4],2
   JE   P2_MY_PIECE_KNIGHT_RIGHTDOWN
   mov  P2_color,PURPLE
  CALL  P2_Draw_Border_Proc 
        P2_Push_Cell_Into_Highlighted_Array
 
      P2_SKIP_RIGHTDOWN_KNIGHT_MOVES:
      P2_MY_PIECE_KNIGHT_RIGHTDOWN:
  MOV P2_GLOBAL_COLUMN,ax
 MOV  P2_GLOBAL_ROW,BX     
 
 ;*******RIGHTUP************
 P2_RIGHTUP_KNIGHT_MOVES:
 MemoryToMemoryMovWord  P2_x,P2_GLOBAL_COLUMN
  MemoryToMemoryMovWord P2_y,P2_GLOBAL_ROW
    MOV AX,P2_GLOBAL_COLUMN
    MOV BX,P2_GLOBAL_ROW
  cmp P2_x,226
  jz P2_SKIP_RIGHTUP_KNIGHT_MOVES
  cmp P2_x,204
  jz P2_SKIP_RIGHTUP_KNIGHT_MOVES
  cmp P2_y,4
  jz P2_SKIP_RIGHTUP_KNIGHT_MOVES
   add P2_x,44
   SUB P2_y,22
   MemoryToMemoryMovWord P2_GLOBAL_ROW, P2_y
   MemoryToMemoryMovWord P2_GLOBAL_COLUMN,P2_X
   
   CALL P2_GET_CELL_FROM_XY
   CMP  P2_GLOBAL_CELL[4],2
   JE   P2_MY_PIECE_KNIGHT_RIGHTUP
   mov  P2_color,PURPLE
  CALL  P2_Draw_Border_Proc 
        P2_Push_Cell_Into_Highlighted_Array
 
 P2_SKIP_RIGHTUP_KNIGHT_MOVES:
 P2_MY_PIECE_KNIGHT_RIGHTUP:
  MOV P2_GLOBAL_COLUMN,ax
 MOV P2_GLOBAL_ROW,BX     
 
 ;*******LEFTDOWN************
 P2_LEFTDOWN_KNIGHT_MOVES:
 MemoryToMemoryMovWord  P2_x,P2_GLOBAL_COLUMN
  MemoryToMemoryMovWord P2_y,P2_GLOBAL_ROW
    MOV AX,P2_GLOBAL_COLUMN
    MOV BX,P2_GLOBAL_ROW
  cmp P2_x,72
  jz P2_SKIP_LEFTDOWN_KNIGHT_MOVES
  cmp P2_x,94
  jz P2_SKIP_LEFTDOWN_KNIGHT_MOVES
  cmp P2_y,158
  jz P2_SKIP_LEFTDOWN_KNIGHT_MOVES
   SUB P2_x,44
   ADD P2_y,22
   MemoryToMemoryMovWord P2_GLOBAL_ROW,   P2_y
   MemoryToMemoryMovWord P2_GLOBAL_COLUMN,P2_X
   
   CALL P2_GET_CELL_FROM_XY
   CMP  P2_GLOBAL_CELL[4],2
   JE   P2_MY_PIECE_KNIGHT_LEFTDOWN
   mov  P2_color,PURPLE
  CALL  P2_Draw_Border_Proc 
        P2_Push_Cell_Into_Highlighted_Array
 
 P2_SKIP_LEFTDOWN_KNIGHT_MOVES:
 P2_MY_PIECE_KNIGHT_LEFTDOWN:
  MOV P2_GLOBAL_COLUMN,ax
  MOV P2_GLOBAL_ROW,BX     
 
 ;*******LEFTUP************
 P2_LEFTUP_KNIGHT_MOVES:
 MemoryToMemoryMovWord  P2_x,P2_GLOBAL_COLUMN
  MemoryToMemoryMovWord P2_y,P2_GLOBAL_ROW
    MOV AX,P2_GLOBAL_COLUMN
    MOV BX,P2_GLOBAL_ROW
  cmp P2_x,72
  jz P2_SKIP_LEFTUP_KNIGHT_MOVES
  cmp P2_x,94
  jz P2_SKIP_LEFTUP_KNIGHT_MOVES
  cmp P2_y,4
  jz P2_SKIP_LEFTUP_KNIGHT_MOVES
   SUB P2_x,44
   sub P2_y,22
   MemoryToMemoryMovWord P2_GLOBAL_ROW,   P2_y
   MemoryToMemoryMovWord P2_GLOBAL_COLUMN,P2_X
   
   CALL P2_GET_CELL_FROM_XY
   CMP  P2_GLOBAL_CELL[4],2
   JE   P2_MY_PIECE_KNIGHT_LEFTUP
   mov  P2_color,PURPLE
  CALL  P2_Draw_Border_Proc 
        P2_Push_Cell_Into_Highlighted_Array
 
 P2_SKIP_LEFTUP_KNIGHT_MOVES:
 P2_MY_PIECE_KNIGHT_LEFTUP:
  MOV P2_GLOBAL_COLUMN,ax
 MOV  P2_GLOBAL_ROW,BX   
 CALL P2_GET_CELL_FROM_XY  
 ;*********************
 pop si
 pop bx
 pop ax
 
 ret
 P2_Highlight_Possible_Knight_Moves ENDP

 P2_Highlight_Possible_King_Moves PROC FAR
 PUSH AX
 PUSH BX
 PUSH SI
   
   
  MOV P2_GLOBAL_COLUMN,ax
  MOV P2_GLOBAL_ROW,BX     
 ;***
    mov si, offset P2_Array_Of_Highlighted_X_Y  
    mov P2_Number_Of_Stored_Highlighted_Cells,0 ;this should be generalized in mov in p1 pressed q not here
    
  ;*******UP************
  
   P2_UP_King_Moves:
   MemoryToMemoryMovWord P2_x,P2_GLOBAL_COLUMN
   MemoryToMemoryMovWord P2_y,P2_GLOBAL_ROW
    MOV AX,P2_GLOBAL_COLUMN
    MOV BX,P2_GLOBAL_ROW
  cmp P2_y,4
  jz P2_SKIP_UP_KING_MOVES
   sub P2_y,22
   MemoryToMemoryMovWord P2_GLOBAL_ROW, P2_y
   MemoryToMemoryMovWord P2_GLOBAL_COLUMN,P2_X
   
   CALL P2_GET_CELL_FROM_XY
   CMP P2_GLOBAL_CELL[4],2
   JE P2_MY_PIECE_KING_UP
   mov P2_color,PURPLE
  CALL P2_Draw_Border_Proc 
  P2_Push_Cell_Into_Highlighted_Array
 
 P2_SKIP_UP_KING_MOVES:
 P2_MY_PIECE_KING_UP:
  MOV P2_GLOBAL_COLUMN,ax
 MOV P2_GLOBAL_ROW,BX     
 
 ;*******DOWN************
  
   P2_DOWN_King_Moves:
   MemoryToMemoryMovWord P2_x,P2_GLOBAL_COLUMN
   MemoryToMemoryMovWord P2_y,P2_GLOBAL_ROW
    MOV AX,P2_GLOBAL_COLUMN
    MOV BX,P2_GLOBAL_ROW
  cmp P2_y,158
  jz P2_SKIP_DOWN_KING_MOVES
   add P2_y,22
   MemoryToMemoryMovWord P2_GLOBAL_ROW, P2_y
   MemoryToMemoryMovWord P2_GLOBAL_COLUMN,P2_X
   
   CALL P2_GET_CELL_FROM_XY
   CMP P2_GLOBAL_CELL[4],2
   JE P2_MY_PIECE_KING_DOWN
   mov P2_color,PURPLE
  CALL P2_Draw_Border_Proc 
  P2_Push_Cell_Into_Highlighted_Array
 
 P2_SKIP_DOWN_KING_MOVES:
 P2_MY_PIECE_KING_DOWN:
  MOV P2_GLOBAL_COLUMN,ax
 MOV P2_GLOBAL_ROW,BX     
 
 ;*******UPPERRIGHT************
 
  P2_UPPERRIGHT_KING_MOVES:
   MemoryToMemoryMovWord P2_x,P2_GLOBAL_COLUMN
  MemoryToMemoryMovWord P2_y,P2_GLOBAL_ROW
    MOV AX,P2_GLOBAL_COLUMN
    MOV BX,P2_GLOBAL_ROW
  cmp P2_y,4
  jz P2_SKIP_UPPERRIGHT_KING_MOVES
  cmp P2_X,226
  jz P2_SKIP_UPPERRIGHT_KING_MOVES
   sub P2_y,22
   ADD P2_x,22
   MemoryToMemoryMovWord P2_GLOBAL_ROW, P2_y
   MemoryToMemoryMovWord P2_GLOBAL_COLUMN,P2_X
   CALL P2_GET_CELL_FROM_XY
  CMP P2_GLOBAL_CELL[4],2
   JE P2_MY_PIECE_KING_UPRIGHT
   mov P2_color,PURPLE
  CALL P2_Draw_Border_Proc 
  P2_Push_Cell_Into_Highlighted_Array
 
 P2_SKIP_UPPERRIGHT_KING_MOVES:
 P2_MY_PIECE_KING_UPRIGHT:
 
 MOV P2_GLOBAL_COLUMN,ax
 MOV P2_GLOBAL_ROW,BX
 
 ;*******UPPERLEFT************
  
   P2_UPPERLEFT_KING_MOVES:
   MemoryToMemoryMovWord P2_x,P2_GLOBAL_COLUMN
   MemoryToMemoryMovWord P2_y,P2_GLOBAL_ROW
    MOV AX,P2_GLOBAL_COLUMN
    MOV BX,P2_GLOBAL_ROW
  cmp P2_y,4
  jz P2_SKIP_UPPERLEFT_KING_MOVES
  cmp P2_x,72
  jz P2_SKIP_UPPERLEFT_KING_MOVES
   sub P2_y,22
   SUB P2_X,22
   MemoryToMemoryMovWord P2_GLOBAL_ROW, P2_y
   MemoryToMemoryMovWord P2_GLOBAL_COLUMN,P2_X
   
   CALL P2_GET_CELL_FROM_XY
   CMP P2_GLOBAL_CELL[4],2
   JE P2_MY_PIECE_KING_UPLEFT
   mov P2_color,PURPLE
  CALL P2_Draw_Border_Proc 
  P2_Push_Cell_Into_Highlighted_Array
 
 P2_SKIP_UPPERLEFT_KING_MOVES:
 P2_MY_PIECE_KING_UPLEFT:
  MOV P2_GLOBAL_COLUMN,ax
 MOV P2_GLOBAL_ROW,BX     
 ;*******DOWNLEFT************
 P2_DOWNLEFT_KING_MOVES:
 MemoryToMemoryMovWord P2_x,P2_GLOBAL_COLUMN
  MemoryToMemoryMovWord P2_y,P2_GLOBAL_ROW
    MOV AX,P2_GLOBAL_COLUMN
    MOV BX,P2_GLOBAL_ROW
  cmp P2_y,158
  jz P2_SKIP_DOWNLEFT_KING_MOVES
  cmp P2_x,72
  jz P2_SKIP_DOWNLEFT_KING_MOVES
   ADD P2_y,22
   SUB P2_X,22
   MemoryToMemoryMovWord P2_GLOBAL_ROW, P2_y
   MemoryToMemoryMovWord P2_GLOBAL_COLUMN,P2_X
   
   CALL P2_GET_CELL_FROM_XY
   CMP P2_GLOBAL_CELL[4],2
   JE P2_MY_PIECE_KING_DOWNLEFT
   mov P2_color,PURPLE
  CALL P2_Draw_Border_Proc 
  P2_Push_Cell_Into_Highlighted_Array
 
 P2_SKIP_DOWNLEFT_KING_MOVES:
 P2_MY_PIECE_KING_DOWNLEFT:
  MOV P2_GLOBAL_COLUMN,ax
 MOV P2_GLOBAL_ROW,BX    
 
 ;*******DOWNRIGHT************
 P2_DOWNRIGHT_KING_MOVES:
 MemoryToMemoryMovWord P2_x,P2_GLOBAL_COLUMN
  MemoryToMemoryMovWord P2_y,P2_GLOBAL_ROW
    MOV AX,P2_GLOBAL_COLUMN
    MOV BX,P2_GLOBAL_ROW
  cmp P2_y,158
  jz P2_SKIP_DOWNRIGHT_KING_MOVES
  cmp P2_x,226
  jz P2_SKIP_DOWNRIGHT_KING_MOVES
   ADD P2_y,22
   ADD P2_X,22
   MemoryToMemoryMovWord P2_GLOBAL_ROW, P2_y
   MemoryToMemoryMovWord P2_GLOBAL_COLUMN,P2_X
   
   CALL P2_GET_CELL_FROM_XY
   CMP P2_GLOBAL_CELL[4],2
   JE P2_MY_PIECE_KING_DOWNRIGHT
   mov P2_color,PURPLE
  CALL P2_Draw_Border_Proc 
  P2_Push_Cell_Into_Highlighted_Array
 
 P2_SKIP_DOWNRIGHT_KING_MOVES:
 P2_MY_PIECE_KING_DOWNRIGHT:
  MOV P2_GLOBAL_COLUMN,ax
 MOV P2_GLOBAL_ROW,BX    
 
 ;*******RIGHT************
 P2_RIGHT_KING_MOVES:
 MemoryToMemoryMovWord P2_x,P2_GLOBAL_COLUMN
  MemoryToMemoryMovWord P2_y,P2_GLOBAL_ROW
    MOV AX,P2_GLOBAL_COLUMN
    MOV BX,P2_GLOBAL_ROW
  cmp P2_x,226
  jz P2_SKIP_RIGHT_KING_MOVES
 
   add P2_x,22
   MemoryToMemoryMovWord P2_GLOBAL_ROW, P2_y
   MemoryToMemoryMovWord P2_GLOBAL_COLUMN,P2_X
   
   CALL P2_GET_CELL_FROM_XY
   CMP P2_GLOBAL_CELL[4],2
   JE P2_MY_PIECE_KING_RIGHT
   mov P2_color,PURPLE
  CALL P2_Draw_Border_Proc 
  P2_Push_Cell_Into_Highlighted_Array
 
 P2_SKIP_RIGHT_KING_MOVES:
 P2_MY_PIECE_KING_RIGHT:
  MOV P2_GLOBAL_COLUMN,ax
 MOV P2_GLOBAL_ROW,BX     
 
 ;*******LEFT************
 P2_LEFT_KING_MOVES:
 MemoryToMemoryMovWord P2_x,P2_GLOBAL_COLUMN
  MemoryToMemoryMovWord P2_y,P2_GLOBAL_ROW
    MOV AX,P2_GLOBAL_COLUMN
    MOV BX,P2_GLOBAL_ROW
  cmp P2_x,72
  jz P2_SKIP_LEFT_KING_MOVES
 
   SUB P2_x,22
   MemoryToMemoryMovWord P2_GLOBAL_ROW, P2_y
   MemoryToMemoryMovWord P2_GLOBAL_COLUMN,P2_X
   
   CALL P2_GET_CELL_FROM_XY
   CMP P2_GLOBAL_CELL[4],2
   JE P2_MY_PIECE_KING_LEFT
   mov P2_color,PURPLE
  CALL P2_Draw_Border_Proc 
  P2_Push_Cell_Into_Highlighted_Array
 
 P2_SKIP_LEFT_KING_MOVES:
 P2_MY_PIECE_KING_LEFT:
  MOV P2_GLOBAL_COLUMN,ax
 MOV P2_GLOBAL_ROW,BX     
 call P2_GET_CELL_FROM_XY
  ;*********************
 pop si
 pop bx
 pop ax
 
 ret
 P2_Highlight_Possible_King_Moves ENDP




P2_Highlight_Possible_Pawn_Moves PROC FAR
 PUSH AX
 PUSH BX
 PUSH SI
 
    
 ;*
    mov si, offset P2_Array_Of_Highlighted_X_Y
    mov P2_Number_Of_Stored_Highlighted_Cells,0 ;this should be generalized in mov in p1 pressed q not here
    
 P2_Forward_Pawn_Moves:
   MemoryToMemoryMovWord P2_x,P2_GLOBAL_COLUMN
   MemoryToMemoryMovWord P2_y,P2_GLOBAL_ROW
    MOV AX,P2_GLOBAL_COLUMN
    MOV BX,P2_GLOBAL_ROW
   
    call P2_GET_CELL_FROM_XY
    
   cmp P2_GLOBAL_CELL[2],136
   je P2_First_Pawn_Move1
   jmp P2_skip_First_Pawn_Move1
   P2_First_Pawn_Move1:
 jmp P2_First_Pawn_Move
 P2_skip_First_Pawn_Move1:
 
 
   P2_NOT_FIRST_PAWN_MOVE:
   CMP P2_Y,4
   JE P2_FAR_SKIP_PAWN_MOVEEEE
   JMP P2_SKIP_FAR_SKIP_PAWN_MOVEEEE
   P2_FAR_SKIP_PAWN_MOVEEEE:
   JMP FAR PTR P2_SKIP_PAWN_MOVE
   P2_SKIP_FAR_SKIP_PAWN_MOVEEEE:
   sub P2_y,22
 MemoryToMemoryMovWord P2_GLOBAL_ROW, P2_y
 MemoryToMemoryMovWord P2_GLOBAL_COLUMN,P2_X
 call P2_GET_CELL_FROM_XY
  CMP P2_GLOBAL_CELL[4],2
    JE P2_FARRRRR_SKIP_PAWN_MOVE
    JMP P2_SKIP_FARRRRR_SKIP_PAWN_MOVE
    P2_FARRRRR_SKIP_PAWN_MOVE:
    JMP P2_Enemy_Pawn_Possible_Move ;P2_SKIP_PAWN_MOVE
    P2_SKIP_FARRRRR_SKIP_PAWN_MOVE:
 CMP P2_GLOBAL_CELL[4],1
 JE P2_FAR_SKIP_PAWN_MOVEE    ; AS ITS AN ENEMY PIECE
 JMP P2_SKIP_FAR_SKIP_PAWN_MOVE
 P2_FAR_SKIP_PAWN_MOVEE:
 MOV P2_GLOBAL_COLUMN,ax
 MOV P2_GLOBAL_ROW,BX  
 JMP FAR PTR P2_Enemy_Pawn_Possible_Move
 P2_SKIP_FAR_SKIP_PAWN_MOVE:
 mov P2_color,purple
 CALL P2_Draw_Border_Proc 
 P2_Push_Cell_Into_Highlighted_Array
 MOV P2_GLOBAL_COLUMN,ax
 MOV P2_GLOBAL_ROW,BX  
 jmp P2_Enemy_Pawn_Possible_Move
 
 
 P2_First_Pawn_Move:
 
 mov P2_GLOBAL_COUNTER_PAWN,2
 P2_loop_first_move_pawn:
   CMP P2_Y,26
   JE P2_FAR_SKIP_PAWN_MOVEEEEE
   JMP P2_SKIP_FAR_SKIP_PAWN_MOVEEEEE
   P2_FAR_SKIP_PAWN_MOVEEEEE:
   JMP FAR PTR P2_SKIP_PAWN_MOVE
   P2_SKIP_FAR_SKIP_PAWN_MOVEEEEE:
 sub P2_y,22
 MemoryToMemoryMovWord P2_GLOBAL_ROW, P2_y
 MemoryToMemoryMovWord P2_GLOBAL_COLUMN,P2_X
 call P2_GET_CELL_FROM_XY
 CMP P2_GLOBAL_CELL[4],2
    JE P2_FARRRRRRR_SKIP_PAWN_MOVE
    JMP P2_SKIP_FARRRRRRR_SKIP_PAWN_MOVE
    P2_FARRRRRRR_SKIP_PAWN_MOVE:
    JMP P2_Enemy_Pawn_Possible_Move ; P2_SKIP_PAWN_MOVE
    P2_SKIP_FARRRRRRR_SKIP_PAWN_MOVE:
 CMP P2_GLOBAL_CELL[4],1
 JE P2_FAR_SKIP_PAWN_MOVE     ; AS ITS AN ENEMY PIECE
 JMP P2_SKIP_FAR_SKIP_PAWN_MOVEE
 P2_FAR_SKIP_PAWN_MOVE:
 JMP FAR PTR P2_Enemy_Pawn_Possible_Move; P2_SKIP_PAWN_MOVE
 P2_SKIP_FAR_SKIP_PAWN_MOVEE:
 mov P2_color,purple
 CALL P2_Draw_Border_Proc 
 P2_Push_Cell_Into_Highlighted_Array
 dec P2_GLOBAL_COUNTER_PAWN
 jnz P2_loop_first_move_pawn
 
 MOV P2_GLOBAL_COLUMN,ax
 MOV P2_GLOBAL_ROW,BX  
 jmp P2_Enemy_Pawn_Possible_Move
 
 
 
 P2_Enemy_Pawn_Possible_Move:
   MemoryToMemoryMovWord P2_x,P2_GLOBAL_COLUMN
   MemoryToMemoryMovWord P2_y,P2_GLOBAL_ROW
    MOV AX,P2_GLOBAL_COLUMN
    MOV BX,P2_GLOBAL_ROW
    CMP P2_X,226
    JE P2_FAR_SKIP_PAWN_MOVE_RIGHT
    JMP P2_SKIP_FAR_SKIP_PAWN_MOVE_RIGHT
    P2_FAR_SKIP_PAWN_MOVE_RIGHT:
    JMP FAR PTR P2_SKIP_PAWN_MOVE_RIGHT
    P2_SKIP_FAR_SKIP_PAWN_MOVE_RIGHT:
    sub P2_y,22
    add P2_x,22
    MemoryToMemoryMovWord P2_GLOBAL_ROW, P2_y
    MemoryToMemoryMovWord P2_GLOBAL_COLUMN,P2_X
    call P2_GET_CELL_FROM_XY
   
    cmp P2_GLOBAL_CELL[4],1
    JNE P2_FAR_SKIP_PAWN_MOVEEE
    JMP P2_SKIP_FAR_SKIP_PAWN_MOVEEE
    P2_FAR_SKIP_PAWN_MOVEEE:
    MOV P2_GLOBAL_COLUMN,ax
    MOV P2_GLOBAL_ROW,BX  
    JMP FAR PTR P2_SKIP_PAWN_MOVE_RIGHT;P2_SKIP_PAWN_MOVE;HNA
    P2_SKIP_FAR_SKIP_PAWN_MOVEEE:
    mov P2_color,black
    CALL P2_Draw_Border_Proc 
    P2_Push_Cell_Into_Highlighted_Array
    
    MOV P2_GLOBAL_COLUMN,ax
    MOV P2_GLOBAL_ROW,BX  
    
    P2_SKIP_PAWN_MOVE_RIGHT:
    
   MemoryToMemoryMovWord P2_x,P2_GLOBAL_COLUMN
   MemoryToMemoryMovWord P2_y,P2_GLOBAL_ROW
    MOV AX,P2_GLOBAL_COLUMN
    MOV BX,P2_GLOBAL_ROW
    CMP P2_x,72
    JE P2_FAR_SKIP_PAWN_MOVE_LEFT
    JMP P2_SKIP_FAR_SKIP_PAWN_MOVE_LEFT
    P2_FAR_SKIP_PAWN_MOVE_LEFT:
    JMP FAR PTR P2_SKIP_PAWN_MOVE
    P2_SKIP_FAR_SKIP_PAWN_MOVE_LEFT:
    sub P2_y,22
    SUB P2_x,22
 
    MemoryToMemoryMovWord P2_GLOBAL_ROW, P2_y
    MemoryToMemoryMovWord P2_GLOBAL_COLUMN,P2_X
    call P2_GET_CELL_FROM_XY
    cmp P2_GLOBAL_CELL[4],1
    JNE P2_SKIP_PAWN_MOVE_ENEMY
    mov P2_color,black
    CALL P2_Draw_Border_Proc 
    P2_Push_Cell_Into_Highlighted_Array
 
 P2_SKIP_PAWN_MOVE_ENEMY:
 P2_skip_First_Pawn_Move:
 P2_SKIP_PAWN_MOVE:
 
 MOV P2_GLOBAL_COLUMN,ax
 MOV P2_GLOBAL_ROW,BX  
 CALL P2_GET_CELL_FROM_XY
 ;pop cx
 pop si
 pop bx
 pop ax
  ret
 P2_Highlight_Possible_Pawn_Moves ENDP

;**************************************
 ;**************************************PLAYER_2_FUNCTIONS*****************************


 Highlight_Possible_Bishop_Moves PROC FAR
;*down***
push ax
push bx
;push si
;mov si, offset Array_Of_Highlighted_X_Y
   ;mov Number_Of_Stored_Highlighted_Cells,0 ;this should be generalized in mov in p1 pressed q not here

 MemoryToMemoryMovWord x,GLOBAL_COLUMN
  MemoryToMemoryMovWord y,GLOBAL_ROW
   MOV AX,GLOBAL_COLUMN
   MOV BX,GLOBAL_ROW
   DOWNLEFTLOOP:
   CMP Y,158
   jE LOWERLEFTLIMIT1
   CMP X,72
   jE LOWERLEFTLIMIT1
   JMP SKIP_FAR_LOWER_LEFT_LIMIT
   LOWERLEFTLIMIT1:


   jmp FAR PTR LOWERLEFTLIMIT
SKIP_FAR_LOWER_LEFT_LIMIT:
  ADD y,22
  SUB X,22
    MemoryToMemoryMovWord GLOBAL_ROW, y
   MemoryToMemoryMovWord GLOBAL_COLUMN,X
  CALL GET_CELL_FROM_XY

  
  CMP GLOBAL_CELL[4],1
  JE MY_PIECE_Bishop_DOWNLEFT

  CMP GLOBAL_CELL[4],1
  
JA ENEMY_PIECE_Bishop_DOWNLEFT
  mov color,GREEN
 CALL Draw_Border_Proc 
 Push_Cell_Into_Highlighted_Array
CMP Y,158
 JNE DOWNLEFTLOOP


jmp LOWERLEFTLIMIT ; dy ana zwdtha 3shn kda ht5ls loop fo2 w tnzl tnafez code el enemy w howa mfesh enemy aslan 
 ENEMY_PIECE_Bishop_DOWNLEFT:


  mov color,YELLOW
 CALL Draw_Border_Proc
 Push_Cell_Into_Highlighted_Array
MY_PIECE_Bishop_DOWNLEFT:
 LOWERLEFTLIMIT:

 MOV GLOBAL_COLUMN,ax
MOV GLOBAL_ROW,BX       


;  ;*UP***
 MemoryToMemoryMovWord x,GLOBAL_COLUMN
 MemoryToMemoryMovWord y,GLOBAL_ROW
   MOV AX,GLOBAL_COLUMN
   MOV BX,GLOBAL_ROW
UPLEFTLOOP:
   CMP Y,4
   jE UPPERLEFTLIMIT1
   CMP X,72
   jE UPPERLEFTLIMIT1
  SUB y,22
  SUB X,22
    MemoryToMemoryMovWord GLOBAL_ROW, y
   MemoryToMemoryMovWord GLOBAL_COLUMN,X
  CALL GET_CELL_FROM_XY
  CMP GLOBAL_CELL[4],1
  JE MY_PIECE_LEFT_BISHOP
JA ENEMY_PIECE_LEFT_Bishop
  mov color,GREEN
 CALL Draw_Border_Proc
 Push_Cell_Into_Highlighted_Array
CMP Y,4
 JNE UPLEFTLOOP
 UPPERLEFTLIMIT1:
JMP UPPERLEFTLIMIT
 ENEMY_PIECE_LEFT_Bishop:
  ;SUB y,22
  mov color,YELLOW
 CALL Draw_Border_Proc
 Push_Cell_Into_Highlighted_Array
MY_PIECE_LEFT_BISHOP:
UPPERLEFTLIMIT:

MOV GLOBAL_COLUMN,ax
MOV GLOBAL_ROW,BX

; ;*RIGHT***

MemoryToMemoryMovWord x,GLOBAL_COLUMN
  MemoryToMemoryMovWord y,GLOBAL_ROW
  MOV AX,GLOBAL_COLUMN
   MOV BX,GLOBAL_ROW
UPRIGHTLOOP:
CMP X,226
   jE UPRIGHTLIMIT1
   CMP Y,4
   jE UPRIGHTLIMIT1
  ADD X,22
  SUB Y,22
    MemoryToMemoryMovWord GLOBAL_ROW, y
   MemoryToMemoryMovWord GLOBAL_COLUMN,X
  CALL GET_CELL_FROM_XY
  CMP GLOBAL_CELL[4],1
  JE MY_PIECE_Bishop_UPRIGHT
JA ENEMY_PIECE_Bishop_UPRIGHT
  mov color,GREEN
 CALL Draw_Border_Proc
 Push_Cell_Into_Highlighted_Array
CMP X,226
 JNE UPRIGHTLOOP
 UPRIGHTLIMIT1:
 JMP UPRIGHTLIMIT
 ENEMY_PIECE_Bishop_UPRIGHT:

  mov color,YELLOW
 CALL Draw_Border_Proc
 Push_Cell_Into_Highlighted_Array
MY_PIECE_Bishop_UPRIGHT:
 UPRIGHTLIMIT:
MOV GLOBAL_COLUMN,ax
MOV GLOBAL_ROW,BX

; ;*left***
MemoryToMemoryMovWord x,GLOBAL_COLUMN
  MemoryToMemoryMovWord y,GLOBAL_ROW
  MOV AX,GLOBAL_COLUMN
   MOV BX,GLOBAL_ROW
DOWNRIGHTLOOP:
CMP X,226
   jE DOWNRIGHTLIMIT1
   CMP Y,158
   jE DOWNRIGHTLIMIT1
  ADD X,22
  ADD Y,22
    MemoryToMemoryMovWord GLOBAL_ROW, y
   MemoryToMemoryMovWord GLOBAL_COLUMN,X
  CALL GET_CELL_FROM_XY
  CMP GLOBAL_CELL[4],1
  JE MY_PIECE_Bishop_DOWNRIGHT
JA ENEMY_PIECE_Bishop_DOWNRIGHT
  mov color,GREEN
 CALL Draw_Border_Proc
 Push_Cell_Into_Highlighted_Array
CMP X,226
 JNE DOWNRIGHTLOOP
 DOWNRIGHTLIMIT1:
 JMP DOWNRIGHTLIMIT
 ENEMY_PIECE_Bishop_DOWNRIGHT:

  mov color,YELLOW
 CALL Draw_Border_Proc

 Push_Cell_Into_Highlighted_Array
MY_PIECE_Bishop_DOWNRIGHT:
 DOWNRIGHTLIMIT:
MOV GLOBAL_COLUMN,ax
MOV GLOBAL_ROW,BX
call GET_CELL_FROM_XY
;pop si
pop bx
pop ax

 Ret
 Highlight_Possible_Bishop_Moves ENDP
 ;***********************
 ;*********************************************************************
Highlight_Possible_Rook_Moves PROC FAR
 ;****down********
   push ax
   push bx
   
   MemoryToMemoryMovWord x,GLOBAL_COLUMN
   MemoryToMemoryMovWord y,GLOBAL_ROW
     MOV AX,GLOBAL_COLUMN
     MOV BX,GLOBAL_ROW
  
     mov si, offset Array_Of_Highlighted_X_Y 
     mov Number_Of_Stored_Highlighted_Cells,0 
   DOWNLOOP:
   CMP Y,158
     jE LOWERLIMIT1
   JMP SKIP_FAR_LOWER_LIMIT
     LOWERLIMIT1:
  
     jmp FAR PTR  LOWERLIMIT
  SKIP_FAR_LOWER_LIMIT:
    ADD y,22
      MemoryToMemoryMovWord GLOBAL_ROW, y
     MemoryToMemoryMovWord GLOBAL_COLUMN,X
    CALL GET_CELL_FROM_XY
  ;**************************************
   
  
    CMP GLOBAL_CELL[4],1
    JE MY_PIECE_ROOK_DOWN1
  
   
  JMP SKIP_FAR_ROOK_DOWN
    MY_PIECE_ROOK_DOWN1:
    
    ;**************************************
  JMP MY_PIECE_ROOK_DOWN
  SKIP_FAR_ROOK_DOWN:
  
    CMP GLOBAL_CELL[4],1
    
  JA ENEMY_PIECE_ROOK_DOWN
    mov color,GREEN
   CALL Draw_Border_Proc
   Push_Cell_Into_Highlighted_Array ;MACRO
  CMP Y, 158
   JNE DOWNLOOP
  
  jmp LOWERLIMIT ; dy ana zwdtha 3shn kda ht5ls loop fo2 w tnzl tnafez code el enemy w howa mfesh enemy aslan 
   ENEMY_PIECE_ROOK_DOWN:
  
    
    mov color,YELLOW
   CALL Draw_Border_Proc
  
   Push_Cell_Into_Highlighted_Array
  
  MY_PIECE_ROOK_DOWN:
   LOWERLIMIT:
   
   MOV GLOBAL_COLUMN,ax
  MOV GLOBAL_ROW,BX
  
  ;*****************UP***************************
  
   MemoryToMemoryMovWord x,GLOBAL_COLUMN
   MemoryToMemoryMovWord y,GLOBAL_ROW
     MOV AX,GLOBAL_COLUMN
     MOV BX,GLOBAL_ROW
  UPLOOP:
     CMP Y,4
     jE UPPERLIMIT
  
    SUB y,22
      MemoryToMemoryMovWord GLOBAL_ROW, y
     MemoryToMemoryMovWord GLOBAL_COLUMN,X
    CALL GET_CELL_FROM_XY
    CMP GLOBAL_CELL[4],1
    JE MY_PIECE
  JA ENEMY_PIECE_ROOK
    mov color,GREEN
   CALL Draw_Border_Proc
   Push_Cell_Into_Highlighted_Array
  CMP Y,4
   JNE UPLOOP
   
  JMP UPPERLIMIT
   ENEMY_PIECE_ROOK:
    
    mov color,YELLOW
   CALL Draw_Border_Proc
   Push_Cell_Into_Highlighted_Array
  
  MY_PIECE:
  UPPERLIMIT:
  
  MOV GLOBAL_COLUMN,ax
  MOV GLOBAL_ROW,BX
  
  ;******************RIGHT*******************
  
  MemoryToMemoryMovWord x,GLOBAL_COLUMN
    MemoryToMemoryMovWord y,GLOBAL_ROW
    MOV AX,GLOBAL_COLUMN
     MOV BX,GLOBAL_ROW
  RIGHTLOOP:
  CMP X,226
     jE RIGHTLIMIT
  
    ADD X,22
      MemoryToMemoryMovWord GLOBAL_ROW, y
     MemoryToMemoryMovWord GLOBAL_COLUMN,X
    CALL GET_CELL_FROM_XY
    CMP GLOBAL_CELL[4],1
    JE MY_PIECE_ROOK_RIGHT
  JA ENEMY_PIECE_ROOK_RIGHT
    mov color,GREEN
   CALL Draw_Border_Proc
  Push_Cell_Into_Highlighted_Array
  
  
  CMP X,226
   JNE RIGHTLOOP
   JMP RIGHTLIMIT
   ENEMY_PIECE_ROOK_RIGHT:
  
    mov color,YELLOW
   CALL Draw_Border_Proc
  Push_Cell_Into_Highlighted_Array
  
  
  MY_PIECE_ROOK_RIGHT:
   RIGHTLIMIT:
  MOV GLOBAL_COLUMN,ax
  MOV GLOBAL_ROW,BX
  
  ;************************LEFT*******************
  MemoryToMemoryMovWord x,GLOBAL_COLUMN
    MemoryToMemoryMovWord y,GLOBAL_ROW
     MOV AX,GLOBAL_COLUMN
     MOV BX,GLOBAL_ROW
  LEFTLOOP:
  CMP X,72
     jE LEFTLIMIT
  
    SUB X,22
      MemoryToMemoryMovWord GLOBAL_ROW, y
     MemoryToMemoryMovWord GLOBAL_COLUMN,X
    CALL GET_CELL_FROM_XY
    CMP GLOBAL_CELL[4],1
    JE MY_PIECE_ROOK_LEFT
  JA ENEMY_PIECE_ROOK_LEFT
    mov color,GREEN 
   CALL Draw_Border_Proc
   Push_Cell_Into_Highlighted_Array
  CMP X,72
   JNE LEFTLOOP
   JMP LEFTLIMIT
   ENEMY_PIECE_ROOK_LEFT:
    mov color,YELLOW
   CALL Draw_Border_Proc
   Push_Cell_Into_Highlighted_Array
  MY_PIECE_ROOK_LEFT:
   LEFTLIMIT:
  MOV GLOBAL_COLUMN,ax
  MOV GLOBAL_ROW,BX
  call GET_CELL_FROM_XY
  
  
  
  
  
  ;pop si
  ;POP DX
  pop bx
  pop ax
  
   Ret
  Highlight_Possible_Rook_Moves ENDP
;*************************************************END ROOK*************************************************
 Highlight_Possible_Queen_Moves PROC FAR
 
  call Highlight_Possible_Rook_Moves
  call Highlight_Possible_Bishop_Moves
  
  RET
  Highlight_Possible_Queen_Moves ENDP

;**************************************************************************************************************
 Highlight_Possible_Knight_Moves PROC FAR
 PUSH AX
 PUSH BX
 PUSH SI
 
                                 ;   MovCursorToLocation 0,5
                                 ;   MOV AX, Is_Allowed_To_Move_Flag
                                 ;  DisplayNumberAX 
   
    mov si, offset Array_Of_Highlighted_X_Y
    mov Number_Of_Stored_Highlighted_Cells,0 ;this should be generalized in mov in p1 pressed q not here
    
  ;******************UPPERLEFT*************************************
  
   UPPERLEFT_KNIGHT_MOVES:
   MemoryToMemoryMovWord x,GLOBAL_COLUMN
   MemoryToMemoryMovWord y,GLOBAL_ROW
    MOV AX,GLOBAL_COLUMN
    MOV BX,GLOBAL_ROW
  cmp y,4
  jz SKIP_UPPERLEFT_KNIGHT_MOVES
  cmp y,26
  jz SKIP_UPPERLEFT_KNIGHT_MOVES
  cmp x,72
  jz SKIP_UPPERLEFT_KNIGHT_MOVES
   sub y,44
   SUB X,22
   MemoryToMemoryMovWord GLOBAL_ROW, y
   MemoryToMemoryMovWord GLOBAL_COLUMN,X
   
   CALL GET_CELL_FROM_XY
   CMP GLOBAL_CELL[4],1
   JE MY_PIECE_KNIGHT_UPLEFT
   mov color,GREEN
  CALL Draw_Border_Proc 
  Push_Cell_Into_Highlighted_Array
 
 SKIP_UPPERLEFT_KNIGHT_MOVES:
 MY_PIECE_KNIGHT_UPLEFT:
  MOV GLOBAL_COLUMN,ax
 MOV GLOBAL_ROW,BX     
 
 ;******************UPPERRIGHT*************************************
 
  UPPERRIGHT_KNIGHT_MOVES:
   MemoryToMemoryMovWord x,GLOBAL_COLUMN
  MemoryToMemoryMovWord y,GLOBAL_ROW
    MOV AX,GLOBAL_COLUMN
    MOV BX,GLOBAL_ROW
  cmp y,4
  jz SKIP_UPPERRIGHT_KNIGHT_MOVES
  cmp y,26
  jz SKIP_UPPERRIGHT_KNIGHT_MOVES
  cmp X,226
  jz SKIP_UPPERRIGHT_KNIGHT_MOVES
   sub y,44
   ADD x,22
   MemoryToMemoryMovWord GLOBAL_ROW, y
   MemoryToMemoryMovWord GLOBAL_COLUMN,X
   CALL GET_CELL_FROM_XY
  CMP GLOBAL_CELL[4],1
   JE MY_PIECE_KNIGHT_UPRIGHT
   mov color,GREEN
  CALL Draw_Border_Proc 
  Push_Cell_Into_Highlighted_Array
 
 SKIP_UPPERRIGHT_KNIGHT_MOVES:
 MY_PIECE_KNIGHT_UPRIGHT:
 
 MOV GLOBAL_COLUMN,ax
 MOV GLOBAL_ROW,BX
 
 ;******************DOWNLEFT*************************************
 DOWNLEFT_KNIGHT_MOVES:
 MemoryToMemoryMovWord x,GLOBAL_COLUMN
  MemoryToMemoryMovWord y,GLOBAL_ROW
    MOV AX,GLOBAL_COLUMN
    MOV BX,GLOBAL_ROW
  cmp y,158
  jz SKIP_DOWNLEFT_KNIGHT_MOVES
  cmp y,136
  jz SKIP_DOWNLEFT_KNIGHT_MOVES
  cmp x,72
  jz SKIP_DOWNLEFT_KNIGHT_MOVES
   ADD y,44
   SUB X,22
   MemoryToMemoryMovWord GLOBAL_ROW, y
   MemoryToMemoryMovWord GLOBAL_COLUMN,X
   
   CALL GET_CELL_FROM_XY
   CMP GLOBAL_CELL[4],1
   JE MY_PIECE_KNIGHT_DOWNLEFT
   mov color,GREEN
  CALL Draw_Border_Proc 
  Push_Cell_Into_Highlighted_Array
 
 SKIP_DOWNLEFT_KNIGHT_MOVES:
 MY_PIECE_KNIGHT_DOWNLEFT:
  MOV GLOBAL_COLUMN,ax
 MOV GLOBAL_ROW,BX    
 
 ;******************DOWNRIGHT*************************************
 DOWNRIGHT_KNIGHT_MOVES:
 MemoryToMemoryMovWord x,GLOBAL_COLUMN
  MemoryToMemoryMovWord y,GLOBAL_ROW
    MOV AX,GLOBAL_COLUMN
    MOV BX,GLOBAL_ROW
  cmp y,158
  jz SKIP_DOWNRIGHT_KNIGHT_MOVES
  cmp y,136
  jz SKIP_DOWNRIGHT_KNIGHT_MOVES
  cmp x,226
  jz SKIP_DOWNRIGHT_KNIGHT_MOVES
   ADD y,44
   ADD X,22
   MemoryToMemoryMovWord GLOBAL_ROW, y
   MemoryToMemoryMovWord GLOBAL_COLUMN,X
   
   CALL GET_CELL_FROM_XY
   CMP GLOBAL_CELL[4],1
   JE MY_PIECE_KNIGHT_DOWNRIGHT
   mov color,GREEN
  CALL Draw_Border_Proc 
  Push_Cell_Into_Highlighted_Array
 
 SKIP_DOWNRIGHT_KNIGHT_MOVES:
 MY_PIECE_KNIGHT_DOWNRIGHT:
  MOV GLOBAL_COLUMN,ax
 MOV GLOBAL_ROW,BX    
 
 ;******************RIGHTDOWN*************************************
 RIGHTDOWN_KNIGHT_MOVES:
 MemoryToMemoryMovWord x,GLOBAL_COLUMN
  MemoryToMemoryMovWord y,GLOBAL_ROW
    MOV AX,GLOBAL_COLUMN
    MOV BX,GLOBAL_ROW
  cmp x,226
  jz SKIP_RIGHTDOWN_KNIGHT_MOVES
  cmp x,204
  jz SKIP_RIGHTDOWN_KNIGHT_MOVES
  cmp y,158
  jz SKIP_RIGHTDOWN_KNIGHT_MOVES
   add x,44
   ADD y,22
   MemoryToMemoryMovWord GLOBAL_ROW, y
   MemoryToMemoryMovWord GLOBAL_COLUMN,X
   
   CALL GET_CELL_FROM_XY
   CMP GLOBAL_CELL[4],1
   JE MY_PIECE_KNIGHT_RIGHTDOWN
   mov color,GREEN
  CALL Draw_Border_Proc 
  Push_Cell_Into_Highlighted_Array
 
 SKIP_RIGHTDOWN_KNIGHT_MOVES:
 MY_PIECE_KNIGHT_RIGHTDOWN:
  MOV GLOBAL_COLUMN,ax
 MOV GLOBAL_ROW,BX     
 
 ;******************RIGHTUP*************************************
 RIGHTUP_KNIGHT_MOVES:
 MemoryToMemoryMovWord x,GLOBAL_COLUMN
  MemoryToMemoryMovWord y,GLOBAL_ROW
    MOV AX,GLOBAL_COLUMN
    MOV BX,GLOBAL_ROW
  cmp x,226
  jz SKIP_RIGHTUP_KNIGHT_MOVES
  cmp x,204
  jz SKIP_RIGHTUP_KNIGHT_MOVES
  cmp y,4
  jz SKIP_RIGHTUP_KNIGHT_MOVES
   add x,44
   SUB y,22
   MemoryToMemoryMovWord GLOBAL_ROW, y
   MemoryToMemoryMovWord GLOBAL_COLUMN,X
   
   CALL GET_CELL_FROM_XY
   CMP GLOBAL_CELL[4],1
   JE MY_PIECE_KNIGHT_RIGHTUP
   mov color,GREEN
  CALL Draw_Border_Proc 
  Push_Cell_Into_Highlighted_Array
 
 SKIP_RIGHTUP_KNIGHT_MOVES:
 MY_PIECE_KNIGHT_RIGHTUP:
  MOV GLOBAL_COLUMN,ax
 MOV GLOBAL_ROW,BX     
 
 ;******************LEFTDOWN*************************************
 LEFTDOWN_KNIGHT_MOVES:
 MemoryToMemoryMovWord x,GLOBAL_COLUMN
  MemoryToMemoryMovWord y,GLOBAL_ROW
    MOV AX,GLOBAL_COLUMN
    MOV BX,GLOBAL_ROW
  cmp x,72
  jz SKIP_LEFTDOWN_KNIGHT_MOVES
  cmp x,94
  jz SKIP_LEFTDOWN_KNIGHT_MOVES
  cmp y,158
  jz SKIP_LEFTDOWN_KNIGHT_MOVES
   SUB x,44
   ADD y,22
   MemoryToMemoryMovWord GLOBAL_ROW, y
   MemoryToMemoryMovWord GLOBAL_COLUMN,X
   
   CALL GET_CELL_FROM_XY
   CMP GLOBAL_CELL[4],1
   JE MY_PIECE_KNIGHT_LEFTDOWN
   mov color,GREEN
  CALL Draw_Border_Proc 
  Push_Cell_Into_Highlighted_Array
 
 SKIP_LEFTDOWN_KNIGHT_MOVES:
 MY_PIECE_KNIGHT_LEFTDOWN:
  MOV GLOBAL_COLUMN,ax
 MOV GLOBAL_ROW,BX     
 
 ;******************LEFTUP*************************************
 LEFTUP_KNIGHT_MOVES:
 MemoryToMemoryMovWord x,GLOBAL_COLUMN
  MemoryToMemoryMovWord y,GLOBAL_ROW
    MOV AX,GLOBAL_COLUMN
    MOV BX,GLOBAL_ROW
  cmp x,72
  jz SKIP_LEFTUP_KNIGHT_MOVES
  cmp x,94
  jz SKIP_LEFTUP_KNIGHT_MOVES
  cmp y,4
  jz SKIP_LEFTUP_KNIGHT_MOVES
   SUB x,44
   sub y,22
   MemoryToMemoryMovWord GLOBAL_ROW, y
   MemoryToMemoryMovWord GLOBAL_COLUMN,X
   
   CALL GET_CELL_FROM_XY
   CMP GLOBAL_CELL[4],1
   JE MY_PIECE_KNIGHT_LEFTUP
   mov color,GREEN
  CALL Draw_Border_Proc 
  Push_Cell_Into_Highlighted_Array
 
 SKIP_LEFTUP_KNIGHT_MOVES:
 MY_PIECE_KNIGHT_LEFTUP:
  MOV GLOBAL_COLUMN,ax
 MOV GLOBAL_ROW,BX   
 CALL GET_CELL_FROM_XY  
 ;***************************************************************
 pop si
 pop bx
 pop ax
 
 ret
 Highlight_Possible_Knight_Moves ENDP
;****************************************************************************************************************



 ;*********************************************************************

;**************************************************************************************************************
Highlight_Possible_King_Moves PROC FAR
 PUSH AX
 PUSH BX
 PUSH SI
   
   
  MOV GLOBAL_COLUMN,ax
  MOV GLOBAL_ROW,BX     
 ;*********
    mov si, offset Array_Of_Highlighted_X_Y
    mov Number_Of_Stored_Highlighted_Cells,0 ;this should be generalized in mov in p1 pressed q not here
    
  ;******************UP*************************************
  
   UP_King_Moves:
   MemoryToMemoryMovWord x,GLOBAL_COLUMN
   MemoryToMemoryMovWord y,GLOBAL_ROW
    MOV AX,GLOBAL_COLUMN
    MOV BX,GLOBAL_ROW
  cmp y,4
  jz SKIP_UP_KING_MOVES
   sub y,22
   MemoryToMemoryMovWord GLOBAL_ROW, y
   MemoryToMemoryMovWord GLOBAL_COLUMN,X
   
   CALL GET_CELL_FROM_XY
   CMP GLOBAL_CELL[4],1
   JE MY_PIECE_KING_UP
   mov color,GREEN
  CALL Draw_Border_Proc 
  Push_Cell_Into_Highlighted_Array
 
 SKIP_UP_KING_MOVES:
 MY_PIECE_KING_UP:
  MOV GLOBAL_COLUMN,ax
 MOV GLOBAL_ROW,BX     
 
 ;******************DOWN*************************************
  
   DOWN_King_Moves:
   MemoryToMemoryMovWord x,GLOBAL_COLUMN
   MemoryToMemoryMovWord y,GLOBAL_ROW
    MOV AX,GLOBAL_COLUMN
    MOV BX,GLOBAL_ROW
  cmp y,158
  jz SKIP_DOWN_KING_MOVES
   add y,22
   MemoryToMemoryMovWord GLOBAL_ROW, y
   MemoryToMemoryMovWord GLOBAL_COLUMN,X
   
   CALL GET_CELL_FROM_XY
   CMP GLOBAL_CELL[4],1
   JE MY_PIECE_KING_DOWN
   mov color,GREEN
  CALL Draw_Border_Proc 
  Push_Cell_Into_Highlighted_Array
 
 SKIP_DOWN_KING_MOVES:
 MY_PIECE_KING_DOWN:
  MOV GLOBAL_COLUMN,ax
 MOV GLOBAL_ROW,BX     
 
 ;******************UPPERRIGHT*************************************
 
  UPPERRIGHT_KING_MOVES:
   MemoryToMemoryMovWord x,GLOBAL_COLUMN
  MemoryToMemoryMovWord y,GLOBAL_ROW
    MOV AX,GLOBAL_COLUMN
    MOV BX,GLOBAL_ROW
  cmp y,4
  jz SKIP_UPPERRIGHT_KING_MOVES
  cmp X,226
  jz SKIP_UPPERRIGHT_KING_MOVES
   sub y,22
   ADD x,22
   MemoryToMemoryMovWord GLOBAL_ROW, y
   MemoryToMemoryMovWord GLOBAL_COLUMN,X
   CALL GET_CELL_FROM_XY
  CMP GLOBAL_CELL[4],1
   JE MY_PIECE_KING_UPRIGHT
   mov color,GREEN
  CALL Draw_Border_Proc 
  Push_Cell_Into_Highlighted_Array
 
 SKIP_UPPERRIGHT_KING_MOVES:
 MY_PIECE_KING_UPRIGHT:
 
 MOV GLOBAL_COLUMN,ax
 MOV GLOBAL_ROW,BX
 
 ;******************UPPERLEFT*************************************
  
   UPPERLEFT_KING_MOVES:
   MemoryToMemoryMovWord x,GLOBAL_COLUMN
   MemoryToMemoryMovWord y,GLOBAL_ROW
    MOV AX,GLOBAL_COLUMN
    MOV BX,GLOBAL_ROW
  cmp y,4
  jz SKIP_UPPERLEFT_KING_MOVES
  cmp x,72
  jz SKIP_UPPERLEFT_KING_MOVES
   sub y,22
   SUB X,22
   MemoryToMemoryMovWord GLOBAL_ROW, y
   MemoryToMemoryMovWord GLOBAL_COLUMN,X
   
   CALL GET_CELL_FROM_XY
   CMP GLOBAL_CELL[4],1
   JE MY_PIECE_KING_UPLEFT
   mov color,GREEN
  CALL Draw_Border_Proc 
  Push_Cell_Into_Highlighted_Array
 
 SKIP_UPPERLEFT_KING_MOVES:
 MY_PIECE_KING_UPLEFT:
  MOV GLOBAL_COLUMN,ax
 MOV GLOBAL_ROW,BX     
 ;******************DOWNLEFT*************************************
 DOWNLEFT_KING_MOVES:
 MemoryToMemoryMovWord x,GLOBAL_COLUMN
  MemoryToMemoryMovWord y,GLOBAL_ROW
    MOV AX,GLOBAL_COLUMN
    MOV BX,GLOBAL_ROW
  cmp y,158
  jz SKIP_DOWNLEFT_KING_MOVES
  cmp x,72
  jz SKIP_DOWNLEFT_KING_MOVES
   ADD y,22
   SUB X,22
   MemoryToMemoryMovWord GLOBAL_ROW, y
   MemoryToMemoryMovWord GLOBAL_COLUMN,X
   
   CALL GET_CELL_FROM_XY
   CMP GLOBAL_CELL[4],1
   JE MY_PIECE_KING_DOWNLEFT
   mov color,GREEN
  CALL Draw_Border_Proc 
  Push_Cell_Into_Highlighted_Array
 
 SKIP_DOWNLEFT_KING_MOVES:
 MY_PIECE_KING_DOWNLEFT:
  MOV GLOBAL_COLUMN,ax
 MOV GLOBAL_ROW,BX    
 
 ;******************DOWNRIGHT*************************************
 DOWNRIGHT_KING_MOVES:
 MemoryToMemoryMovWord x,GLOBAL_COLUMN
  MemoryToMemoryMovWord y,GLOBAL_ROW
    MOV AX,GLOBAL_COLUMN
    MOV BX,GLOBAL_ROW
  cmp y,158
  jz SKIP_DOWNRIGHT_KING_MOVES
  cmp x,226
  jz SKIP_DOWNRIGHT_KING_MOVES
   ADD y,22
   ADD X,22
   MemoryToMemoryMovWord GLOBAL_ROW, y
   MemoryToMemoryMovWord GLOBAL_COLUMN,X
   
   CALL GET_CELL_FROM_XY
   CMP GLOBAL_CELL[4],1
   JE MY_PIECE_KING_DOWNRIGHT
   mov color,GREEN
  CALL Draw_Border_Proc 
  Push_Cell_Into_Highlighted_Array
 
 SKIP_DOWNRIGHT_KING_MOVES:
 MY_PIECE_KING_DOWNRIGHT:
  MOV GLOBAL_COLUMN,ax
 MOV GLOBAL_ROW,BX    
 
 ;******************RIGHT*************************************
 RIGHT_KING_MOVES:
 MemoryToMemoryMovWord x,GLOBAL_COLUMN
  MemoryToMemoryMovWord y,GLOBAL_ROW
    MOV AX,GLOBAL_COLUMN
    MOV BX,GLOBAL_ROW
  cmp x,226
  jz SKIP_RIGHT_KING_MOVES
 
   add x,22
   MemoryToMemoryMovWord GLOBAL_ROW, y
   MemoryToMemoryMovWord GLOBAL_COLUMN,X
   
   CALL GET_CELL_FROM_XY
   CMP GLOBAL_CELL[4],1
   JE MY_PIECE_KING_RIGHT
   mov color,GREEN
  CALL Draw_Border_Proc 
  Push_Cell_Into_Highlighted_Array
 
 SKIP_RIGHT_KING_MOVES:
 MY_PIECE_KING_RIGHT:
  MOV GLOBAL_COLUMN,ax
 MOV GLOBAL_ROW,BX     
 
 ;******************LEFT*************************************
 LEFT_KING_MOVES:
 MemoryToMemoryMovWord x,GLOBAL_COLUMN
  MemoryToMemoryMovWord y,GLOBAL_ROW
    MOV AX,GLOBAL_COLUMN
    MOV BX,GLOBAL_ROW
  cmp x,72
  jz SKIP_LEFT_KING_MOVES
 
   SUB x,22
   MemoryToMemoryMovWord GLOBAL_ROW, y
   MemoryToMemoryMovWord GLOBAL_COLUMN,X
   
   CALL GET_CELL_FROM_XY
   CMP GLOBAL_CELL[4],1
   JE MY_PIECE_KING_LEFT
   mov color,GREEN
  CALL Draw_Border_Proc 
  Push_Cell_Into_Highlighted_Array
 
 SKIP_LEFT_KING_MOVES:
 MY_PIECE_KING_LEFT:
  MOV GLOBAL_COLUMN,ax
 MOV GLOBAL_ROW,BX     
 call GET_CELL_FROM_XY
  ;***************************************************************
 pop si
 pop bx
 pop ax
 
 ret
 Highlight_Possible_King_Moves ENDP
;****************************************************************************************************************

;****************************************************************************************************************

Highlight_Possible_Pawn_Moves PROC FAR
 PUSH AX
 PUSH BX
 PUSH SI
 
    
 ;***
    mov si, offset Array_Of_Highlighted_X_Y
    mov Number_Of_Stored_Highlighted_Cells,0 ;this should be generalized in mov in p1 pressed q not here
    
 Forward_Pawn_Moves:
   MemoryToMemoryMovWord x,GLOBAL_COLUMN
   MemoryToMemoryMovWord y,GLOBAL_ROW
    MOV AX,GLOBAL_COLUMN
    MOV BX,GLOBAL_ROW
   
    call GET_CELL_FROM_XY
    
   cmp GLOBAL_CELL[2],26
   je First_Pawn_Move1
   jmp skip_First_Pawn_Move1
   First_Pawn_Move1:
 jmp First_Pawn_Move
 skip_First_Pawn_Move1:
 
 
   NOT_FIRST_PAWN_MOVE:
   CMP Y,158
   JE FAR_SKIP_PAWN_MOVEEEE
   JMP SKIP_FAR_SKIP_PAWN_MOVEEEE
   FAR_SKIP_PAWN_MOVEEEE:
   JMP FAR PTR SKIP_PAWN_MOVE
   SKIP_FAR_SKIP_PAWN_MOVEEEE:
   add y,22
 MemoryToMemoryMovWord GLOBAL_ROW, y
 MemoryToMemoryMovWord GLOBAL_COLUMN,X
 call GET_CELL_FROM_XY
  CMP GLOBAL_CELL[4],1
    JE FARRRRR_SKIP_PAWN_MOVE
    JMP SKIP_FARRRRR_SKIP_PAWN_MOVE
    FARRRRR_SKIP_PAWN_MOVE:
    JMP Enemy_Pawn_Possible_Move;NEW  
    SKIP_FARRRRR_SKIP_PAWN_MOVE:
 CMP GLOBAL_CELL[4],2
 JE FAR_SKIP_PAWN_MOVEE    ; AS ITS AN ENEMY PIECE
 JMP SKIP_FAR_SKIP_PAWN_MOVE
 FAR_SKIP_PAWN_MOVEE:
 MOV GLOBAL_COLUMN,ax
 MOV GLOBAL_ROW,BX  
 JMP FAR PTR Enemy_Pawn_Possible_Move
 SKIP_FAR_SKIP_PAWN_MOVE:
 mov color,GREEN
 CALL Draw_Border_Proc 
 Push_Cell_Into_Highlighted_Array
 MOV GLOBAL_COLUMN,ax
 MOV GLOBAL_ROW,BX  
 jmp Enemy_Pawn_Possible_Move
 
 
 First_Pawn_Move:
 
 mov GLOBAL_COUNTER_PAWN,2
 loop_first_move_pawn:
   CMP Y,136
   JE FAR_SKIP_PAWN_MOVEEEEE
   JMP SKIP_FAR_SKIP_PAWN_MOVEEEEE
   FAR_SKIP_PAWN_MOVEEEEE:
   JMP FAR PTR SKIP_PAWN_MOVE
   SKIP_FAR_SKIP_PAWN_MOVEEEEE:
 add y,22
 MemoryToMemoryMovWord GLOBAL_ROW, y
 MemoryToMemoryMovWord GLOBAL_COLUMN,X
 call GET_CELL_FROM_XY
 CMP GLOBAL_CELL[4],1
    JE FARRRRRRR_SKIP_PAWN_MOVE
    JMP SKIP_FARRRRRRR_SKIP_PAWN_MOVE
    FARRRRRRR_SKIP_PAWN_MOVE:
    JMP SKIP_PAWN_MOVE
    SKIP_FARRRRRRR_SKIP_PAWN_MOVE:
 CMP GLOBAL_CELL[4],2
 JE FAR_SKIP_PAWN_MOVE     ; AS ITS AN ENEMY PIECE
 JMP SKIP_FAR_SKIP_PAWN_MOVEE
 FAR_SKIP_PAWN_MOVE:
 JMP FAR PTR Enemy_Pawn_Possible_Move
 SKIP_FAR_SKIP_PAWN_MOVEE:
 mov color,GREEN
 CALL Draw_Border_Proc 
 Push_Cell_Into_Highlighted_Array
 dec GLOBAL_COUNTER_PAWN
 jnz loop_first_move_pawn
 
 MOV GLOBAL_COLUMN,ax
 MOV GLOBAL_ROW,BX  
 jmp Enemy_Pawn_Possible_Move
 
 
 
 Enemy_Pawn_Possible_Move:
   MemoryToMemoryMovWord x,GLOBAL_COLUMN
   MemoryToMemoryMovWord y,GLOBAL_ROW
    MOV AX,GLOBAL_COLUMN
    MOV BX,GLOBAL_ROW
    CMP X,226
    JE FAR_SKIP_PAWN_MOVE_RIGHT
    JMP SKIP_FAR_SKIP_PAWN_MOVE_RIGHT
    FAR_SKIP_PAWN_MOVE_RIGHT:
    JMP FAR PTR SKIP_PAWN_MOVE_RIGHT
    SKIP_FAR_SKIP_PAWN_MOVE_RIGHT:
    add y,22
    add x,22
    MemoryToMemoryMovWord GLOBAL_ROW, y
    MemoryToMemoryMovWord GLOBAL_COLUMN,X
    call GET_CELL_FROM_XY
   
    cmp GLOBAL_CELL[4],2
    JNE FAR_SKIP_PAWN_MOVEEE
    JMP SKIP_FAR_SKIP_PAWN_MOVEEE
    FAR_SKIP_PAWN_MOVEEE:
    MOV GLOBAL_COLUMN,ax
    MOV GLOBAL_ROW,BX  
    JMP FAR PTR SKIP_PAWN_MOVE_RIGHT
    SKIP_FAR_SKIP_PAWN_MOVEEE:
    mov color,YELLOW
    CALL Draw_Border_Proc 
    Push_Cell_Into_Highlighted_Array
    
    MOV GLOBAL_COLUMN,ax
    MOV GLOBAL_ROW,BX  
    
    SKIP_PAWN_MOVE_RIGHT:
    
   MemoryToMemoryMovWord x,GLOBAL_COLUMN
   MemoryToMemoryMovWord y,GLOBAL_ROW
    MOV AX,GLOBAL_COLUMN
    MOV BX,GLOBAL_ROW
    CMP x,72
    JE FAR_SKIP_PAWN_MOVE_LEFT
    JMP SKIP_FAR_SKIP_PAWN_MOVE_LEFT
    FAR_SKIP_PAWN_MOVE_LEFT:
    JMP FAR PTR SKIP_PAWN_MOVE
    SKIP_FAR_SKIP_PAWN_MOVE_LEFT:
    add y,22
    SUB x,22
 
    MemoryToMemoryMovWord GLOBAL_ROW, y
    MemoryToMemoryMovWord GLOBAL_COLUMN,X
    call GET_CELL_FROM_XY
    cmp GLOBAL_CELL[4],2
    JNE SKIP_PAWN_MOVE_ENEMY
    mov color,YELLOW
    CALL Draw_Border_Proc 
    Push_Cell_Into_Highlighted_Array
 
 SKIP_PAWN_MOVE_ENEMY:
 skip_First_Pawn_Move:
 SKIP_PAWN_MOVE:
 
 MOV GLOBAL_COLUMN,ax
 MOV GLOBAL_ROW,BX  
 CALL GET_CELL_FROM_XY
 ;pop cx
 pop si
 pop bx
 pop ax
  ret
 Highlight_Possible_Pawn_Moves ENDP


;**************************************************************************************************************

;*************************************************************************************************
InitPiecesPROC PROC FAR
  MOV GLOBAL_COLUMN,72
  MOV GLOBAL_ROW,4

  INITPIECESROWS:
 INITPIECESLOOP:

  CALL GET_CELL_FROM_XY
  CALL Draw_Complete_Cell_Proc
  ADD GLOBAL_COLUMN,22
 CMP GLOBAL_COLUMN,227
 JB INITPIECESLOOP
 MOV GLOBAL_COLUMN,72
 ADD GLOBAL_ROW,22
 CMP GLOBAL_ROW,159
 JB INITPIECESROWS
 RET
 InitPiecesPROC ENDP


SET_Is_Allowed_To_Move_Flag PROC FAR
 push si
 push di
 PUSH AX 
 mov si,0   ;will hold x 
 mov di ,2  ; will hold y
 
 MOV Is_Allowed_To_Move_Flag,0 
 MemoryToMemoryMovWord temp_Number_Of_Stored_Cells_In_Highlighted_Cells_Array,Number_Of_Stored_Highlighted_Cells
 MemoryToMemoryMovWord GLOBAL_COLUMN, CurrentXPosition
 MemoryToMemoryMovWord GLOBAL_ROW, CurrentYPosition 
  
 
  MemoryToMemoryMovWord  Temp_Selected_Cell_Index,Selected_Cell_Index
 
  
 ;CALL SET_Selected_cell_Index ;SELECTED_CELL_INDEX VARIABLE NOW HOLDS THE INDEX OF THE CELL I WANT TO MOVE TO
                             
 ; MOV THIS INDEX TO A REGISTER BEFOR THE LOOP -> FOR THE CMP INSTRUCTION 
 
 
 ;NOW GET THIS INDEX FOR EACH PAIR IN HIGHLIGHTED ARRAY ,COMPARE IT WITH TEMP INDEX, IF FOUND THEN THE MOV IS POSSIBLE
 CMP Number_Of_Stored_Highlighted_Cells ,0 
 JE NOT_FOUND 
 Search_for_cell_in_array_of_highlighted_cells:
 
 MemoryToMemoryMovWord GLOBAL_COLUMN, Array_Of_Highlighted_X_Y[si] ;starting from 0
 MemoryToMemoryMovWord GLOBAL_ROW, Array_Of_Highlighted_X_Y[di] ;starting from 1
 call SET_Selected_cell_Index 

 CALL GET_CELL_FROM_XY
 MOV AX,Selected_cell_Index

 cmp AX , Temp_Selected_Cell_Index
 jne SEARCH_AGAIN  
 
 JMP DONT_SEARCH_AGAIN
 SEARCH_AGAIN:
 ADD SI,4
 ADD DI,4 
 dec temp_Number_Of_Stored_Cells_In_Highlighted_Cells_Array 
 JNZ Search_for_cell_in_array_of_highlighted_cells
 JMP NOT_FOUND
 
 DONT_SEARCH_AGAIN:
 
 ;if equal : set the flag is found to 1
 MOV  Is_Allowed_To_Move_Flag ,1 
 
 NOT_FOUND:
 MemoryToMemoryMovWord GLOBAL_COLUMN,CurrentXPosition
 MemoryToMemoryMovWord GLOBAL_ROW,CurrentYPosition
 MemoryToMemoryMovWord  Selected_Cell_Index,Temp_Selected_Cell_Index
 CALL GET_CELL_FROM_XY 
 
 POP AX
 pop di 
 pop si
 RET
  SET_Is_Allowed_To_Move_Flag ENDP 
 
  CHECK_PROC_P1 PROC FAR
   PUSHA
  MOV bx, 0
  mov di,0 
  PUSH SI
  CALL Highlight_Possible_Rook_Moves
  POP SI
  fill_Check_Array_ROOK:
  MemoryToMemoryMovWord Array_Of_Possible_Moves_For_Check_P1[di] , Array_Of_Highlighted_X_Y[bx]
  add bx ,2
  add di ,2  
  MemoryToMemoryMovWord Array_Of_Possible_Moves_For_Check_P1[di] , Array_Of_Highlighted_X_Y[bx]
  add bx ,2
  add di ,2  
  INC Number_of_Cells_In_Array_Of_Check_p1
  dec Number_Of_Stored_Highlighted_Cells
  
  jnz fill_Check_Array_ROOK
  mov bx,0
  
   PUSH SI
   mov si, offset Array_Of_Highlighted_X_Y
   mov Number_Of_Stored_Highlighted_Cells,0
   CALL Highlight_Possible_Bishop_Moves
  POP SI
  
  fill_Check_Array_Bishop:
  MemoryToMemoryMovWord Array_Of_Possible_Moves_For_Check_P1[di] , Array_Of_Highlighted_X_Y[bx]
  add bx ,2
  add di ,2  
  MemoryToMemoryMovWord Array_Of_Possible_Moves_For_Check_P1[di] , Array_Of_Highlighted_X_Y[bx]
  add bx ,2
  add di ,2  

  INC Number_of_Cells_In_Array_Of_Check_p1
  dec Number_Of_Stored_Highlighted_Cells
  jnz fill_Check_Array_Bishop
  mov bx,0
  
  
  
  CALL Highlight_Possible_King_Moves
  fill_Check_Array_King:
  MemoryToMemoryMovWord Array_Of_Possible_Moves_For_Check_P1[di] , Array_Of_Highlighted_X_Y[bx]
  add bx ,2
  add di ,2  
  MemoryToMemoryMovWord Array_Of_Possible_Moves_For_Check_P1[di] , Array_Of_Highlighted_X_Y[bx]
  add bx ,2
  add di ,2  
  INC Number_of_Cells_In_Array_Of_Check_p1
  dec Number_Of_Stored_Highlighted_Cells
  jnz fill_Check_Array_King
  mov bx,0
  
  
  CALL Highlight_Possible_Knight_Moves
  fill_Check_Array_Knight:
  MemoryToMemoryMovWord Array_Of_Possible_Moves_For_Check_P1[di] , Array_Of_Highlighted_X_Y[bx]
  add bx ,2
  add di ,2  
  MemoryToMemoryMovWord Array_Of_Possible_Moves_For_Check_P1[di] , Array_Of_Highlighted_X_Y[bx]
  add bx ,2
  add di ,2  
  INC Number_of_Cells_In_Array_Of_Check_p1
  dec Number_Of_Stored_Highlighted_Cells
  jnz fill_Check_Array_Knight
  mov bx,0
  
  
  CALL Highlight_Possible_Pawn_Moves
  
  fill_Check_Array_Pawn:
  MemoryToMemoryMovWord Array_Of_Possible_Moves_For_Check_P1[di] , Array_Of_Highlighted_X_Y[bx]
  add bx ,2
  add di ,2  
  MemoryToMemoryMovWord Array_Of_Possible_Moves_For_Check_P1[di] , Array_Of_Highlighted_X_Y[bx]
  add bx ,2
  add di ,2  
  INC Number_of_Cells_In_Array_Of_Check_p1
  dec Number_Of_Stored_Highlighted_Cells
  jnz fill_Check_Array_Pawn
  mov bx,0
  
  
  PUSH SI
   mov si, offset Array_Of_Highlighted_X_Y
   mov Number_Of_Stored_Highlighted_Cells,0
   CALL Highlight_Possible_Queen_Moves
   POP SI
  fill_Check_Array_Queen:
  MemoryToMemoryMovWord Array_Of_Possible_Moves_For_Check_P1[di] , Array_Of_Highlighted_X_Y[bx]
  add bx ,2
  add di ,2  
  MemoryToMemoryMovWord Array_Of_Possible_Moves_For_Check_P1[di] , Array_Of_Highlighted_X_Y[bx]
  add bx ,2
  add di ,2   
  INC Number_of_Cells_In_Array_Of_Check_p1
  dec Number_Of_Stored_Highlighted_Cells
  jnz fill_Check_Array_Queen
  mov bx,0
  
   POPA
   RET
    CHECK_PROC_P1 ENDP  

;*************************************************************************************

;*************************************************************************************

Traverse_Check_PROC PROC FAR
 PUSHA
 MemoryToMemoryMovWord temp_global_row4,GLOBAL_ROW
 MemoryToMemoryMovWord temp_global_column4,GLOBAL_COLUMN
 MemoryToMemoryMovWord temp_Number_Of_Stored_Cells_In_Highlighted_Cells_Array,Number_Of_Stored_Highlighted_Cells
 mov si,0
 mov di ,2 
 search_for_player_2_king:
 MemoryToMemoryMovWord GLOBAL_COLUMN,Array_Of_Highlighted_X_Y[si]
 MemoryToMemoryMovWord GLOBAL_ROW,Array_Of_Highlighted_X_Y[di]
 call GET_CELL_FROM_XY
 cmp GLOBAL_CELL[8] ,29
 je king2_found
 add si,4
 add di,4
 dec temp_Number_Of_Stored_Cells_In_Highlighted_Cells_Array
 jnz search_for_player_2_king
 jmp skip_king2_found
 king2_found:
 ;MemoryToMemoryMovWord p1_check_pos_column,GLOBAL_COLUMN
 ;MemoryToMemoryMovWord p1_check_pos_row,GLOBAL_ROW
 ;mov CHECK_FLAG,1 ;black player checks yellow player 
 MovCursorToLocation 0,18
 displaystring p1_checks_p2_msg
 skip_king2_found:

 MemoryToMemoryMovWord GLOBAL_COLUMN,temp_global_column4
 MemoryToMemoryMovWord GLOBAL_ROW,temp_global_row4

  CALL GET_CELL_FROM_XY
POPA
RET 
Traverse_Check_PROC ENDP 


P2_Traverse_Check_PROC PROC FAR
 PUSHA
 MemoryToMemoryMovWord P2_temp_global_row4,P2_GLOBAL_ROW
 MemoryToMemoryMovWord P2_temp_global_column4,P2_GLOBAL_COLUMN
 MemoryToMemoryMovWord P2_temp_Number_Of_Stored_Cells_In_Highlighted_Cells_Array,P2_Number_Of_Stored_Highlighted_Cells
 mov si,0
 mov di ,2 
 P2_search_for_player_2_king:
 MemoryToMemoryMovWord P2_GLOBAL_COLUMN,P2_Array_Of_Highlighted_X_Y[si]
 MemoryToMemoryMovWord P2_GLOBAL_ROW,P2_Array_Of_Highlighted_X_Y[di]
 call P2_GET_CELL_FROM_XY
 cmp P2_GLOBAL_CELL[8] ,13;hna
 je P2_king2_found
 add si,4
 add di,4
 dec P2_temp_Number_Of_Stored_Cells_In_Highlighted_Cells_Array
 jnz P2_search_for_player_2_king
 jmp P2_skip_king2_found
 P2_king2_found:
 ;MemoryToMemoryMovWord p1_check_pos_column,GLOBAL_COLUMN
 ;MemoryToMemoryMovWord p1_check_pos_row,GLOBAL_ROW
 ;mov CHECK_FLAG,1 ;black player checks yellow player 
 MovCursorToLocation 72,18
 displaystring p2_checks_p1_msg
 P2_skip_king2_found:

 MemoryToMemoryMovWord P2_GLOBAL_COLUMN,P2_temp_global_column4
 MemoryToMemoryMovWord P2_GLOBAL_ROW,P2_temp_global_row4

  CALL P2_GET_CELL_FROM_XY
POPA
RET 
P2_Traverse_Check_PROC ENDP 



Player1_Pressed_Q  PROC FAR
 PUSH CX
 PUSH DX
 
 PUSH AX
 PUSH BX
 MOV AX,CurrentXPosition
 MOV BX,CurrentYPosition
 PUSH AX
 PUSH BX
 
 MemoryToMemoryMovWord GLOBAL_COLUMN, CurrentXPosition
 MemoryToMemoryMovWord GLOBAL_ROW, CurrentYPosition
 
 CALL GET_CELL_FROM_XY
 ;;;CHECK IF THE SELECTED CELL HAS A PIECE OF MINE///ENEMY
 
 CMP P1_IS_SELECTED ,0
 
 JE P1_SELECT_PIECE1
 
 JMP P1_SELECT_PIECE1_SKIP_FAR
 
 P1_SELECT_PIECE1:
 JMP FAR PTR P1_SELECT_PIECE
 P1_SELECT_PIECE1_SKIP_FAR:
 
 JNE P1_MOVING_PIECE ;************WE HAVE TO TRACK POSSIBLE DROPS**********;
 
 
 ;**********************************START OF MOVING LOGIC****************************************************************************************
  ; CALL Traverse_Check_PROC ; row and column are set 
  ; cmp CHECK_FLAG,1 
  ; je display_check_msg 

  ; display_check_msg:
  ; MovCursorToLocation 20,78
  ; displaystring p1_checks_p2_msg
  ; ; MemoryToMemoryMovWord p1_check_pos_column , CurrentXPosition
  ; ; MemoryToMemoryMovWord p1_check_pos_row,CurrentYPosition

 P1_MOVING_PIECE:
 MOV P1_IS_SELECTED ,0
 
 MemoryToMemoryMovWord temp_global_column2,GLOBAL_COLUMN
 MemoryToMemoryMovWord temp_global_row2,GLOBAL_ROW
 MemoryToMemoryMovWord Temp_Selected_Cell_Index2,Selected_Cell_Index
 MemoryToMemoryMovWord GLOBAL_COLUMN,CurrentXPosition
 MemoryToMemoryMovWord GLOBAL_ROW,CurrentYPosition
 CALL GET_CELL_FROM_XY
 CALL SET_Selected_cell_Index 
 
 CALL SET_Is_Allowed_To_Move_Flag 
 MOV SI,Selected_cell_Index
     
 
 MemoryToMemoryMovWord Selected_Cell_Index,Temp_Selected_Cell_Index2
 MemoryToMemoryMovWord GLOBAL_COLUMN,temp_global_column2
 MemoryToMemoryMovWord GLOBAL_ROW,temp_global_row2
 CALL GET_CELL_FROM_XY
;kan hna traverse check
 CMP Is_Allowed_To_Move_Flag,0
 
 
 
 JE Not_possible_mov1 
 
 CALL CLEAR_CELL            ;id of previous cell is 00 
 CALL SET_CELL              ; DRAW NEW PIECE ON THE MOVED_TO CELL 

;new timer 
  pusha
 mov al,CURRENTMINUTE 
 mov bl, 60 
 MUL bl   
 ;al contains minutes in seconds  
 ADD al, CURRENTSECOND  ;al feha el total time 
 add al,3 
 ;mov ah,0
 MemoryToMemoryMovWord cell_1_1[si+10], ax
popa  





;**********TAR2EYA*************
 PUSH SI
 CALL SET_Selected_cell_Index 
 MOV SI,Selected_cell_Index
 cmp Cell_1_1[SI+8],1
 je pawn_prob
 jne skip_pawn_prob
 pawn_prob:
 cmp GLOBAL_ROW,158
 je tar2ya
 jne skip_pawn_prob
 tar2ya:
 mov Cell_1_1[SI+8],12
 skip_pawn_prob:
POP SI
 ;**********TAR2EYA*************


 
 ;***********************BN SET EL TIME OF CELL GWA SET_CELL*******
JMP CONTMOV
Not_possible_mov1:
JMP Not_possible_mov

CONTMOV:
ShowNotificationFORP1



 Not_possible_mov:
 MOV P1_IS_SELECTED ,0
 
  

 CALL DeHighlightCells 

 ;CALL Traverse_Check_PROC  ;ERGA3Y L DEH  

 JMP END_MOV ;  THIS WAS NOT WORKING BECAUSE WHEN MOVING , BY3ADY 3L SELECT BRDO FA 3MLT JUMP 
 
 
 ;**********************************END OF MOVING LOGIC****************************************************************************************
 
 
 ;********************************** START OF SELECTING LOGIC ****************************************************************************************
 
 P1_SELECT_PIECE:
;*************************TIMER *******************************************
PUSH AX                               
MOV aX, CURRENTTIME
CMP GLOBAL_CELL[10] , AX   

 JAE FAR_END_MOV 
 JMP SKIP_FAR_END_MOV

                                     ;

 FAR_END_MOV:
 POP AX

 MovCursorToLocation 0,20
 DisplayString mes_Timer 

 
 JMP FAR PTR ILLEGAL_MOVE_ON_P1_PIECES
 SKIP_FAR_END_MOV:
 POP AX

;**************************************************************************
 CMP GLOBAL_CELL[4],1
 jne ENEMY_PIECE1                           ;THE SELECTED PIECE IS INVALID -> RETURN FROM FUNCTION
 
 JMP SKIP_ENEMY_PIECE_FAR
 ENEMY_PIECE1:
 
 JMP FAR PTR ENEMY_PIECE
 SKIP_ENEMY_PIECE_FAR:
;new timer 
  pusha
 mov al,CURRENTMINUTE 
 mov bl, 60 
 MUL bl   
 ;al contains minutes in seconds  
 ADD al, CURRENTSECOND  ;al feha el total time 
 ;add al,3 
 ;mov ah,0
 MemoryToMemoryMovWord totalCurrentTime, ax
 
mov cx,totalCurrentTime
 MOV SI,Selected_cell_Index
 cmp Cell_1_1[SI+10],cx
 JA END_Mov1
 popa
jmp skip_END_Mov1
 END_Mov1:
 popa
  MovCursorToLocation 0,18;henaaa
  DisplayString mes_Timer 
 jmp far ptr END_Mov 
skip_END_Mov1:
 MOV P1_IS_SELECTED,1
 CALL SET_Selected_cell_Index ;            WE HAVE TO HIGHLIGHT POSSIBLE MOVES**********;
  ;***************************************
  ;***************************************
  ;***************************************
  ;***************************************TODO
 
  MemoryToMemoryMovWord x,GLOBAL_COLUMN
   MemoryToMemoryMovWord y,GLOBAL_ROW
   ;add y,22
   mov color,GREEN
  CALL Draw_Border_Proc
                                       ;NOW SELECTED CELL INDEX HOLDS ITS INFO 
 CMP GLOBAL_CELL[8],9 ;ROOK
 JNZ SKIP_Rook
  
 
 CALL Highlight_Possible_Rook_Moves
 
 SKIP_Rook: 
 
 
 CMP GLOBAL_CELL[8],10 ;KNIGHT
 JNZ SKIP_KNIGHT
 CALL Highlight_Possible_Knight_Moves
 SKIP_KNIGHT:
 
 CMP GLOBAL_CELL[8],13 ;KING
 JNZ SKIP_KING
 CALL Highlight_Possible_King_Moves 
 SKIP_KING:
 
 
 CMP GLOBAL_CELL[8],12 ;QUEEN
 JNZ SKIP_QUEEN
 PUSH SI
 mov si, offset Array_Of_Highlighted_X_Y
 mov Number_Of_Stored_Highlighted_Cells,0
 CALL Highlight_Possible_Queen_Moves
 POP SI
 SKIP_QUEEN:
 
 
 CMP GLOBAL_CELL[8],11 ;BISHOP
 JNZ SKIP_BISHOP
 PUSH SI
 mov si, offset Array_Of_Highlighted_X_Y
 mov Number_Of_Stored_Highlighted_Cells,0
 CALL Highlight_Possible_Bishop_Moves
 POP SI
 SKIP_BISHOP:
 
 
 CMP GLOBAL_CELL[8],1 ;PAWN
 JNZ SKIP_PAWNN
 CALL Highlight_Possible_Pawn_Moves 
 SKIP_PAWNN:
 



 JMP END_MOV
 
 ENEMY_PIECE:
 Illegal_Move_On_Un_Highlighted_Cells: ;new
 ;pop ax ;new
 ILLEGAL_MOVE_ON_P1_PIECES:
 ;raga3y dy zy mkanet un commented 
 CALL DeHighlightCells 
 ;CALL Traverse_Check_PROC

 ;********************************** END OF SELECTING LOGIC ****************************************************************************************
 
                                        ;BELL SOUND IF ILLEGAL (NOT TESTED YET)**********
 ;********************************** START OF SELECTING LOGIC ****************************************************************************************
 
 END_MOV:
 
 
 ;MOV P1_IS_SELECTED,0
 POP BX ;CURRENT X
 POP AX ;CURRENT Y
 MOV CurrentXPosition,AX
 MOV CurrentYPosition,BX
 
 POP BX
 POP AX
 
 POP dX
 POP CX
 RET
 Player1_Pressed_Q ENDP 
P2_ENTER_PRESSED_PROC  PROC FAR
 PUSH CX
 PUSH DX
 
 
 
 PUSH AX
 PUSH BX
 MOV AX,P2_CurrentXPosition
 MOV BX,P2_CurrentYPosition
 PUSH AX
 PUSH BX
 MemoryToMemoryMovWord P2_GLOBAL_COLUMN, P2_CurrentXPosition
 MemoryToMemoryMovWord P2_GLOBAL_ROW,    P2_CurrentYPosition
 
 CALL P2_GET_CELL_FROM_XY
 ;;;CHECK IF THE SELECTED CELL HAS A PIECE OF MINE///ENEMY
 
 CMP P2_IS_SELECTED ,0
 
 JE P2_SELECT_PIECE1
 
 JMP P2_SELECT_PIECE1_SKIP_FAR
 
 P2_SELECT_PIECE1:
 JMP FAR PTR P2_SELECT_PIECE
 P2_SELECT_PIECE1_SKIP_FAR:
 
 JNE P2_MOVING_PIECE ;************WE HAVE TO TRACK POSSIBLE DROPS**********;
 
 
 ;**********************************START OF MOVING LOGIC****************************************************************************************
 
 
 
 P2_MOVING_PIECE:
 MOV P2_IS_SELECTED ,0
 
 MemoryToMemoryMovWord P2_temp_global_column2,P2_GLOBAL_COLUMN
 MemoryToMemoryMovWord P2_temp_global_row2,   P2_GLOBAL_ROW
 MemoryToMemoryMovWord P2_Temp_Selected_Cell_Index2,P2_Selected_Cell_Index
 MemoryToMemoryMovWord P2_GLOBAL_COLUMN,P2_CurrentXPosition
 MemoryToMemoryMovWord P2_GLOBAL_ROW,   P2_CurrentYPosition
 CALL P2_GET_CELL_FROM_XY
 CALL P2_SET_Selected_cell_Index 
 
 CALL P2_SET_Is_Allowed_To_Move_Flag 
  MOV SI,P2_Selected_cell_Index
     
 
 MemoryToMemoryMovWord P2_Selected_Cell_Index,P2_Temp_Selected_Cell_Index2
 MemoryToMemoryMovWord P2_GLOBAL_COLUMN,      P2_temp_global_column2
 MemoryToMemoryMovWord P2_GLOBAL_ROW,         P2_temp_global_row2
 CALL P2_GET_CELL_FROM_XY
 CMP P2_Is_Allowed_To_Move_Flag,0
 
 
 
 JE P2_Not_possible_mov1 
 
 CALL P2_CLEAR_CELL            ;id of previous cell is 00 
 CALL P2_SET_CELL              ; DRAW NEW PIECE ON THE MOVED_TO CELL 
;new timer 
  pusha
 mov al,CURRENTMINUTE 
 mov bl, 60 
 MUL bl   
 ;al contains minutes in seconds  
 ADD al, CURRENTSECOND  ;al feha el total time 
 add al,3 
 ;mov ah,0
 MemoryToMemoryMovWord cell_1_1[si+10], ax
popa  

;**********TAR2EYA*************
PUSH SI
 ;CALL P2_SET_Selected_cell_Index 
 MOV SI,P2_Selected_cell_Index
 cmp Cell_1_1[SI+8],17
 je P2_pawn_prob
 jne P2_skip_pawn_prob
 P2_pawn_prob:
 cmp P2_GLOBAL_ROW,4
 je P2_tar2ya
 jne P2_skip_pawn_prob
 P2_tar2ya:
 mov Cell_1_1[SI+8],28
 P2_skip_pawn_prob:
POP SI
 ;**********TAR2EYA*************


JMP P2_CONTMOV
P2_Not_possible_mov1:
JMP P2_Not_possible_mov

P2_CONTMOV:
ShowNotificationFORP2 
 
 P2_Not_possible_mov:
 MOV P2_IS_SELECTED ,0
 
 CALL P2_DeHighlightCells 
  
 JMP  P2_END_MOV ;  THIS WAS NOT WORKING BECAUSE WHEN MOVING , BY3ADY 3L SELECT BRDO FA 3MLT JUMP 
 
 
 ;**********************************END OF MOVING LOGIC****************************************************************************************
 
 
 ;********************************** START OF SELECTING LOGIC ****************************************************************************************
 
 P2_SELECT_PIECE:

;****************************TIMER*******************************
PUSH AX
MOV aX, CURRENTTIME
CMP  AX ,  P2_GLOBAL_CELL[10]   ;NEW

 JBE P2_FAR_END_MOV 
 JMP P2_SKIP_FAR_END_MOV

                                     ;NEW

 P2_FAR_END_MOV:
 POP AX
 MovCursorToLocation 32,20
 displaystring mes_Timer
 JMP FAR PTR P2_ILLEGAL_MOVE_ON_P2_PIECES ;;;;;
     P2_SKIP_FAR_END_MOV:
 POP AX
;******************************TIMER****************************

 CMP P2_GLOBAL_CELL[4],2
 jne P2_ENEMY_PIECE1                           ;THE SELECTED PIECE IS INVALID -> RETURN FROM FUNCTION
 
 JMP P2_SKIP_ENEMY_PIECE_FAR
 P2_ENEMY_PIECE1:
 
 JMP FAR PTR P2_ENEMY_PIECE
 P2_SKIP_ENEMY_PIECE_FAR:
 ;new timer 
  pusha
 mov al,CURRENTMINUTE 
 mov bl, 60 
 MUL bl   
 ;al contains minutes in seconds  
 ADD al, CURRENTSECOND  ;al feha el total time 
 ;add al,3 
 ;mov ah,0
 MemoryToMemoryMovWord totalCurrentTime, ax
 
mov cx,totalCurrentTime
 MOV SI,P2_Selected_cell_Index
 cmp Cell_1_1[SI+10],cx
 JA P2_END_Mov1
 popa
jmp P2_skip_END_Mov1
 P2_END_Mov1:
 popa
   MovCursorToLocation 32,18;henaaa
  DisplayString mes_Timer 
 jmp far ptr P2_END_Mov 
P2_skip_END_Mov1:
 MOV P2_IS_SELECTED,1
 CALL P2_SET_Selected_cell_Index ;            WE HAVE TO HIGHLIGHT POSSIBLE MOVES**********;
  ;***************************************
  ;***************************************
  ;***************************************
  ;***************************************TODO
 
  MemoryToMemoryMovWord  P2_x, P2_GLOBAL_COLUMN
   MemoryToMemoryMovWord P2_y,P2_GLOBAL_ROW
   ;add y,22
   mov P2_color,PURPLE
  CALL P2_Draw_Border_Proc
                                       ;NOW SELECTED CELL INDEX HOLDS ITS INFO 
 CMP  P2_GLOBAL_CELL[8],25 ;ROOK;
 JNZ  P2_SKIP_Rook 
 PUSH SI
 CALL P2_Highlight_Possible_Rook_Moves
 pop si
 P2_SKIP_Rook: 
 
 
 CMP P2_GLOBAL_CELL[8],26;KNIGHT 
 JNZ P2_SKIP_KNIGHT
 CALL P2_Highlight_Possible_Knight_Moves
 P2_SKIP_KNIGHT:
 
 CMP P2_GLOBAL_CELL[8],29 ;KING 
 JNZ P2_SKIP_KING
 CALL P2_Highlight_Possible_King_Moves 
 P2_SKIP_KING:
 

 CMP P2_GLOBAL_CELL[8],28 ;QUEEN
 JNZ P2_SKIP_QUEEN
 PUSH SI
 mov si, offset P2_Array_Of_Highlighted_X_Y
 mov P2_Number_Of_Stored_Highlighted_Cells,0
 CALL P2_Highlight_Possible_Queen_Moves
 POP SI
 P2_SKIP_QUEEN:
 
 
 CMP P2_GLOBAL_CELL[8],27 ;BISHOP
 JNZ P2_SKIP_BISHOP
 PUSH SI
 mov si, offset P2_Array_Of_Highlighted_X_Y
 mov     P2_Number_Of_Stored_Highlighted_Cells,0
 CALL P2_Highlight_Possible_Bishop_Moves
 POP SI
 P2_SKIP_BISHOP:
 
 
 CMP P2_GLOBAL_CELL[8],17 ;PAWN
 JNZ P2_SKIP_PAWNN
 CALL P2_Highlight_Possible_Pawn_Moves 
 P2_SKIP_PAWNN:
 
 
 JMP P2_END_MOV
 
 P2_ENEMY_PIECE:
 P2_Illegal_Move_On_Un_Highlighted_Cells: 

 P2_ILLEGAL_MOVE_ON_P2_PIECES:

 CALL P2_DeHighlightCells 
 ;call P2_Traverse_Check_PROC
 ;********************************** END OF SELECTING LOGIC ****************************************************************************************
 
                                        ;BELL SOUND IF ILLEGAL (NOT TESTED YET)**********
 ;********************************** START OF SELECTING LOGIC ****************************************************************************************
 
P2_END_MOV:
 
 
 ;MOV P1_IS_SELECTED,0
 POP BX ;CURRENT X
 POP AX ;CURRENT Y
 MOV P2_CurrentXPosition,AX
 MOV P2_CurrentYPosition,BX
 
 POP BX
 POP AX
 
 POP dX
 POP CX
 RET
 P2_ENTER_PRESSED_PROC ENDP 


;Asks the user for his name and store it
GetUserName PROC
    player1nameBack:

    ;Print messages
    MovCursorToLocation 3,10
    DisplayString  NameEntryConstrainsMsg       
    MovCursorToLocation 3,1
    DisplayString MSG_WELCOME 
    
    ;Clear username
    MOV BX, 0
    UserName_Clear:
    MOV player1name[BX], '$'
    INC BX
    CMP BX, 15;max usernamesize
    JLE UserName_Clear
    
    ;Wait for user input
    ReadString actual_p1_name_size
    
    ;Check if first character doesn't start with a letter
    CMP player1name[0], 'A'
    JB  player1nameBack
    CMP player1name[0] , 'Z'
    JBE UserName_Return

    CMP player1name[0], 'a'
    JB  player1nameBack
    CMP player1name[0], 'z'
    JA  player1nameBack
    
    UserName_Return:
    
    ;Wait for any key to continue
    MovCursorToLocation 3,15
    DisplayString MSG_CONT
    WaitKeyPress
 
    RET
GetUserName ENDP
;===============================================================

 WaitOtherPlayer PROC
    
   
    ;Print waiting message
    MovCursorToLocation 3, 10 
    DisplayString WaitingPlayerMsg
    
    ;Hide the cursor some where in the screen
    MovCursorToLocation WINDOW_WIDTH, WINDOW_HIGHT, 0
    
    MOV BX, 1
    
    UserName_Send:
    ;Send a letter of username
    MOV CL, actual_p1_name_size[BX]
    SendChar CL
    UserName_SendNotReceived:
       
    ;Check if ESC is pressed to quit the program
    GetKeyPressAndFlush ;e
    CMP AL, ESC_AsciiCode
    JNE UserName_ContinueReceive
    MOV AX, 4C00H
    INT 21H         ;Back to the system
     
 
    UserName_ContinueReceive:


    ReceiveChar

    JZ UserName_SendNotReceived
        MOV actual_p2_name_size[BX], AL
    INC BX
    CMP BX, 15
    JLE UserName_Send
   ;  displayString player2name
    EmptyKeyQueue
    
    RET
WaitOtherPlayer ENDP
;===============================================================



MAIN PROC FAR

                                 MOV                   AX,@DATA
                                 MOV                   DS,AX
                                 MOV                   AX,0
                                 MOV                   BX,0
                                 MOV                   DX,0
                                 MOV                   CX,0
                                 MOV                   SI,0
                                 MOV                   DI,0
                                
                                 
                                 ;Set Divisor Latch Access Bit
	mov dx,3fbh 			; Line Control Register
	mov al,10000000b		;Set Divisor Latch Access Bit
	out dx,al			;Out it
	;Set LSB byte of the Baud Rate Divisor Latch register.
	mov dx,3f8h			
	mov al,0ch			
	out dx,al
	;Set MSB byte of the Baud Rate Divisor Latch register.
	mov dx,3f9h
	mov al,00h
	out dx,al
	;Set port configuration
	mov dx,3fbh
	mov al,00011011b
	;0:Access to Receiver buffer, Transmitter buffer
	;0:Set Break disabled
	;011:Even Parity
	;0:One Stop Bit
	;11:8bits
	out dx,al                   

                                   EnterTextMode
                                   changeMainAndTextScreenColors 7EH
                                   CALL GetUserName
                                   CALL WaitOtherPlayer
                      
                                   welcomecheck:
                                   WaitKeyPress
                                   CMP AH,Enter_ScanCode
                                   JNE welcomecheck
                                  
                                   WELCOMESCREEN:
  
  
                                
                                   ToMainScreenWindow
                                   MemoryToMemoryMovByte InlineP1,actual_p1_name_size+1
                          MemoryToMemoryMovByte InlineP2,actual_p2_name_size+1 
                          inc InlineP1
                          inc InlineP2
                                   welcome:


                                    mov ah,1
	                                  int 16h
                                    jz far_recieveInvitation 
                                    jmp skip_far_recieveInvitation

                                    far_recieveInvitation:
                                    jmp far ptr recieveInvitation

                                    skip_far_recieveInvitation:
                                    mov ah ,0
                                    int 16h
                                    SendChar ah
                                    cmp AH,F1_ScanCode
                                    JNE SKIP_F1PRESS
                                    displaystring MSG_CHATTING2
                                    UpdateCursorLocationRight 1
                                    displaystring player2name
                                    UpdateCursorLocationDown 0
                                    MOV S_FirstTimeFlag,1
                                    CMP R_FirstTimeFlag,1
                                    JE far_label1
                                    jmp skip_far_label1
                                    far_label1:
                                    jmp far ptr label1
                                    skip_far_label1:
                                    SKIP_F1PRESS:
                                    

                                    cmp AH,F2_ScanCode
                                    JNE SKIP_F2PRESS
                                    displaystring MSG_TOGAME2
                                    UpdateCursorLocationRight 1
                                    displaystring player2name
                                    UpdateCursorLocationDown 0


                                    test1:;new
                                    mov S_GAMEINVITE,1
                                    CMP R_GAMEINVITE,1
                                    JE far_2label1 
                                    jmp skip_far_2label1
                                    far_2label1:
                                    jmp far ptr GAME  
                                    skip_far_2label1:
                                    SKIP_F2PRESS:
                                    
                                    
                                    cmp AH,ESC_ScanCode
                                    JE r_ESCPRESS1_1
JMP r_SKIP_ESCPRESS1_1
                                    r_ESCPRESS1_1:
                                    JMP FAR PTR ESCPRESS1
r_SKIP_ESCPRESS1_1:
                                    JMP welcome
                          
                                    recieveInvitation:
                                     ReceiveChar ;al
                                    mov ah,al 
                                    cmp AH,F1_ScanCode
                                    JNE R_NOT_F1PRESS
                                    MOV R_FirstTimeFlag,1
                                    displaystring player2name
                                    UpdateCursorLocationRight actual_p2_name_size+1
                                    displaystring chatchar
                                    displaystring MSG_CHATTING
                                    UpdateCursorLocationDown 0
                                    CMP S_FirstTimeFlag,1
                                    JE label1
                                    R_NOT_F1PRESS:

                                     cmp AH,F2_ScanCode
                                    JNE R_NOT_F2PRESS
                                    
                                    MOV R_GAMEINVITE,1
                                  
                                   
                                    displaystring player2name
                                    UpdateCursorLocationRight actual_p2_name_size+1
                                    displaystring chatchar
                                    displaystring MSG_TOGAME
                                    UpdateCursorLocationDown 0
                                    CMP S_GAMEINVITE,1
                                    JE START_GAME_LOOP9
                                    JMP SKIP_START_GAME_LOOP9
                                    START_GAME_LOOP9:
                                      mov ISBLACKPLAYER,1 ;kant commented 
                                    JMP FAR PTR GAME
                                    SKIP_START_GAME_LOOP9:
                                    R_NOT_F2PRESS:
                                    
                                    cmp AH,ESC_ScanCode
                                    JE ESCPRESS1_1
JMP SKIP_ESCPRESS1_1
                                    ESCPRESS1_1:
                                    JMP FAR PTR ESCPRESS1
SKIP_ESCPRESS1_1:
                                    JMP welcome
                              
;***********************************************************************************************
label1: ;start chatting label bas 3shn a enter text mode mesh gowa infinite loop 

  mov ah, 00h
	mov al, 03h
	int 10h
	
	startchat: 
  mov R_FirstTimeFlag,0
  mov S_FirstTimeFlag,0

  ;   mov ah, 00h
	; mov al, 03h
	; int 10h
  

MovCursorToLocation 0,0
displaystring player1name
MovCursorToLocation actual_p1_name_size+1,0
displaystring chatchar
MovCursorToLocation 0,12
displaystring player2name
MovCursorToLocation actual_p2_name_size+1,12
displaystring chatchar
MovCursorToLocation 0,11
displaystring chatline 
MovCursorToLocation 0,22
displaystring chatline



	;sender
	mov ah,1
	int 16h
	jz receivechat1
  jmp skip_receivechat1
	receivechat1:
  jmp far ptr receivechat

skip_receivechat1:

	mov dx , 3FDH		; Line Status Register
 	In al , dx 			;Read Line Status
	AND al , 00100000b
	jz receivechat2
  jmp skip_receivechat2
	receivechat2:
  jmp far ptr receivechat

skip_receivechat2:
	;If empty put the VALUE in Transmit data register
	mov ah, 0
	int 16h
  ;ascii fel AL , scan ah

  CMP AH,F2_ScanCode ;will send 2 
  JNE Skip_F2 
  mov dx , 3F8H		; Transmit data register
  mov al,2
  out dx , al
  jmp FAR PTR WELCOMESCREEN


  Skip_F2:
   CMP AH, F3_ScanCode
   JNE Skip_F3
    mov dx , 3F8H		; Transmit data register
    mov al,3 
    out dx , al
    jmp FAR PTR WELCOMESCREEN ;main screen window
   Skip_F3:
   cmp ah,ESC_ScanCode
   jne skip_ESC 
   mov dx , 3F8H		; Transmit data register
   mov al,1
   out dx , al
   mov ah ,4ch  
   int 21h 
   skip_ESC:

   mov dx , 3F8H		; Transmit data register
   out dx , al ;ASCII CODES

	;Check if esc is pressed before transmitting
	cmp al, 27d
	je far_escape1chat
jmp skip_far_escape1chat

  far_escape1chat:
  jmp escape1chat
	
  skip_far_escape1chat:
cmp Ah,F2_ScanCode
je gameinvite 
jmp skip_gameinvite

gameinvite:
MovCursorToLocation 0,23
displaystring MSG_CLEAR
displaystring MSG_TOGAME2
MovCursorToLocation 30,23
displaystring player2name



skip_gameinvite:
  cmp al,Enter_AsciiCode
  je entercheck
  jmp skip_entercheck

entercheck:
inc SY
mov SX,0

skip_entercheck:

  cmp al,08H
  je backcheck
  jmp skip_backcheck

backcheck:  ;TODO: NSHOF MALHA DE

	mov ah, 2
	mov dl, 32d
	int 21h

dec sx
MovCursorToLocation sx,sy
jmp skipback

skip_backcheck:

	mov ah, 2
	mov bh, 0
	mov dh, SY
	mov dl, SX
	int 10h



	mov ah, 2
	mov dl, al
	int 21h

	inc SX
skipback:
	cmp sx, 80d
	jne skipchat
	mov sx, 0
	inc sy
	skipchat:

	escape1chat:
	;Check if esc is pressed before transmitting
	cmp al, 27d
	je far_escapechat
  jmp skip_far_escapechat

  far_escapechat:
  jmp escapechat

  skip_far_escapechat:
	receivechat:
		mov dx , 3FDH		; Line Status Register	
		in al , dx 
		AND al , 1
		JZ Far_startchat
    jmp skip_startchat
    Far_startchat:
    jmp startchat

skip_startchat:

	;If Ready read the VALUE in Receive data register
		mov dx , 03F8H
		in al , dx 
	  cmp al,3  
  je WELCOMESCREEN1
  jmp skip_WELCOMESCREEN
  WELCOMESCREEN1:
  jmp far ptr WELCOMESCREEN 
  skip_WELCOMESCREEN:
    
   cmp al,2  
   jne lbl
   jmp far ptr WELCOMESCREEN
   lbl:

   cmp al,1  
   jne skip_escccc
mov ah ,4ch  
   int 21h 
   skip_escccc:
		;Check if esc is pressed before transmitting
		cmp al, 27d
		je far_escapechat1
    jmp skip_far_escapechat1


    far_escapechat1:
    jmp escapechat
skip_far_escapechat1:

    cmp al,16d
    je recieve_gameinvite
    jmp skip_recieve_gameinvite

recieve_gameinvite:
MovCursorToLocation 0,23
displaystring player1name
MovCursorToLocation actual_p1_name_size+1,23
displaystring MSG_CLEAR
displaystring MSG_TOGAME

skip_recieve_gameinvite:
    cmp al,Enter_AsciiCode
    je R_entercheck
    jmp R_skip_entercheck
    R_entercheck:
    inc RY
    mov RX,0

   R_skip_entercheck:
 cmp al,08H
  je R_backcheck
  jmp R_skip_backcheck

R_backcheck:

	mov ah, 2
	mov dl, 32d
	int 21h

dec sx
MovCursorToLocation sx,sy
jmp R_skipback

R_skip_backcheck:
		mov ah, 2
		mov bh, 0
		mov dh, RY
		mov dl, RX
		int 10h

		inc rx
    R_skipback:
		cmp rx, 80d
		jne skip1chat
		mov rx, 0
		inc ry
		skip1chat:
		mov ah, 2
		mov dl, al
		int 21h
		jmp startchat

		escapechat:
 		MOV AH , 4ch
  		INT 21H

 ;***********************************************************************************************
                                
                                  JMP SKIP_ESCPRESS
ESCPRESS1:
JMP ESCPRESS
SKIP_ESCPRESS:

                                  WaitKeyPress
                                  cmp AH,F1_ScanCode
                                  JE CHAT
                                  JNE welcome1
                                  JMP SKIP_WELCOME
                                  welcome1:
                                  JMP FAR PTR welcome
                                  SKIP_WELCOME:
                                  

                                  F2PRESS:

                                  

                                  displaystring player1name+2
                                  UpdateCursorLocationRight 7
                                  displaystring MSG_TOGAME
                                  UpdateCursorLocationDown 0

                                  WaitKeyPress
                                  cmp AH,F2_ScanCode
                                  JE GAMESET
                                   JMP FAR PTR welcome
                                  

                                    GAMESET:
                                    ; mov ISBLACKPLAYER,1
                                     JMP GAME 

                                  ESCPRESS:
                                 displaystring MSG_TERMINATE
                                  JE FARJUMP

   FARJUMP:                      
                                 JMP                   FAR PTR ENDGAME
   ENDGAMESKIP:                  
  
   CHAT:                         
                                                    EnterTextMode

                                 moV                   ax,0600h
                                 mov                   bh,07
                                 MOV                   DX,184FH
                                 int                   10H
    
                                
                              MovCursorToLocation 0,12
                              displaystring chatline
                          
                                MovCursorToLocation 1,0
                                 DisplayString         player1name+2
                                 MovCursorToLocation 1,13
                                 displaystring         player2name+2
                                MovCursorToLocation 1,2
                                 ;ReadString            user_pressed
                                WaitKeyPress
                                 CMP                   ah,F2_ScanCode
                                 JZ                    GAME
    
                                 CMP                   ah,ESC_ScanCode
                                 JZ                    FARJUMP1
                                 JMP                   ENDGAMESKIP1
   FARJUMP1:                     
                                 JMP                   FAR PTR ENDGAME
   ENDGAMESKIP1:                 

   GAME:                         
   MOV S_GAMEINVITE,0
    MOV R_GAMEINVITE,0
                                 EnterGraphicsMode
                                 MOV                   color1,GRAY
                                 MOV                   color2,WHITE
                                  MOV                   P2_color1,GRAY
                                 MOV                   P2_color2,WHITE
                                 MovCursorToLocation 0,0
                                 displaystring player1name
                                 MovCursorToLocation 0,1
                                 DisplayString BLACKPALYER

                                 MovCursorToLocation 32,0
                                 DisplayString player2name
                                 MovCursorToLocation 32,1
                                 displaystring YELLOWPLAYER

                                 MovCursorToLocation 0,23
                                 displaystring player1name
                                 MovCursorToLocation actual_p1_name_size+1,23
                                 displaystring chatchar

                                 MovCursorToLocation 0,24
                                 displaystring player2name
                                 MovCursorToLocation actual_p2_name_size+1,24
                                 displaystring chatchar
                                 
                                 
                                 
CALL InitPiecesPROC





                                 mov                   cx,CurrentXPosition
                                 mov                   dx,CurrentYPosition
   START_GAME_LOOP:              
    MovCursorToLocation 0,20
                                 printtime
                                 ;getTime
                               ;  GetKeyPress                                                        ;16/01
  
                                ; jz                    START_GAME_LOOP
                                ;sender
	mov ah,1
	int 16h
	jz far_recieve
  jmp skip_far_recieve
  far_recieve:
  jmp far ptr recieve
  skip_far_recieve:
	
	mov dx , 3FDH		; Line Status Register
 	In al , dx 			;Read Line Status
	AND al , 00100000b
		jz ffar_recieve
  jmp sskip_far_recieve
  ffar_recieve:
  jmp far ptr recieve
  sskip_far_recieve:
	;If empty put the VALUE in Transmit data register
	mov ah, 0
	int 16h
  ;  mov dx , 3F8H		; Transmit data register
  ;  mov al,ah
	;  out dx , al
	;Check if esc is pressed before transmitting
	;cmp al, 27d
	;je escape1
   pressed:                      
                                                                                ; 16/00
                                ;  CMP                   AH,F2_ScanCode
                                ;  JNE                   NOT_P1
                                ;  mov                   ISBLACKPLAYER,1 
                           NOT_P1:
                           CMP ISBLACKPLAYER,1
                           JNE PLAYER_21

                            
                          jmp skip_PLAYER_21
                           PLAYER_21:
                           jmp far ptr PLAYER_2
                           skip_PLAYER_21:
                                 CMP                   Al,P2_UP_AsciiCode
                                 JE                    UP

                                 JMP                   SKIP_UP
   UP:                           
                                 CALL                  UP_PRESSED_PROC
                                 jmp Out_Ascii
   
                         
            
   SKIP_UP:                      CMP                   Al,P2_DOWN_AsciiCode
                                 JE                    DOWN
                                 JMP                   SKIP_DOWN
   DOWN:                         
                              CALL    DOWN_PRESSED_PROC
                               jmp Out_Ascii
   SKIP_DOWN:                    
                                 CMP                   Al,P2_Right_AsciiCode
                                 JE                    RIGHT
                                 JMP                   SKIP_RIGHT
   RIGHT:              
                               CALL RIGHT_PRESSED_PROC
                                jmp Out_Ascii
    
   SKIP_RIGHT:                   
                                 CMP                   Al,P2_Left_AsciiCode
                                 JE                    LEFT
                                 JMP                   SKIP_LEFT
   LEFT:                       
                                 CALL LEFT_PRESSED_PROC
                                  jmp Out_Ascii
   SKIP_LEFT:                    
                                 CMP                   Al , 9d
                                 JE                    Q_SELECT
                                 JMP                   SKIP_Q_SELECT
   Q_SELECT:                     
                                 
                                 CALL                  Player1_Pressed_Q
                                  jmp Out_Ascii
                                
   SKIP_Q_SELECT:   
  cmp ah,F4_SCANCODE 
  JNE not_f4
   
 mov dx , 3F8H		; Transmit data register
 mov al,4 
 out dx , al 
 jmp FAR PTR WELCOMESCREEN
not_f4:


    mov dx , 3F8H		; Transmit data register
   ;mov al,ah
	out dx , al
;MemoryToMemoryMovByte InlineP1,actual_p1_name_size+1
MovCursorToLocation InlineP1,23
  mov ah,2  
  mov dl, al  
  int 21h 
  inc InlineP1
  cmp InlineP1,39
  je downlineP1
  jmp skip_downlineP1
downlineP1:
; MovCursorToLocation actual_p1_name_size+1,23
; displaystring MSG_CLEAR2
MemoryToMemoryMovByte InlineP1,actual_p1_name_size+1
inc inlinep1
mov ah,06h
mov al,1H
mov bh,00h
mov ch,23
mov cl,inlinep1
mov dh,23
mov dl,39 
int 10h
skip_downlineP1:
jmp skip_Out_Ascii
 Out_Ascii:
  mov dx , 3F8H		; Transmit data register
   ;mov al,ah
	out dx , al
  skip_Out_Ascii:
   ;ESC SCANCODE NOT WORKING
                                 CMP                   ah , ESC_ScanCode
                                 JE                    ENDGAME1 
                                 JMP SKIP_ENDGAME_FAR
                                                       ENDGAME1:
                                                        mov dx , 3F8H		; Transmit data register
                                                        mov al,1
                                                        out dx , al 
                                                       JMP FAR PTR ENDGAME
                                                       SKIP_ENDGAME_FAR:

                                 JMP                   SKIP_ENDGAME
   SKIP_ENDGAME:                 
                                 JMP                   START_GAME_LOOP              
  ;*************************************PLAYER 2 *****************************************    
  PLAYER_2:  
  CMP Al,P2_UP_AsciiCode
  JE P2_UP 
  JMP P2_SKIP_UP
  P2_UP:
  CALL P2_UP_PRESSED_PROC 
  jmp out_p2
   P2_SKIP_UP:

    CMP Al,P2_DOWN_AsciiCode
  JE P2_DOWN
  JMP P2_SKIP_DOWN
  P2_DOWN:
  CALL P2_DOWN_PRESSED_PROC 
    jmp out_p2
   P2_SKIP_DOWN:

    CMP Al,P2_LEFT_AsciiCode
  JE P2_LEFT
  JMP P2_SKIP_LEFT
  P2_LEFT:
  CALL P2_LEFT_PRESSED_PROC 
    jmp out_p2
   P2_SKIP_LEFT:

    CMP Al,P2_RIGHT_AsciiCode
  JE P2_RIGHT
  JMP P2_SKIP_RIGHT
  P2_RIGHT:
  CALL P2_RIGHT_PRESSED_PROC 
    jmp out_p2
   P2_SKIP_RIGHT:

   CMP Al,9d
  JE P2_ENTER
  JMP P2_SKIP_ENTER
  P2_ENTER:
  CALL P2_ENTER_PRESSED_PROC 
    jmp out_p2
   P2_SKIP_ENTER:

   CMP AH,F4_SCANCODE
   JE F4_PRESSED
   JMP SKIP_F4_PRESSED
   F4_PRESSED:
    mov dx , 3F8H		; Transmit data register
    mov al,4
    out dx , al 

   jmp far ptr WELCOMESCREEN
   SKIP_F4_PRESSED:
  ;  mov dx , 3F8H		; Transmit data register
  ;  mov al,ah
	;  out dx , al
    cmp ah,ESC_ScanCode
jne not_escape
mov dx , 3F8H		; Transmit data register
mov al,1
out dx , al 
 JMP FAR PTR ENDGAME
not_escape:
mov dx , 3F8H		; Transmit data register
out dx , al 
MovCursorToLocation InlineP1,23
mov ah ,2    
mov dl,al 
int 21h  


inc InlineP1
cmp InlineP1,39
je downline2
jmp skip_downline2

downline2:
; MovCursorToLocation actual_p1_name_size+1,23
; displaystring MSG_CLEAR2
MemoryToMemoryMovByte InlineP1,actual_p1_name_size+1
inc InlineP1
mov ah,06h
mov al,1H
mov bh,00h
mov ch,23
mov cl,inlinep1
mov dh,23
mov dl,39 
int 10h

skip_downline2:

jmp skip_out_p2 
out_p2:
mov dx , 3F8H		; Transmit data register
out dx , al 
 skip_out_p2:

  Recieve:
    mov dx , 3FDH		; Line Status Register	
		in al , dx 
		AND al , 1
		JZ Far_START_GAME_LOOP
    jmp skip_Far_START_GAME_LOOP
    Far_START_GAME_LOOP:
    jmp far ptr START_GAME_LOOP
    skip_Far_START_GAME_LOOP:

	;If Ready read the VALUE in Receive data register
		mov dx , 03F8H
		in al , dx 
    ;mov ah,al 
cmp al,1 
jne not_r_escape 
jmp far ptr ENDGAME
not_r_escape:
cmp al,4 
jne not_r_f4 
jmp far ptr WELCOMESCREEN
not_r_f4:
		;mov VALUE , al
;*****************************recieve*************************************
; CMP AH,F2_SCANCODE
; JNE SKIIP_F2_SCANCODE_P
; MOV ISBLACKPLAYER,0
SKIIP_F2_SCANCODE_P:
CMP ISBLACKPLAYER,1 ;THEREFORE I AM PLAYER 2 AND THE SENDER IS PLAYER 1 SO ILL DO PLAYER 1 MOVES
JE far_PLAYERRR_2
jmp skip_far_PLAYERRR_2
far_PLAYERRR_2:
jmp PLAYERRR_2

skip_far_PLAYERRR_2:
  CMP                                                  Al,P2_UP_AsciiCode
                                 JE                    R_UP

                                 JMP                   R_SKIP_UP
   R_UP:                           
                                 CALL                  UP_PRESSED_PROC
                                 jmp r_out_ascii
   
                         
            
   R_SKIP_UP:                      CMP                 Al,P2_DOWN_AsciiCode
                                 JE                    R_DOWN
                                 JMP                   R_SKIP_DOWN
   R_DOWN:                         
                                 CALL    DOWN_PRESSED_PROC
                                 jmp r_out_ascii
   R_SKIP_DOWN:                    
                                 CMP                   Al,P2_Right_AsciiCode
                                 JE                    R_RIGHT
                                 JMP                   R_SKIP_RIGHT
   R_RIGHT:              
                                 CALL RIGHT_PRESSED_PROC
                                 jmp r_out_ascii
    
   R_SKIP_RIGHT:                   
                                 CMP                   Al,P2_Left_AsciiCode
                                 JE                    R_LEFT
                                 JMP                   R_SKIP_LEFT
   R_LEFT:                       
                                 CALL LEFT_PRESSED_PROC
                                 jmp r_out_ascii
   R_SKIP_LEFT:                    
                                 CMP                   Al , 9d ;q scancode
                                 JE                    R_Q_SELECT
                                 JMP                   R_SKIP_Q_SELECT
   R_Q_SELECT:                     
                                 
                                 CALL                  Player1_Pressed_Q
                                 jmp r_out_ascii
                                


   R_SKIP_Q_SELECT: 

MovCursorToLocation InlineP2,24
mov ah,2  
mov dl,al   
int 21h 
inc InlineP2
cmp InlineP2,39
je Rdownline2
jmp skip_Rdownline2

Rdownline2:

MemoryToMemoryMovByte InlineP2,actual_p2_name_size+1
inc inlinep2
mov ah,06h
mov al,1H
mov bh,00h
mov ch,24
mov cl,inlinep2
mov dh,24
mov dl,39 
int 10h


skip_Rdownline2:


r_out_ascii:
;hna


   ;ESC SCANCODE NOT WORKING
                                ;  CMP                   ah , ESC_ScanCode
                                ;  JE                    R_ENDGAME1 
                                ;  JMP R_SKIP_ENDGAME_FAR
                                ;                        R_ENDGAME1:
                                ;                        JMP FAR PTR ENDGAME
                                ;                        R_SKIP_ENDGAME_FAR:

                               ;  JMP                   R_SKIP_ENDGAME
  ; R_SKIP_ENDGAME:                 
                                 JMP                   START_GAME_LOOP              
  ;*************************************PLAYER 2 *****************************************   
  PLAYERRR_2:   
  CMP Al,P2_UP_AsciiCode
  JE R_P2_UP 
  JMP R_P2_SKIP_UP
  R_P2_UP:
  CALL P2_UP_PRESSED_PROC 
  jmp notprint
   R_P2_SKIP_UP:

    CMP Al,P2_DOWN_AsciiCode
  JE R_P2_DOWN
  JMP R_P2_SKIP_DOWN
  R_P2_DOWN:
  CALL P2_DOWN_PRESSED_PROC 
    jmp notprint
   R_P2_SKIP_DOWN:

    CMP Al,P2_LEFT_AsciiCode
  JE R_P2_LEFT
  JMP R_P2_SKIP_LEFT
  R_P2_LEFT:
  CALL P2_LEFT_PRESSED_PROC 
    jmp notprint
   R_P2_SKIP_LEFT:

    CMP Al,P2_RIGHT_AsciiCode
  JE R_P2_RIGHT
  JMP R_P2_SKIP_RIGHT
  R_P2_RIGHT:
  CALL P2_RIGHT_PRESSED_PROC 
    jmp notprint
   R_P2_SKIP_RIGHT:

   CMP Al,9d
  JE R_P2_ENTER
  JMP R_P2_SKIP_ENTER
  R_P2_ENTER:
  CALL P2_ENTER_PRESSED_PROC 
    jmp notprint
   R_P2_SKIP_ENTER:

   CMP Al,4
   JE R_F4_PRESSED
   JMP R_SKIP_F4_PRESSED
   R_F4_PRESSED:
  
   jmp far ptr WELCOMESCREEN
   
   
R_SKIP_F4_PRESSED:
cmp al,1  
jne not_r_escsss 
jmp ENDGAME
not_r_escsss:

MovCursorToLocation InlineP2,24
mov ah,2  
mov dl,al   
int 21h 
inc InlineP2
cmp InlineP2,39
je Rdownline22
jmp skip_Rdownline22

Rdownline22:

MemoryToMemoryMovByte InlineP2,actual_p2_name_size+1
inc inlinep2

mov ah,06h
mov al,1H
mov bh,00h
mov ch,24
mov cl,inlinep2
mov dh,24
mov dl,39 
int 10h

skip_Rdownline22:

notprint: 


                              JMP                   START_GAME_LOOP
                   

   ENDGAME:                      

                                 mov                   ah,0
             
                                 int                   16h

   ;Change to Text MODE
                                 MOV                   AH,0
                                 MOV                   AL,03h
                                 INT                   10h
    
   ; return control to operating system
                                 MOV                   AH , 4ch
                                 MOV                   AL , 01H
                                 INT                   21H
    
                                 HLT
MAIN ENDP
    end main