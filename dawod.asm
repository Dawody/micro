;author : SABRY-TEAM (CUSB-COM19)
;date : 10-12-2016
;GAPS GAME
;________________________
INCLUDE macroy.inc
.MODEL SMALL
.STACK 64
.DATA

DIR		DB	?				;;WALL MOVING DIRECTION ,ACTUALY IT IS NOT HELPFUL BUT DON'T REMOVE IT =D
WALLSIZE	EQU	00FFH;0170H			;;THE WALL VERTICAL LENGTH
GATESIZE	EQU	07H				;;HALF OF GATE SIZE	(IT'S 7 RIGHT AND 7 LEFT THEN IT IS HORIZONTAL LENGTH IS 14 )
GAPSIZE		EQU	20H				;;GAP VERTICAL SIZE
WALL		DW	WALLSIZE DUP(0),'$'		;;NOT HELPFUL BUT DON'T REMOVE IT XD
GAPOSY		DW	50H,50H,50H,50H,50H		;;THE ARRAY THAT HOLD THE START POSITIONS OF THE GAPS Y-POSITION
WLPOSX		DW	007FH,00FEH,017DH,01FCH,027BH	;;THE ARRAY THAT HOOOLD THE START POSITION OF THE WALLS X-POSITION
WLS		EQU	5H				;;CONSTANT FOR WALLS AND GAPS NUMBER
AMANAT1		DW	?				;;TEMP VAR
AMANAT2		DB	?				;;AN OTHER TEMP VAR
NAME1		DB	"MOHAMED $"			;;THE NAME OF THE FIRST PLAYER
SCOR1		DB	"33",'$'			;;SCORE OF THE FIRST PLAYER
NAME2		DB	"AHMED $"			;;THE NAME OF THE SECOND PLAYER
SCOR2		DB	"77",'$'			;;SCORE OF THE SECOND PLAYER
CHAT1		DB	"CHAT1",'$'			;;CHAT OF THE FIRST PLAYER
CHAT2		DB	"CHAT2",'$'			;;CHAT OF THE SECOND PLAYER
VERTICAL_BAR	DB	"-----------------------------------------------------------------------------$"
STATUS_BAR		DB	"- STASTUS BAR",'$'	;;STATUES BAR


;;___________________
x_center   dw  8 ;x of ball's center
y_center   dw  8 ;y of ball's center 
y_value    dw  ? ;drawing ball parameters
x_value    dw  ? ;drawing ball parameters
radius     dw  8 ;ball's raduis
b_error      dw  1 ;drawing ball parameters
b_speed      dw  5 ;speed of moving balls
b_colour     db  01H ;colour of ball (1=blue)

.CODE
;;VERY IMPORTANT NOTES : SABRY MAKE THE SCREEN START FROM (0,0) AT THE LEFT TOP AND END IN (027FH,00FFH) AT THE RIGHT BUTTOM
;; 			AND THIS FOR PIXELS ONLY BECAUSE TXT IN SABRY HAVE DIFFERENT MESURMENTS !
;;			YOU WILL NOT NEED IT BECAUSE I HANDLED IT BEFORE =D
;;					_{|-|<ENJOY YA SALEH>|-|}_
MAIN	PROC NEAR 
	MOV 	AX,@DATA
	MOV	DS,AX
	
	CHMODGFC		   	 ;;GRAPHIC_MODE (CHange_MODe_GraFiC)
	;CHMODTXT			 ;;TEXT_MODE TO MAKE DEBUGING EASIER

MOVE:
	CALL	DRAWSCENE
	CALL	Draw_Circle
	CALL	control_ball

;
;EL3B HENA YA SALEH
;	NOW I DREW ALL THE SCENE (WALLS AND GAPS AND THE BALL ) ALSO I CAN MOVE THE BALL
;	THEN YOU SHOULD USE THE FOLLOING TO DETECT THE WINNING AND LOSING CONDITIONS
;	CENTRE OF THE BALL AND IT'S RADIUS
;	THE NEARIST WALL X-POSITION AND IT'S GAP Y-POSITION AND THE DIMENTIONS OF THE GATE
;	ALL IS SHOWEN IN THE DATA ا^ا
	
	CALL	SHOWTEXT	
	JMP	MOVE





GETKEYW					;;MALHASH LAZMA
	
	CHMODTXT
	MOV AX,4C00h
	INT 21H
MAIN	ENDP
	


DRAWSCENE	PROC	NEAR
	PUSH	AX
	PUSH	BX
	PUSH	CX
	PUSH	DX
	PUSH	DI
	PUSH	SI
	

	MOV	DI,OFFSET WLPOSX	 ;;DI HOLD THE ARRAY OF X POSITIONS FOR THE WALLS
	MOV	SI,OFFSET GAPOSY	 ;;SI HOLD THE ARRAY OF Y POSITIONS FOR THE GAPS



;MOVE:	
	CALL EXTRATIME			;;MAKE IT SLOW TO LET ME NOTICE THE GAME AND PLAY IT
	CLRSCRN				;;CLEAR THE PLAYING SCREEN
	MOV	CL,WLS			;;COUNTER FOR WALLS

;	CALL	Draw_Circle

NEXTWAL:
	GETKEY				;;GET KEY UP OR DOWN FOR MOVING THE GAP POSOTION FOR THIS WALL ONLY
	JZ	CONTIN			;;IF NO KEY PRESSED THEN CINTINUE 
	MOV	BL,AH			;;IF KEY PRESSED STORE THE SCAN CODE FOR THAT KEY TO LET ME CLEAN THE BUFFER
	GETKEYW				;;CLEAN THE BUFFER
	MOV	AH,BL			;;GIVE ME THE KEY PRESSED SCAN CODE AGAIN 



	GETGDIR	AH,DIR			;;ACCORDING TO THE PRESSED KEY SCAN CODE ,DETECT THE MOVING DIRECTION FOR THE GAP (UP OR DOWN)
	MOV	AH,DIR			;;NOW (AH) HOLD THE VALUE I WANT TO USE FOR MOVING THE GAP UP OR DOWN
	MOV	DX,007FH		;;DX HOLD THE VALUE (7FH) TO FIND IF THE CURRENT WALL I WILL MOVE IT GAP OR NOT
	CMP	[DI],DX			
	JA	CONTIN			;;IF THE CURRENT WALL X-POSITION FROM 0 TO (7FH) ,THEN I WILL MOVE IT'S GAP
	ADD	[SI],AH			;;UP OR DOWN ACCORDING TO THE AH VALUE (IT MAY BY POSITIVE OR NEGATIVE)
	

CONTIN:	CALL	DRAWWALL		;;NOW I HAVE ALL THE INFORMATION NEEDED TO DRAW ONE WALL IN SINGLE POSITION
	MOV	AX,[DI]			;;NOW WE WILL MOVE THE WALL X-POSITION IN THE ARRAY TO DRAW IT IN NEW POSOTION THE NEXT TIME
	CMP	AX,0002H		;;BUT IF THE X-POSITION MORE THEN 2
	JA	LFMV			;;MOVE LEFT NORMALLY
	MOV	AX,027CH		;;ELSE PUT THE WAAL X-POSITION TO THE RIGHT OF THE SCREEN (THE MOST RIGHT POINT IN THE SCREEN AT 27FH)

LFMV:	DEC	AX			;;THE NORMAL LEFT MOVING BY DECREMENT THE X-POSITION
	MOV	[DI],AX			;;STORE THE NEW POSITION OF THE CURRENT WALL
	ADD	DI,2			;;SKIP TO THE NEXT WALL
	ADD	SI,2			;;SKIP TO THE GAP OF THE NEXT WALL
	DEC	CL			;;DECREMENT THE WALLS COUNTER FOT THIS SCENE
	JNZ	NEXTWAL			;;NOW DRAW THE NEXT WALL
					
					;;AFTER DRAWING ALL THE WALLS IN THE CURRENT SCENE 
;	SUB	DI,WLS*2		;;HOLD THE FIRST WALL AGAIN
;	SUB	SI,WLS*2		;;HOLD THE GAP OF THE FIRST WALL AGAIN
;	CALL	SHOWTEXT		;;SHOW THE TEXT OF THE (SCORE,CHAT,STATUES_BAR)

;	CALL	control_ball
	
;	JMP	MOVE			;;GO TO THE NEXT SCENE WITH NEW POSITIONS 

	
	
	POP	SI
	POP	DI
	POP	DX
	POP	CX
	POP	BX
	POP	AX


DRAWSCENE	ENDP


DRAWWALL	PROC	NEAR
	PUSH	AX
	PUSH	BX
	PUSH	CX
	PUSH	DX
	PUSH	DI
	PUSH	SI
		

	MOV	BX,WALLSIZE		;;HOLD THE WALL SIZE
	MOV	DX,0000			;;THE WALL START FROM THE TOP OF THE SCREEN
DRAWPX:	MOV	AMANAT1,BX		;;THE NEXT MACRO WILL USE BX SO I WILL STORE IT IN AMANAT1
	DRAWPXL	[DI],DX,1		;;DRAW SINGLE PIXEL IN (X,Y,COLOR)->([DI],DX,BLUE)
	INC	DX			;;INCREMENT THE Y-POSITION TO DRAW VERTICAL LINE
	CMP	DX,[SI]			;;IF WE REACH THE GAP POSITION READY TO DRAW GAP HERE
	JNE	CONTWL			;;IF NOT CONTINUE WALL DRAWING
	CALL	DRGATE			;;IF YES DRAW THE TOP GATE
	ADD	DX,GAPSIZE		;;DRAW THE GAP NOW BY SKIPPING DRAWING WALL LINE FOR VALUE EQUAL (GAP SIZE)
	CALL	DRGATE			;;AND DRAW THE DOWN GATE
CONTWL:	MOV	BX,AMANAT1		;;BX HOLD THE WALL SIZE AGAIN
	DEC	BX			;;DECREMENT THE WALL SIZE BY ONE AND CONTINUE
	CMP	BX,0			;;IF WALL SIZE NOW EQUAL ZERO THAT MEAN -> WALL IS COMPLETED
	JA	DRAWPX			;;IF WALL IS NOT COMPLETED CONTINUE DRAWING THE WALL PIXELS


	POP	SI
	POP	DI
	POP	DX
	POP	CX
	POP	BX
	POP	AX
	RET
DRAWWALL	ENDP

;;_______________________________________________________________________________________________________
;;THIS PROCEEDURE HAVE NO IMPORTANCE IN THE GAME TILL NOW

FINISH	PROC	NEAR
	CHMODTXT	
	MOV	AX,4C00H
	INT	21H
	RET
FINISH	ENDP
;___________________________________________________________________________________________________




DRGATE	PROC	NEAR
				;;NEVER EDIT DX HERE (WARNING),JUST USE IT AS IT IS
	PUSH	AX
	PUSH	BX
	PUSH	CX


	MOV	CX,[DI]			;;HOLD THE GATE X-POSITION (THE X-POSITION OF THE CURRENT WALL)
	MOV	BX,GATESIZE		;;HOLD THE GATE SIZE (HORIZONTAL LENGTH) BUT FOR ONE DIRECTION (1/2 THE ACTUAL GATE LENGTH)
	
CONT2:	INC	CX			;;INCREMENT HORIZONTALLY TO DRAW THE HORIZONTAL LINE OF THE GATE
	DRAWPXL	CX,DX,1			;;DRAW PIXCEL
	DEC	BX			;;COUNTER FOR THE GATE SIZE IN THE RIGHT DIRECTION
	JNZ	CONT2			;;IF THE RIGHT DIRECTION OF THE GATE DOESN'T COMPLETED YET CONTINUE DRAWING 
	MOV	BX,GATESIZE		;;READY TO DRAW THE LEFT PART OF THE GATE
	MOV	CX,[DI]			;;CONTINUE AS THE PREVIOUS PART
CONT3:	DEC	CX
	DRAWPXL	CX,DX,1
	DEC	BX
	JNZ	CONT3
	
	POP	CX
	POP	BX
	POP	AX
	RET
DRGATE	ENDP
;;________________________________________________________________________________________________________________

SHOWTEXT	PROC	NEAR
	PUSH	AX
	PUSH	BX
	PUSH	CX
	PUSH	DX
	
	SETCRS	19H,0H	
	SOUT	NAME1
	COUT	':'
	SOUT	SCOR1
	


	SETCRS	1AH,0H	
	SOUT	CHAT1

	SETCRS	1BH,0H	
	SOUT	CHAT2
	
	SETCRS	1CH,0H
	SOUT	VERTICAL_BAR

	SETCRS	1DH,0H	
	SOUT	STATUS_BAR
	
	POP	DX
	POP	CX
	POP	BX
	POP	AX
	RET
SHOWTEXT	ENDP



;;_____________________________________________________________________________________________________
;;EXTRA TIME PROCEDURE IS JUST METHOD TO MAKE THE GAME SLOWLER TO MAKE THE PLAYER CAN PLAY AND NOTICE THE GAME ANIMATION XD
EXTRATIME	PROC	NEAR
	PUSH	AX

	MOV	AX,01FFFH
EXTRA:	DEC	AX
	JNZ	EXTRA			;;JUST LOOP :)
	
	POP	AX
	RET
EXTRATIME	ENDP
;;______________________________________________________________________________________________________

;-----------DRAW BALL----------------------------------------------

Draw_Circle 	PROC	NEAR
	PUSH	AX
	PUSH	BX
	PUSH	CX
	PUSH	DX
	PUSH	SI
	PUSH	DI

 ;       ClearScreen
        mov ax,0
        mov y_value,ax
        mov ax,radius
        mov x_value,ax
drawcircle:
        mov al,b_colour ;colour goes in al
        mov ah,0ch
     
        mov cx, x_value ;Octonant 1
        add cx, x_center ;( x_value + x_center,  y_value + y_center)
        mov dx, y_value
        add dx, y_center
        int 10h

        mov cx, x_value ;Octonant 4
        neg cx
        add cx, x_center ;( -x_value + x_center,  y_value + y_center)
        int 10h

        mov cx, y_value ;Octonant 2
        add cx, x_center ;( y_value + x_center,  x_value + y_center)
        mov dx, x_value
        add dx, y_center
        int 10h

        mov cx, y_value ;Octonant 3
        neg cx
        add cx, x_center ;( -y_value + x_center,  x_value + y_center)
        int 10h

        mov cx, x_value ;Octonant 7
        add cx, x_center ;( x_value + x_center,  -y_value + y_center)
        mov dx, y_value
        neg dx
        add dx, y_center
        int 10h

        mov cx, x_value ;Octonant 5
        neg cx
        add cx, x_center ;( -x_value + x_center,  -y_value + y_center)
        int 10h

        mov cx, y_value ;Octonant 8
        add cx, x_center ;( y_value + x_center,  -x_value + y_center)
        mov dx, x_value
        neg dx
        add dx, y_center
        int 10h

        mov cx, y_value ;Octonant 6
        neg cx
        add cx, x_center ;( -y_value + x_center,  -x_value + y_center)
        int 10h

        inc y_value
        

condition1:
        cmp b_error,0
        jg condition2
        mov cx, y_value
        mov ax, 2
        imul cx
        add cx, 1
        inc ax
        add b_error, ax
        mov bx, y_value
        mov dx, x_value
        cmp bx, dx
        ja return
        jmp drawcircle

condition2:
        dec x_value
        mov cx, y_value
        sub cx, x_value
        mov ax, 2
        imul cx
        inc ax
        add b_error, ax
        mov bx, y_value
        mov dx, x_value
        cmp bx, dx
        ja return
        jmp drawcircle
return:
	
	POP	DI
	POP	SI
	POP	DX
	POP	CX
	POP	BX
	POP	AX
RET
Draw_Circle ENDP

control_ball 	PROC	 NEAR
	PUSH	AX
	PUSH	BX
	PUSH	CX
	PUSH	DX
	PUSH	SI
	PUSH	DI

   
          mov ah,1
          int 16h
          jnz check 
          jmp notwt
check:
          mov ah,0 ;for remove key from buffer 
          int 16h

notwt:
          cmp ah,4dh
          je  exit
          cmp ah,48h
          je  UP
          cmp ah,50h
          je DOWN 
          jmp EXIT
UP:
          mov cx,b_speed
          sp1:
          dec y_center
          loop sp1
          CALL Draw_Circle
          jmp EXIT
DOWN:   
          mov cx,b_speed
          sp2:
          inc y_center
          loop sp2 
          CALL Draw_Circle



EXIT:
	POP	DI
	POP	SI
	POP	DX
	POP	CX
	POP	BX
	POP	AX
            RET
control_ball ENDP



	END	MAIN
