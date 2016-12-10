;;_____________________________________________________
;;CLEAR THE SCREEN
;;----------------------------------------------------
CLRSCRN	MACRO
	MOV AX,0600H
	MOV BX,0700H
	MOV CX,0000H
	MOV DX,184FH
	INT 10H
ENDM
;;_____________________________________________________
;;CLEAR THE FIRST LINE
;;----------------------------------------------------
CLRFRSTLINE	MACRO
	MOV AX,0600H
	MOV BX,0700H
	MOV CX,0000H
	MOV DX,024FH
	INT 10H
ENDM
;;___________________________________________________
;;SETTING CURSOR TO SPECIFIC POSITION (DECIMAL NOT HEX)
;;-----------------------------------------------------
SETCRS	MACRO ROW,COLUMN
	MOV AH,02H
	MOV BH,00H
	MOV DH,ROW
	MOV DL,COLUMN
	INT 10H
ENDM
;;____________________________________________________
;;GETTING CURSOR POSITION IN DH,DL (ROW,COLUMN)
;;-----------------------------------------------------
GETCRS	MACRO
	MOV AH,03H
	MOV BH,00H
	INT 10H
ENDM
;;_______________________________________________________
;;MOVE THE CURSER ONE CHAR LEFT
MOVCRS	MACRO
	GETCRS
	INC	DL
	SETCRS	DH,DL
ENDM
;;_______________________________________________________
;;CHANGE THE VIDEO MODE TO CGA TEXT
;;-----------------------------------------------------
CHMODTXT	MACRO
	MOV AX,0003H
	INT 10H
ENDM
;;______________________________________________________
;;CHANGE THE VIDEO MODE TO MONOCHROME
;;-----------------------------------------------------
CHMODMONO	MACRO
	MOV AX,0007H
	INT 10H
ENDM
;;______________________________________________________
;;CHANGE THE VIDEO MODE TO GRAPHIC
;;-----------------------------------------------------
CHMODMGRAPHIC	MACRO
	MOV AX,0004H
	INT 10H
ENDM
;;______________________________________________________
;;CHANGE THE VIDEO MODE TO GRAPHIC2
;;-----------------------------------------------------
CHMODGFC	MACRO
	MOV AX,0012H
	;MOV AX,0013H
	INT 10H
ENDM
;;______________________________________________________
;;WAIT TILL GET KEY PRESSED
;;-----------------------------------------------------
GETKEYW	MACRO
	MOV AH,00H
	INT 16H
ENDM
;;______________________________________________________
;;DON'T WAIT TILL GET KEY PRESSED
;;-----------------------------------------------------
GETKEY	MACRO

;CHECK:	
	MOV AH,01H
	INT 16H	
	;JZ CHECK
ENDM
;;______________________________________________________


	









;;_____________________________________________________
;;DISPLAY STRING
;;-----------------------------------------------------
SOUT	MACRO	MESSAGE
	MOV AH,09H
	MOV DX,OFFSET MESSAGE
	INT 21H
ENDM
;;___________________________________________________
;;DISPLAY CHAR
;;-----------------------------------------------------
COUT	MACRO	CHAR
	MOV AH,02H
	MOV DL,CHAR
	INT 21H
ENDM
;;___________________________________________________
;;PTING RED CHAR
COUTC	MACRO	CHAR
	MOV 	AH,09
	MOV	AL,CHAR
	MOV	CX,1
	MOV	BH,0
	MOV	BL,4
	INT	10H
ENDM
;;_______________________________________________________
;;PRINT WHITE CHAR
COUTX	MACRO	CHAR
	MOV 	AH,09
	MOV	AL,CHAR
	MOV	CX,1
	MOV	BH,0
	MOV	BL,0FH
	INT	10H
ENDM
;;_______________________________________________________
;;DISPLAY STRING INDEX
;;-----------------------------------------------------
SOUTX	MACRO	INDX
	MOV AH,09H
	MOV DX,INDX
	INT 21H
ENDM


;;___________________________________________________
;;DISPLAY ENTER
;;-----------------------------------------------------
ENDL	MACRO	MESG
	MOV AH,09H
	MOV DX,OFFSET MESG
	INT 21H
ENDM

;;______________________________________________________
;;MAKE BELL SOUND!
;;-----------------------------------------------------
BELL	MACRO
	MOV AH,02H
	MOV DL,07H
	INT 21H
ENDM
;;_______________________________________________________
;;READ ONE CHAR AND PUT IT IN AL WTHOUT ECHO
;;-----------------------------------------------------
CIN	MACRO
	MOV AH,07H
	INT 21H
ENDM
;;_______________________________________________________
;;READ FROM KEYBOARD
;;-----------------------------------------------------
SIN	MACRO 	INPUTY
	MOV AH,0AH
	MOV DX,OFFSET INPUTY
	INT 21H
ENDM
;;_______________________________________________________
;;DISPLAY THE INPUT DATA 
;;-----------------------------------------------------
SINOUT	MACRO	INPUTY
	MOV AH,09H
	MOV DX,OFFSET INPUTY+2
	INT 21H
ENDM
;;_______________________________________________________
;;DISPLAY THE NUMBER IN AX
;;-------------------------------------------------------
DISPLAYNUM MACRO
	MOV SI,OFFSET STRNUM
	
	ADD AH,'0'
	MOV [SI],AH
	INC SI
	ADD AL,'0'
	ADD AL,'0'
	MOV [SI],AL
	MOV AH,09H
	MOV DX,OFFSET STRNUM
	INT 21H



ENDM
;;_______________________________________________________
;;READ STRING AND CONVERT IT TO NUMBER AND SAVE IT TO AX
;;-------------------------------------------------------
READNUM MACRO


ENDM







;;COLOURS










;;___________________________________________________
;;CONVERT FROM HEX TO DEC FROM REG TO VAR UP TO 99DEC
CONVY	MACRO	CL,HOUR	
	MOV 	AH,00
	MOV 	AL,CL
	MOV	DL,0AH

	DIV	DL
	ADD	HOUR,AH
	MOV	AH,00
	DIV	DL
	MOV 	DL,10H
	MOV 	AL,AH
	MOV 	AH,00
	MUL	DL
	ADD	HOUR,AL
ENDM
;;______________________________________________________
;;CONVERT FROM NUMBER IN VAR TO STRING IN VAR UP TO 99DEC
CNTOS	MACRO	HOUR,HOURS
	MOV	SI,OFFSET HOURS
	MOV	AH,00
	MOV	AL,HOUR
	MOV	DL,10H
	DIV	DL
	ADD	AH,'0'
	ADD	AL,'0'
	MOV	[SI],AL
	MOV	[SI]+1,AH
ENDM
;;___________________________________________________________




;;_____________________________________________________
;;GET THE DIRECTION UP->1 , RT->2 , DN->3 , LF->4 NO->0
GETDIR	MACRO	AH,DIR
TESTUP:	CMP	AH,48H
	JNE	TESTDW
	MOV	AH,1H
TESTDW:	CMP	AH,50H
	JNE	TESTRT
	MOV	AH,3H
TESTRT:	CMP	AH,4DH
	JNE	TESTLF
	MOV	AH,2H
TESTLF:	CMP	AH,4BH
	JNE	TESTNO
	MOV	AH,4H
TESTNO:	CMP	AH,04H
	JBE	SETDIR
	MOV	AH,0H
SETDIR:	MOV	DIR,AH

ENDM
;;______________________________________________




;;_____________________________________________________
;;GET THE DIRECTION UP->1 , RT->2 , DN->3 , LF->4 NO->0
GETGDIR	MACRO	AH,DIR

TESTUP:	CMP	AH,48H
	JNE	TESTDW
	MOV	AH,0FBH
	JMP	SETDIR
TESTDW:	CMP	AH,50H
	JNE	TESTX
	MOV	AH,5
	JMP	SETDIR
TESTX:	MOV	AH,4DH
	JNE	TESTNO
	;CHMODTXT
	MOV AX,4C00h
	INT 21H	
TESTNO:	MOV	AH,00H

SETDIR:	MOV	DIR,AH

ENDM
;;______________________________________________





;;SHIFTING THE SHAP ARRAY POINT ACCORDING TO THE DIRECTION NUMBER

SHIFTING	MACRO	DIR
MOVUP:	CMP	DIR,1
	JNE	MOVDN
	MOV	AL,[DI]
	DEC	AL
	MOV	AH,[SI]
SHFUP:	XCHG	AL,[DI]
	XCHG	AH,[SI]
	INC	DI
	INC	SI
	DEC	CH
	JNZ	SHFUP

MOVDN:	CMP	DIR,3
	JNE	MOVRT
	MOV	AL,[DI]
	INC	AL
	MOV	AH,[SI]
SHFDN:	XCHG	AL,[DI]
	XCHG	AH,[SI]
	INC	DI
	INC	SI
	DEC	CH
	JNZ	SHFDN
	
MOVRT:	CMP	DIR,2
	JNE	MOVLF
	MOV	AL,[SI]
	INC	AL
	MOV	AH,[DI]
SHFRT:	XCHG	AL,[SI]
	XCHG	AH,[DI]
	INC	SI
	INC	DI
	DEC	CH
	JNZ	SHFRT

MOVLF:	CMP	DIR,4
	JNE	DNTMV
	MOV	AL,[SI]
	DEC	AL
	MOV	AH,[DI]
SHFLF:	XCHG	AL,[SI]
	XCHG	AH,[DI]
	INC	SI
	INC	DI
	DEC	CH
	JNZ	SHFLF

DNTMV:	JMP	START

ENDM





DRAWPXL	MACRO	X,Y,C
	MOV	CX,X
	MOV	DX,Y
	MOV	AL,C
	MOV	AH,0CH
	INT	10H
	
ENDM


CoutC   MACRO  char
        mov ah,2
        mov dl,char
        int 21h
ENDM
GameMode MACRO
         mov ah,0
         mov al,12h
         int 10h
ENDM 
TextMode MACRO
         mov ah,00
         mov al,03
         int 10h 
ENDM  
ClearScreen MACRO
         mov ax,0600h
         mov bx,0700h
         mov cx,0000h
         mov dx,184Fh
         int 10h
ENDM
goDosBox MACRO 
        mov ax,4c00h
        int 21h
        ENDM

