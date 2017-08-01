; created by Endre Petõ

INCLUDE Irvine32.inc

.data

	val_A		SBYTE		12h
	val_B		WORD		186
	val_C		SWORD		-42
	val_D		SDWORD		5371

	helloStr	BYTE		'AaBbCcDdEe?ll:!', 0ah, 0
	letterChangeStr	BYTE		'MamBBo Nabbd!', 0ah, 0
	intarray DWORD 100,-200,-300,400,500
	numbersInString	BYTE		'A1a2Bb3CcD4dE5e?ll:!', 0ah, 0

.code

;-------------------------------------------------------------------------------

main proc
	;CALL	Felad3a
	;CALL	Felad3b
	;CALL	Uppercase
	;CALL	Felad3b2
	;CALL	Factorial
	;CALL	Summa
	;CALL	Stringszamol
	;CALL		String
	;CALL	Tomb
	;CALL	Numtring
	;CALL	Multiply
	;CALL	Mintask
	;CALL	Absolut

	INVOKE	ExitProcess,0
main endp

;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;	3/a Feladat megoldása
;	EAX := a*(b+c+d)
;	Adatok az adat szegmensben
Felad3a	PROC

	mov		ax,val_B
	cwde
	mov		ebx,eax
	mov		ax,val_C
	cwde
	add		eax,ebx
	mov		ebx,val_D
	add		eax,ebx
	xchg		eax,ebx
	mov		eax,0
	mov		al,val_A
	cbw
	cwde
	mul		ebx

	CALL		WriteInt
	CALL		Crlf
	CALL		Crlf
	RET
Felad3a ENDP


;	EAX := (a*b)-(c*d)
;	Adatok az adat szegmensben
Multiply	PROC

	mov		ax,val_B
	cwde
	mov		ebx,eax
	mov		ax,val_C
	cwde
	mul		eax
	mov		ebx,val_D
	mov		al,val_A
	cbw
	cwde
	mul		ebx

	sub	eax,ebx

	CALL		WriteInt
	CALL		Crlf
	CALL		Crlf
	RET
Multiply ENDP

;	EAX := max(EAX - ECX, EBX + EDX)
;	Tetszõleges adatokra
Felad3b	PROC
	
	mov		eax,20
	mov		ebx,10
	mov		ecx,16
	mov		edx,10

	sub		eax,ecx
	add		ebx,edx

	cmp		eax,ebx
	jge		ugras
	xchg		eax,ebx
ugras:
	call		WriteInt

	call		Crlf
	call		Crlf
	RET
Felad3b ENDP

;	EAX := max(EAX - ECX, EBX + EDX)
;	Tetszõleges adatokra
Absolut	PROC
	mov		eax,20
	mov		ebx,10
	mov		ecx,21
	mov		edx,15

	sub		eax,ecx
	add		ebx,edx
	
ciklus:
	cmp		eax,0
	jl	negalforeax
	cmp		ebx,0
	jl	negalforebx
	cmp		eax,ebx
	jge		ugras
	xchg		eax,ebx
	jmp		ugras
negalforeax:
	neg eax
	jmp		ciklus
negalforebx:
	neg ebx
	jmp		ciklus
ugras:
	call		WriteInt

	call		Crlf
	call		Crlf
	RET
Absolut ENDP


;	EAX := min(EAX - ECX, EBX + EDX)
;	Tetszõleges adatokra
Mintask	PROC
	
	mov		eax,20
	mov		ebx,10
	mov		ecx,16
	mov		edx,10

	sub		eax,ecx
	add		ebx,edx

	cmp		eax,ebx
	jle		ugras
	xchg		eax,ebx
ugras:
	call		WriteInt

	call		Crlf
	call		Crlf
	RET
Mintask ENDP

;	EAX := Max(AX-CX, BX+DX)
;	Tetszõleges adatokra
Felad3b2	PROC
	mov		ax,12
	mov		bx,22
	mov		cx,36
	mov		dx,40

	sub		ax,cx
	add		bx,dx

	cmp		ax,bx
	jge		ugras
	xchg		ax,bx
ugras:
	call		WriteInt

	call		Crlf
	call		Crlf
	
	RET
Felad3b2 ENDP


;	ESI általá mutatott karaktersorozat kisbetûit nagyra.
Uppercase PROC
	MOV	ESI, offset helloStr
	MOV	EAX, 0
ciklus:
	MOV	AL,byte ptr [ESI]
	CMP	AL,0
	JE	strvege
	CMP	AL, 'z'
	JG	tovabb
	CMP	AL, 'a'
	JL	tovabb
	SUB	AL, 32
	JMP	tovabb
tovabb:
	CALL	WriteChar
	INC	ESI
	JMP	ciklus
strvege:
	CALL		Crlf
	CALL		Crlf
	RET
Uppercase ENDP

;	6!
;	Adatok az adat szegmensben
Factorial PROC
	MOV		EAX,1
	MOV		EBX,6
	ciklus:
	CMP		EBX,1
	JLE		vege
	MUL		EBX
	DEC		EBX
	JMP		ciklus
	vege:
	CALL		WriteDec
	CALL		Crlf
	CALL		Crlf
	RET
Factorial ENDP

;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;	(a+b)/(c+d)
Summa	PROC
	;c+d
	mov		ebx,val_D
	cwde
	mov		ax,val_C
	cwde
	add		eax,ebx


	;a+b
	mov		cl,val_A
	cwde
	mov		bx,val_B
	add		ebx,ecx
	xor		edx, edx
	
	cbw
	cwde
	div		ebx

	CALL		WriteInt
	CALL		Crlf
	CALL		Crlf
	RET
Summa ENDP

;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;	hány 'l' van a sringben
Stringszamol	PROC
	
	MOV	ESI, OFFSET hellostr
	MOV	EAX, 0
ciklus:
	MOV	BL, [ESI]
	CMP	BL,0
	JE	strvege
	CMP	BL, 'l'
	JNE	tovabb
	INC	EAX
tovabb:
	INC	ESI
	JMP	ciklus
strvege:
	CALL	WriteDec
	CALL		Crlf
	CALL		Crlf
	RET
Stringszamol ENDP

;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;	cseréli a b és B betûket szóközre
String	PROC
	MOV	ESI, OFFSET letterChangeStr
	MOV	EAX, 0
ciklus:
	MOV	AL, [ESI]
	CMP	AL,0
	JE	strvege
	CMP	AL, 'b'
	JE	convert
	CMP	AL, 'B'
	JE	convert
	JMP	tovabb
convert:
	MOV	AL,' '
	JMP	tovabb
tovabb:
	CALL	WriteChar
	INC	ESI
	JMP	ciklus
strvege:
	CALL		Crlf
	CALL		Crlf
	RET
String ENDP
	
	; Összeadja egy tömb negatív értékeit
Tomb	proc
	mov  edi,OFFSET intarray	
	mov  ecx,LENGTHOF intarray
	mov  eax,0
L1:	
	mov	ebx,0
	mov	ebx,[edi]
	CMP	ebx,0
	JLE	Osszead
	add  edi,TYPE intarray
	loop L1
	JMP strvege
Osszead:
	add  eax,[edi]
	add  edi,TYPE intarray
	JMP	L1
strvege:
	CALL	WriteInt
	CALL		Crlf
	CALL		Crlf
	RET
Tomb	endp


;	ESI általá mutatott karaktersorozatból a számokat szóközre.
Numtring PROC
	MOV	ESI, OFFSET numbersInString
	MOV	EAX, 0
ciklus:
	MOV	AL, [ESI]
	CMP	AL,0
	JE	strvege
	CMP	AL, '9'
	JG	tovabb
	CMP	AL, '0'
	JL	tovabb
	JMP	convert
convert:
	MOV	AL,'x'
	JMP	tovabb
tovabb:
	CALL	WriteChar
	INC	ESI
	JMP	ciklus
strvege:
	CALL		Crlf
	CALL		Crlf
	RET
Numtring ENDP

;===============================================================================
.stack
			dw		1000	dup	(?)

end main
