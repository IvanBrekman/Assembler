.model tiny
.code

LOCALS
org 100h

start:		call CLEAR_WINDOW

		mov ax, ds
		mov es, ax
		
		;push 82h

		;push 'A'
		;push 82h

		;push 10
		;push offset STR2
		;push offset STR1

		;push 13
		;push offset STR2
		;push offset STR1

		;push 8
		;push 82h

		push 82h
		push 10
		push 2a4fh
		
		call itoa_st
		
		mov ax, 0900h
		mov dx, offset STR1
		int 21h
		jmp EXIT

		mov bx, 0b800h
		mov es, bx

		mov ah, 4eh
		add ax, '0'
		mov es:[(24 * 80) * 2], ax

EXIT: 		mov ax, 4c00h
		int 21h

;=================================== STRLEN ===================================
;------------------------------------------------------------------------------
; Func calc str len
;
; Entry:  DI - str addr
; Note:   ES - segment start addr
; Return: CX - str len
; Destr:  AL
;------------------------------------------------------------------------------
strlen		proc

		xor cx, cx
		mov al, '$'		; String end symbol

@@body:		scasb			; <--------------------------------+
		jz @@finish		; if cur symbol == '$' finish func |
					;		count len cycle ---|
		inc cx			; Update str len counter	   |
		jmp @@body		; Continue counting str len 	   |
					; <--------------------------------+

@@finish:	ret
		endp
;------------------------------------------------------------------------------
		
;------------------------------------------------------------------------------
; Strlen stack wrapper
;
; Entry:  ARG1 - str addr
; Note:   ES - segment start addr
; Return: AX - str len
; Destr:  CX DI : BP SP
;------------------------------------------------------------------------------
strlen_st	proc
					; ------------ PROLOG ------------
		push bp			; Save old BP to stack
		mov  bp, sp		; Update BP = SP
		sub  sp, 0 * 2		; Allocate mem for locals
					; ---------- END PROLOG ----------

		mov di, [bp + 4]	; Load ARG1 to DI
		call strlen		; Call registers strlen
		mov ax, cx		; Load strlen result to AX
		
					; ------------ EPILOG ------------
		add sp, 0 * 2		; Clear mem for locals
		pop bp			; Load BP

		ret 1 * 2		; ret with clear params
		endp
					; ---------- END EPILOG ----------
;------------------------------------------------------------------------------
;==============================================================================


;=================================== STRCHR ===================================
;------------------------------------------------------------------------------
; Function search symbol in str
;
; Entry:  DI - str addr
;	  AH - searched symbol
; Note:   ES - segment start addr
; Return: AX - addr of searched symbol or 0, if not found
; Destr:  CX
;------------------------------------------------------------------------------
strchr		proc

		call strlen		; Get string len

		mov al, ah
		sub di, cx		; Set DI = str addr
		dec di
		repne scasb		; while (ES:[DI] != al && cx > 0)
		
		jne @@not_found		; if symbol not found
		
		mov ax, di
		dec ax			; Save symbol addr to AX
		jmp @@finish

@@not_found:	xor ax, ax		; Set AX to 0 if symbol not found

@@finish:	ret
		endp
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Strchr stack wrapper
;
; Entry:  ARG1 - str addr
;	  ARG2 - searched symbol
; Note:   ES - segment start addr
; Return: AX - addr of searched symbol or 0, if not found
; Destr:  AX CX DI : BP SP
;------------------------------------------------------------------------------
strchr_st	proc
					; ------------ PROLOG ------------
		push bp			; Save old BP to stack
		mov  bp, sp		; Update BP = SP
		sub  sp, 0 * 2		; Allocate mem for locals
					; ---------- END PROLOG ----------
		mov ah, [bp + 6]
		mov di, [bp + 4]	; Prepare registers for strchr call
		call strchr
					; ------------ EPILOG ------------
		add sp, 0 * 2		; Clear mem for locals
		pop bp			; Load BP

		ret 2 * 2		; ret with clear params
		endp
					; ---------- END EPILOG ----------
;------------------------------------------------------------------------------
;==============================================================================


;================================== STRNCPY ===================================
;------------------------------------------------------------------------------
; Function copies n elems from src string to dest string and add '$' to the end
; 
; Entry:  DI - addr to dest str
;	  SI - addr to src str
;	  CX - n
; Note:   ES - segment start addr
; 	  DS - segment start addr
; Return: NONE
; Destr:  NONE
;------------------------------------------------------------------------------
strncpy		proc

		repe movsb

		ret
		endp
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Function copies n elems from src string to dest string and add '$' to the end
; 
; Entry:  ARG1 - addr to dest str
;	  ARG2 - addr to src str
;	  ARG3 - n
; Note:   ES - segment start addr
; 	  DS - segment start addr
; Return: NONE
; Destr:  CX DI SI : BP SP
;------------------------------------------------------------------------------
strncpy_st	proc
					; ------------ PROLOG ------------
		push bp			; Save old BP to stack
		mov  bp, sp		; Update BP = SP
		sub  sp, 0 * 2		; Allocate mem for locals
					; ---------- END PROLOG ----------

		mov di, [bp + 4]
		mov si, [bp + 6]	; Prepare registers for strncpy call
		mov cx, [bp + 8]

		call strncpy
					; ------------ EPILOG ------------
		add sp, 0 * 2		; Clear mem for locals
		pop bp			; Load BP

		ret 3 * 2		; ret with clear params
		endp
					; ---------- END EPILOG ----------
;------------------------------------------------------------------------------
;==============================================================================


;================================== STRNCMP ===================================
;------------------------------------------------------------------------------
; Func compares 2 strings
;
; Entry:  SI - addr to string 1
;	  DI - addr to string 2
;         CX - n
; Note:   ES - segment start addr
;	  DS - segment start addr
; Return: AX - 0 if str are equal else 1
; Destr:  NONE
;------------------------------------------------------------------------------
strncmp		proc

		repe cmpsb		; Cmp 2 strings
		jnz @@not_equal		; If not equal
		
		mov ax, 0		; If equal set AX = 0
		jmp @@finish
		
@@not_equal:	mov ax, 1		; If not equal set AX = 1

@@finish:	ret
		endp
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Strncmp stack wrapper
;
; Entry:  ARG1 - addr to string 1
;	  ARG2 - addr to string 2
;         ARG3 - n
; Note:   ES - segment start addr
;	  DS - segment start addr
; Return: AX - 0 if str are equal else 1
; Destr:  CX DI SI : BP SP
;------------------------------------------------------------------------------
strncmp_st	proc
					; ------------ PROLOG ------------
		push bp			; Save old BP to stack
		mov  bp, sp		; Update BP = SP
		sub  sp, 0 * 2		; Allocate mem for locals
					; ---------- END PROLOG ----------
		
		mov si, [bp + 4]
		mov di, [bp + 6]	; Prepare registers for strncmp call
		mov cx, [bp + 8]

		call strncmp
					; ------------ EPILOG ------------
		add sp, 0 * 2		; Clear mem for locals
		pop bp			; Load BP

		ret 3 * 2		; ret with clear params
		endp
					; ---------- END EPILOG ----------
;------------------------------------------------------------------------------
;==============================================================================


;=================================== ATOI =====================================
;------------------------------------------------------------------------------
; Function convert string to integer
;
; Entry:  SI - addr to string
;	  BX - radix
; Note:   ES - segment start addr
;	  DS - segment start addr
; Return: AX - converted number
; Destr:  CX DX DI
;------------------------------------------------------------------------------
atoi		proc

		xor dx, dx
		
@@body:		lodsb			; Load next digit
		mov ah, al
		mov di, offset DIGITS	; Prepare registers for strchr call

		call strchr
		je @@finish		; if symbol is not (0-9a-f) finish func

		sub ax, offset DIGITS	; Get symbol value

		cmp ax, bx		; if symbol value (AX) >= radix (BX)
		jge @@finish		; finish func

		mov cx, ax		; Save AX
		mov ax, dx		; Move cur num to AX
		mul bx			; Mul cur num with radix (num * radix)
		add ax, cx		; Add digit to number
		mov dx, ax		; Save num to DX

		jmp @@body

@@finish:	mov ax, dx		; Load result to AX

		ret
		endp
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Function convert string to integer (if radix is degree of 2)
;
; Entry:  SI - addr to string
;	  BX - degree of 2
; Note:   ES - segment start addr
;	  DS - segment start addr
; Return: AX - converted number
; Destr:  CX DX DI
;------------------------------------------------------------------------------
atoi_2		proc

		xor dx, dx
		
@@body:		lodsb			; Load next digit
		mov ah, al
		mov di, offset DIGITS	; Prepare registers for strchr call

		call strchr
		je @@finish		; if symbol is not (0-9a-f) finish func

		sub ax, offset DIGITS	; Get symbol value

		mov cl, bl		; Move degree of 2 to CL
		shl dx, cl		; Mul num with radix by left shift
		add dx, ax		; Add digit to num

		jmp @@body

@@finish:	mov ax, dx		; Load result to AX

		ret
		endp
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Atoi stack wrapper
;
; Entry:  ARG1 - addr to string
;	  ARG2 - radix
; Note:   ES - segment start addr
;	  DS - segment start addr
; Return: AX - converted number
; Destr:  CX DX DI
;------------------------------------------------------------------------------
atoi_st		proc
					; ------------ PROLOG ------------
		push bp			; Save old BP to stack
		mov  bp, sp		; Update BP = SP
		sub  sp, 0 * 2		; Allocate mem for locals
					; ---------- END PROLOG ----------
		mov si, [bp + 4]
		mov bx, [bp + 6]	; Prepare regosters for atoi call
		call atoi
					; ------------ EPILOG ------------
		add sp, 0 * 2		; Clear mem for locals
		pop bp			; Load BP

		ret 2 * 2		; ret with clear params
		endp
					; ---------- END EPILOG ----------
;------------------------------------------------------------------------------
;==============================================================================


;=================================== ITOA =====================================
;------------------------------------------------------------------------------
; Function convert integer to string
;
; Entry:  AX - number
;	  CX - radix
;	  DI - addr to string, where number will be written
; Note:   ES - segment start addr
; Return: NONE
; Destr:  BX DX SI
;------------------------------------------------------------------------------
itoa		proc
		
		mov si, di		; Save str addr

@@body:		xor dx, dx		; Zero DX for clear div
		div cx
	
		mov bx, offset DIGITS
		add bx, dx		; Set BX to needed digit in DIGITS

		mov bx, [bx]
		mov es:[di], bl		; Set symbol in string
		inc di
		
		cmp ax, 0
		jne @@body		; If number (AX) is zero finish func

		mov byte ptr es:[di], '$'
		mov di, si		; Add '$' to the end and load str addr
		call strrvs		; to DI. Reverse string to get right
					; result
		ret
		endp
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Itoa stack wrapper
;
; Entry:  ARG1 - number
;	  ARG2 - radix
;	  ARG3 - addr to string, where number will be written
; Note:   ES - segment start addr
; Return: NONE
; Destr:  BX DX SI : BP SP
;------------------------------------------------------------------------------
itoa_st		proc
					; ------------ PROLOG ------------
		push bp			; Save old BP to stack
		mov  bp, sp		; Update BP = SP
		sub  sp, 0 * 2		; Allocate mem for locals
					; ---------- END PROLOG ----------
		mov ax, [bp + 4]
		mov cx, [bp + 6]	; Prepare registers for itoa call
		mov di, [bp + 8]
		
		call itoa
					; ------------ EPILOG ------------
		add sp, 0 * 2		; Clear mem for locals
		pop bp			; Load BP

		ret 3 * 2		; ret with clear params
		endp
					; ---------- END EPILOG ----------
;------------------------------------------------------------------------------
;==============================================================================


;------------------------------------------------------------------------------
; Function reversed string
;
; Entry:  DI - str addr
; Note:   ES - segment start addr
; Return: NONE
; Destr:  AX CX SI
;------------------------------------------------------------------------------
strrvs		proc

		mov si, di		; Set SI to first symbol

		call strlen
		mov ax, cx		; Save str len to AX
		
		sub di, 2		; Set DI to last symbol

		mov cx, 2
		div cx			; Calc str len / 2

@@body:		mov  cl, es:[si]
		xchg cl, es:[di]	; Change 2 symbols by CL reg
		mov  es:[si], cl

		inc si
		dec di			; Update SI, DI pointers
		dec al			; Update sycle counter

		jne @@body

		ret
		endp
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Clear all window
;
; Entry: None
; Exit:	 None
; Destr: AL, CX, ES, SI
;------------------------------------------------------------------------------
CLEAR_WINDOW	proc
		mov si, 0b800h
		mov es, si
		mov al, ' '
		
		mov si, 80
@@body:		mov cx, 80
		
		cld
		rep stosw

		sub si, 1
		cmp si, 0
		jne @@body

		ret
		endp
;------------------------------------------------------------------------------

.data
DIGITS		db '0123456789abcdef'

STR1		db 'HELLO NIGGERS$'
STR2		db 'BYE   NIGGERS$'

end start
