.model tiny
.code

LOCALS
org 100h

FRAME_ROW     = 5
FRAME_COL     = 10
FRAME_WIDTH   = 60
FRAME_HEIGHT  = 10

STRING_END    = '$'
CONSOLE_START = 80h
  VIDEO_START = 0b800h

start:		call CLEAR_WINDOW

; Draw frame
		mov bl, ds:[CONSOLE_START+2]	; Get frame type from console
		sub bx, '0'			; Convert frame type to int
		add bx, bx			; BX *= 2

		mov ax, offset styles_address	; Get address of frame style
		add bx, ax			; symbols

		mov bx, [bx]

		mov ah, 02h			; Set color attr
		mov di, (FRAME_ROW * 80 + FRAME_COL) * 2
						; Set started addr
		
		mov dx, 0b800h
		mov es, dx			; Set ES to videoseg

		call draw_frame

; Draw text
		mov ax, ds
		mov es, ax

		mov di, CONSOLE_START
		mov ax, STRING_END
		mov cx, 100

		repne scasb
		
		jne EXIT

		mov ah, 04h
		mov si, di
		mov cx, VIDEO_START
		mov dx, (FRAME_ROW * 80 + FRAME_COL) * 2
		
		call print_text
		
EXIT:		mov ax, 4c00h
		int 21h

;------------------------------------------------------------------------------
; Draws one line in a frame
; 
; Entry: AH - color attr
;	 BX - addr of style string fragment
;        CL - line length (frame width)
;        DI - starting addr to draw
; 	 ES - videoseg addr
; Exit:	 None
; Destr: AL CX DX DI
;------------------------------------------------------------------------------
draw_line	proc
		mov al, [bx+0]		; Get starting symbol
		stosw			; AX = attr:symb

		mov al, [bx+1]		; Get filling symbol
		sub cl, 2		; Substract line length (CL)
					; to fill this symbol N-2 times

		jcxz @@BAD_CX		; Jump if CX = Zero
		
		cld			; CLear Destination flag		
		rep stosw		; Repeat stosw CX times

		mov al, [bx+2]		; Draw ending symbol
		stosw

		ret

@@BAD_CX:	mov ah, 09h
		mov dx, offset BAD_CX_MSG
		int 21h			; Print error msg
		jmp EXIT
		endp

;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Draws frame: width  - FRAME_WIDTH
;	       height - FRAME_HEIGHT
; 
; Entry: AH - color attr
;	 BX - addr of style frame fragment
;	 DI - starting addr to draw
;	 ES - videoseg addr
; Exit: None
; Destr: CL SI
;------------------------------------------------------------------------------
draw_frame	proc

		mov cl, FRAME_WIDTH	; Set line width
		call draw_line		; Draw first frame line
		
		add bx, 3		; Update drawing symbols
		mov si, FRAME_HEIGHT - 2; Set cycle counter in SI

frame_body:	mov cl, FRAME_WIDTH	; Update line width---------
		add di, (80 - FRAME_WIDTH) * 2
					; Set DI to new line	   |
		call draw_line		;			   |
					;      Frame cycle <-------|
		dec si			; Check if continue cycle  |
		cmp si, 0		;			   |
		jne frame_body		; Jmp to cycle body---------
 
		add bx, 3		; Update drawing symbols
		mov cl, FRAME_WIDTH	; Update line width
		add di, (80 - FRAME_WIDTH) * 2
					; Set DI to new line
		call draw_line		; Draw last frame line

		ret
		endp
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Set text into the frame
; Entry: AH - color attr
;	 SI - str addr
;	 CX - videoseg addr
;	 DX - frame_start addr
;	
; Note: DS - segment start addr
; Return: None
; Destr: AX BX ES DI
;------------------------------------------------------------------------------
print_text	proc
		
		add dx, (80 + 2) * 2	; Set DX to str start addr in frame
		
		mov bx, ds
		mov es, bx		; Set ES DI to str addr to calc str len
		mov di, si		;

		call strlen
		cmp bx, 0
		je @@end_print		; if str len == 0 finish

		mov es, cx		; Set ES DI to str start addr in frame
		mov di, dx		;
		
		mov cx, FRAME_WIDTH - 4	; Set str line length
		mov dx, FRAME_HEIGHT - 2; Set lines amount for str

@@put_sym:	lodsb			; Put symbol <---------------------|
		stosw			;				   |
					;				   |
		dec bx			; Update counters		   |
		dec cx			;				   |
					;			     	   |
		cmp cx, 0		; Make new line if str line length |
		je @@new_line		; counter == 0			   |
					;	Put symbol cycle ----------|
@@check:	cmp bx, 0		;				   |
		je @@end_print		; If str len counter == 0 or	   |
		cmp dx, 0		; lines amount counter == 0	   |
		je @@end_print		; finish print			   |
					;				   |
		jmp @@put_sym           ; <--------------------------------|

@@new_line:	mov cx, FRAME_WIDTH - 4	; Update line length counter
		dec dx			; Update lines amount counter
		add di, (80 - FRAME_WIDTH + 4) * 2
		jmp @@check		; Set DI to addr of new line for str

@@end_print:	ret
		endp
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Calc string len
;
; Entry: DI - str addr
; Return: BX - string len
; Destr: AL
;------------------------------------------------------------------------------
strlen		proc

		mov al, STRING_END
		xor bx, bx		; Clear str len counter
		
@@body:		scasb			; <-----------------------------|
		jz @@finish		; While cur symbol != '$'	|
					;		Calc len cycle--|
		inc bx			; Update str len counter	|
		jmp @@body		; <-----------------------------|

@@finish:	ret
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
BAD_CX_MSG	db 'CX register is ZERO$'

frame_style1	db 0feh, 0c4h, 0feh, 0b3h, ' ', 0b3h, 0feh, 0c4h, 0feh
frame_style2	db 0feh, 0cdh, 0feh, 0bah, ' ', 0bah, 0feh, 0cdh, 0feh
frame_style3  	db '+-+| |+-+'

styles_address	dw 84h, offset frame_style1, offset frame_style2, offset frame_style3

end	 	start