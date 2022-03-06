.186
.model tiny
.code

org 100h
LOCALS

;================================== CONSTANTS ==================================
VIDEO_SEG 	   = 0b800h
INT_08H   	   = 08h * 4
INT_09H   	   = 09h * 4

KEYB_BUF_HEAD      = 041ah
KEYB_BUF_TAIL	   = 041ch

FRAME_START        = (FRAME_ROW * 80 + FRAME_COL) * 2
FRAME_COLOR        = 00dh
FRAME_TXT_COLOR    = 030h
FRAME_UPDATE_SHIFT = (80 - FRAME_WIDTH) * 2
FRAME_TXT_UPDATE   = 80 * 2

KEY_Q		   = 10h
;===============================================================================


;==================================== Macro ====================================
;-------------------------------------------------------------------------------
; Macro for changing original int to your resident
;
; Entry: int_address 	     - address if original int
;	 int_offset  	     - label for place, where int offset was written
;	 int_segment 	     - label for place, where int segment was written
;	 int new_int_address - label for your resident code
; Destr: AX
;-------------------------------------------------------------------------------
CHANGE_INT macro int_address, int_offset, int_segment, new_int_address
		mov ax, es:[int_address]		; Save base int offset
		mov int_offset,  ax

		mov ax, es:[int_address + 2]		; Save base int swgment
		mov int_segment, ax

		mov ax, cs				; Set current segment

		cli					;---------------------+
		mov es:[int_address], offset new_int_address    ;	      |
							; Set new int offset  |
		mov es:[int_address + 2], ax		; and segment 	      |
		sti					;---------------------+
endm
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
; Macro for resetting interruptions
;
; Entry: int_address - address if original int
;	 int_offset  - label for place, where original int offset was written
;	 int_segment - label for place, where original int segment was written
; Destr: AX BX DI SI
;-------------------------------------------------------------------------------
RESET_INT macro int_address, int_offset, int_segment
		mov di, offset int_offset		; Load base int offset 
		mov si, offset int_segment		; and segment addrs

		mov ax, [di]				; Save base int ossfset
		mov bx, [si]				; and segment values

		cli					;-------------------+
		mov es:[int_address],     ax		; Load original int |
		mov es:[int_address + 2], bx		; data		    |
		sti					;-------------------+
endm
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
; Macro set BX = FLAG value
;
; Entry: label - label to flag
; Destr: BX
;-------------------------------------------------------------------------------
GET_FLAG macro label
		mov bx, offset label
		mov bx, cs:[bx]
		cmp bx, 0
endm
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
; Macro set FLAG = VALUE value
;
; Entry: label - label to flag
;	 value - new flag value
; Destr: BX
;-------------------------------------------------------------------------------
SET_FLAG macro label, value
		mov bx, offset label
		mov byte ptr cs:[bx], value
endm
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
; Macro set VALUE to system REGISTER
;
; Entry: reg - system register
;	 val - value for sysem register
; Destr: DI
;-------------------------------------------------------------------------------
MOV_SYS macro reg, val
		mov di, val
		mov reg, di
endm
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
; Macro updates registers value
;
; Entry: None
; Destr: AX BX CX DI SI
;-------------------------------------------------------------------------------
UPDATE_REG macro reg, label
		push offset reg_val		;-----------------------------+
		push 10h			; Convert reg value to string |
		push reg			;			      |
		call itoa_st			;-----------------------------+

		mov di, offset reg_val		;-----------------------------+
		call strlen			;			      |
		mov bx, 5 + 4			; Get shift to reg str	      |
		sub bx, cx			;-----------------------------+

		mov di, offset label
		add di, bx
		mov si, offset reg_val

		call strncpy			; Copy reg value to reg str
endm
;-------------------------------------------------------------------------------
;===============================================================================


start:		;call CLEAR_WINDOW
		MOV_SYS ES, 0			; Set ES = 0

		; Load residents
		CHANGE_INT INT_08H, old_08_ofs, old_08_seg, my_INT_08H
		CHANGE_INT INT_09H, old_09_ofs, old_09_seg, my_INT_09H
		; --------------

main:		in al, 60h			;-----------------------------+
	;	jmp my_INT_08H
		inc bx
		cmp al, 1			; While pressed button != ESC |
		jne main			;-----------------------------+

EXIT:		MOV_SYS ES, 0			; Set ES = 0

		RESET_INT  INT_08H, old_08_ofs, old_08_seg
		;RESET_INT  INT_09H, old_09_ofs, old_09_seg
		; Reset original ints

		MOV_SYS es, 0			;-----------------------+
		mov bx, KEYB_BUF_HEAD		;			|
		mov cx, es:[bx]			; Clear keyboard buffer |
		mov bx, KEYB_BUF_TAIL		;			|
		mov es:[bx], cx			;-----------------------+

		mov ax, 3100h			; Function for TSR
		mov dx, offset EOF		;-----------------+
		shr dx, 4			; Calc paragraphs |
		inc dx				;-----------------+

		int 21h

;------------------------------------------------------------------------------
; Resident to 08h interrupt
; Function draws frame of regs if NEED_TO_DRAW == 1
;
; Destr: NONE
;------------------------------------------------------------------------------
my_INT_08H	proc

		push bp
		mov  bp, sp			; Save bp

		push ax bx cx dx di si es	; Save regs
		
		GET_FLAG NEED_TO_DRAW		; If NEED_TO_DRAW == 0
		jz @@not_draw			; skip drawing frame

		GET_FLAG NEED_TO_SAVE		; If NEED_TO_SAVE == 1
		jnz @@update_save		; save videomem view to buf

		
@@print:	
		MOV_SYS es, VIDEO_SEG		; Set ES to program segment
	;	call check_frame_trash
		MOV_SYS ds, cs
		MOV_SYS es, cs			; Set ES to program segment
		call update_regs		; Draw frame
		call print_regs

		
		MOV_SYS es, cs			; Set ES to program segment
		mov di, offset frame_buffer
		mov si, FRAME_START
		call mem_to_buf			; Load data from mem to buf

		jmp @@end

@@not_draw: 	GET_FLAG NEED_TO_SAVE		; If NEED_TO_DRAW == 0 and
		jnz @@end			; NEED_TO_SAVE == 1 - finish

		MOV_SYS es, VIDEO_SEG		; Set ES = VIDEO_SEG to load
		mov di, FRAME_START		; data from buf
		mov si, offset save_buffer
		call buf_to_mem			; Load data from buf to mem

		SET_FLAG NEED_TO_SAVE, 1	; Set NEED_TO_SAVE = 1

		jmp @@end

@@update_save:  MOV_SYS es, cs			; Set ES to program segment
		mov di, offset save_buffer
		mov si, FRAME_START
		call mem_to_buf			; Load data from mem to buf

		SET_FLAG NEED_TO_SAVE, 0	; Set NEED_TO_SAVE = 0
		jmp @@print

@@end:		pop es si di dx cx bx ax bp	; Load regs

		db 0eah				;----------------------+
old_08_ofs	dw 0				; JMP to original int  |
old_08_seg	dw 0				;----------------------+

		ret
		endp
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Resident to 09h interrupt
; Function set required value to NEED_TO_DRAW flag depending on pressed button
;
; Destr: NONE
;------------------------------------------------------------------------------
my_INT_09H	proc
		push ax di			; Save regs

		mov di, offset NEED_TO_DRAW	; Get flag addr

		in  al, 60h
		cmp al, KEY_Q			; Check if pressed key is Q
		jnz @@end

		xor byte ptr cs:[di], 1		; Swap flag value

@@end:		pop di ax			; Load regs

		db 0eah				;----------------------+
old_09_ofs	dw 0				; JMP to original int  |
old_09_seg	dw 0				;----------------------+

		ret
		endp
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Save videomem to buffer
;
; Entry:  DI - addr to dest str
;	  SI - addr to src str
; Note:   ES - segment dest start addr
; Return: NONE
; Destr:  CX DI SI
;------------------------------------------------------------------------------
mem_to_buf	proc

		push ds
		mov cx, VIDEO_SEG
		mov ds, cx

		mov cx, FRAME_HEIGHT
@@body:		push cx				;-----------------------+
						;			|
		mov cx, FRAME_WIDTH * 2		;			|
		call strncpy			; Save 1 string <-------|
						;			|
		add si, FRAME_UPDATE_SHIFT	; Update mem addr to new|
		pop cx				; string		|
						;			|
		loop @@body			;-----------------------+

		pop ds
		ret
		endp
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
buf_to_mem	proc

		mov cx, FRAME_HEIGHT
@@body:		push cx

		mov cx, FRAME_WIDTH * 2
		call strncpy

		add di, FRAME_UPDATE_SHIFT
		pop cx

		loop @@body

		ret
		endp
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Function updates all regs values in strings
;
; Entry: NONE
; Note:   ES - segment start addr
; Destr: AX BX CX DI SI
;------------------------------------------------------------------------------
update_regs	proc

		UPDATE_REG cs:[bp-2], str_AX
		UPDATE_REG cs:[bp-4], str_BX
		UPDATE_REG cs:[bp-6], str_CX
		UPDATE_REG cs:[bp-8], str_DX
		; UPDATE_REG ax, str_AX
		; UPDATE_REG bx, str_BX
		; UPDATE_REG cx, str_CX
		; UPDATE_REG dx, str_DX

		ret
		endp
;------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
; Function for drawing frame
;
; Entry: NONE
; Destr: AX BX CX DX DI SI ES
;-------------------------------------------------------------------------------
print_regs	proc

		mov ah, FRAME_COLOR			;----------------------+
		mov bx, offset frame_style4		;		       |
		MOV_SYS es, VIDEO_SEG			; Prepare registers for|
		mov di, FRAME_START			; frame drawing        |
							;----------------------+
		call draw_frame

		mov ah, FRAME_TXT_COLOR			;----------------------+
		mov si, offset str_AX			;		       |
		mov cx, VIDEO_SEG			; Prepare registers for|
		mov dx, FRAME_START			; drawing text in frame|
							;----------------------+
		call print_text

		ret
		endp
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
; Entry: None
;-------------------------------------------------------------------------------
check_frame_trash proc

		mov di, FRAME_START
		mov si, offset frame_buffer
		mov bx, offset  save_buffer

		mov cx, FRAME_HEIGHT
@@body:		push cx

		mov cx, FRAME_WIDTH * 2

@@line_cycle:	mov dx, cx
		xor ax, ax
		call strncmp
		jnz @@not_equal

		add di, FRAME_UPDATE_SHIFT
		pop cx

		loop @@body
		jmp @@end

@@not_equal:	dec di
		mov ax, es:[di]
		sub dx, cx
		dec dx
		add bx, dx
		mov byte ptr cs:[bx], al

		inc bx
		inc di
		jmp @@line_cycle

@@end:		ret
		endp
;-------------------------------------------------------------------------------

.data
NEED_TO_DRAW	dw 0				; Flag for drawing frame
NEED_TO_SAVE	dw 1				; Flag for saving data in
						; videomem before drawing frame

str_AX 		db 'AX = 0000'			;--------------------------+
str_BX 		db 'BX = 0000'			; Buffers for text in frame|
str_CX 		db 'CX = 0000'			;			   |
str_DX 		db 'DX = 0000$'			;--------------------------+
reg_val		dw 4 dup (0)			; Buffer to regs values

; Use      FRAME_WIDTH * FRAME_HEIGHT
;		    ||   |
frame_buffer	dw (11 * 6) dup (0)		; Buffers for double
 save_buffer    dw (11 * 6) dup (0)		; bufferization

include STRLIB.asm
include FRAME.asm

EOF:						; Label to the End Of File
end start
