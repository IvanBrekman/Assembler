.186
.model tiny
.code

org 100h

start:		cli
		xor di, di
		mov es, di

		mov di, 09h * 4

		mov ax, es:[di]
		mov old09ofs, ax
		mov ax, es:[di+2]
		mov old09seg, ax

		mov es:[di], offset NEW09

		push cs
		pop ax
		mov es:[di+2], ax
		
		sti

do_wait:	in al, 60h
		cmp al, 1
		jne do_wait

		mov ax, 3100h
		mov dx, offset HAPPY_END
		shr dx, 4
		inc dx
		int 21h

;------------------------------------------------------------------------------
; Resident of 09h interruption
;------------------------------------------------------------------------------
NEW09		proc
		push ax di es

		mov di, 0b800h
		mov es, di
		mov di, (5*80 + 80/2) * 2
		mov ah, 4eh
		cld

next:		in al, 60h
		stosw			; mov es:[di], ax
		and di, 0fffh

;		mov cx, 0f0ffh
;delay:		nop
;		loop delay
;
;		cmp al, 1
;		jne next

		in al, 61h		; Send ACK to keyb
		mov ah, al
		or al, 80h
		out 61h, al
		mov al, ah
		out 21h, al
	
		mov al, 20h		; Send EOI
		out 20h, al
		
		iret

		pop es di ax

		db 0eah			; JMP FAR
old09ofs	dw 0
old09seg	dw 0

		endp
;------------------------------------------------------------------------------

HAPPY_END:

end start
