;==============================================================================
; Printf lib                                                     (c) IvanBrekan
;                                                       Last update: 16.03.2022
;==============================================================================


;================================= CONSTANTS ==================================
%define     CODE_P      '%'         ; <---------------------+
%define     CODE_B      'b'
%define     CODE_C      'c'
%define     CODE_D      'd'         ; printf specificators  |
%define     CODE_O      'o'
%define     CODE_S      's'
%define     CODE_X      'x'         ; <---------------------+

%define     STR_END     0x00        ; '\0'
%define     STR_NEW     0x0A        ; '\n'

%define     REG_BYTES   8           ; max size of reg (in bytes)

%define     STDIN       0           ; <---------------------+
%define     STDOUT      1           ; files destcriptors    |
%define     STDERR      2           ; <---------------------+

%define     sys_WRITE   0x01        ; system write func
%define     sys_EXIT    0x3C        ; system exit  func

%define     _UNKNOW_SPECIFICATOR    0x0BAD
;==============================================================================


;==================================== CODE ====================================
section     .text

global      _start
_start:     ;push ARG3
        ;    push '2'
        ;    push ARG1;

        ;    mov  esi, FORMAT
        ;    call printf

            mov rax, 5
            mov r10, 2
            mov rdi, NUMBER

            call itoa


FINISH:     mov  rax, sys_EXIT
            xor  rdi, rdi
            syscall

ABORT:      mov  rax, sys_EXIT          ; Note that user should mov exit_code
            syscall                     ; to RDI  by himself

; .functions
;------------------------------------------------------------------------------
; Function print message to STDOUT using string format
;
; ENTRY:    RSI     - format string address
;           ARGS... - format string arguments pushed into stack (in reverse 
;                     order)
; RESURN:   EAX     - format length
; DESTR:    EAX
;------------------------------------------------------------------------------
printf:     push rbp                    ; Save BP
            mov  rbp, rsp               ; Mov RBP to RSP
            add  rbp,2 *  REG_BYTES     ; Mov RBP to 1 ARG address

            push rsi

    .process_symbol:                    ; <---------------------------------+
            cmp byte [rsi], STR_END     ; If ([rsi] == STR_END)             |
            je  .end                    ;     finish printf                 |
                                        ;                                   |
            cmp byte [rsi], CODE_P      ; If ([rsi] == CODE_P)              |
            je  .spec                   ;     process specificator          |
                                        ;                                   |
            call print_symbol           ; If is usual symbol - print symbol |
            inc  rsi                    ; Get next symbol address           |
                                        ;                                   |
            jmp  .process_symbol        ; repeat... ------------------------+

    .spec:                              ; Processing specificator +++++++++++
            inc  rsi                    ; Get specificator type address

            push rax                    ; Save RAX
            mov  al, [rsi]              ; Calculate shift in JUMP_TABLE
            call [JUMP_TABLE + rax * REG_BYTES]
            pop  rax                    ; Load RAX

            inc  rsi                    ; Get next symbol address
            add  rbp, REG_BYTES         ; Set RBP to next ARG

            jmp .process_symbol         ; +++++++++++++++++++++++++++++++++++

    .end:
            mov  rax, rsi
            pop  rsi,                   ; Calculate return value
            sub  rax, rsi

            pop  rbp                    ; Load BP
            ret
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Function print 1 symbol to STDOUT
;
; ENTRY:    RSI - symbol address
; RETURN:   NONE
; DESTR:    NONE
;------------------------------------------------------------------------------
print_symbol:
            push rax                ; <---------------------+
            push rdi                ; Save regs             |
            push rdx                ; <---------------------+
            
            mov rax, sys_WRITE      ; Prepare regs to 
            mov rdi, STDOUT         ; sys write call
            mov rdx, 1  

            syscall                 ; write call
            
            pop  rdx                ; <---------------------+
            pop  rdi                ; Load regs             |
            pop  rax                ; <---------------------+

            ret
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Function print char to STDOUT
;
; ENTRY:    NONE
; RETURN:   NONE
; DESTR:    NONE
;------------------------------------------------------------------------------
print_char:
            push rsi                ; Save regs
            
            mov  rsi, rbp           ; Load char from args
            call print_symbol
            
            pop  rsi                ; Load regs 

            ret
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Function print string in STDOUT
;
; ENTRY:    NONE
; RETURN:   NONE
; DESTR:    NONE
;------------------------------------------------------------------------------
print_string:
            push rsi

            mov  rsi, [rbp]

    .process_symbol:
            cmp byte [rsi], STR_END     ; If ([rsi] == STR_END)             |
            je  .end                    ;     finish printf                 |

            call print_symbol
            inc  rsi

            jmp  .process_symbol
            
    .end:
            pop  rsi                    ; Load regs
            ret
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Function print 1 symbol to STDOUT
;
; NOTE:     Function increase RBP to next printf argument
;
; ENTRY:    RSI - symbol address
; RETURN:   NONE
; DESTR:    NONE
;------------------------------------------------------------------------------
print_number_bin:
            ret
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Function print 1 symbol to STDOUT
;
; NOTE:     Function increase RBP to next printf argument
;
; ENTRY:    RSI - symbol address
; RETURN:   NONE
; DESTR:    NONE
;------------------------------------------------------------------------------
print_number_oct:
            ret
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Function print 1 symbol to STDOUT
;
; NOTE:     Function increase RBP to next printf argument
;
; ENTRY:    RSI - symbol address
; RETURN:   NONE
; DESTR:    NONE
;------------------------------------------------------------------------------
print_number_dec:
            ret
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Function print 1 symbol to STDOUT
;
; NOTE:     Function increase RBP to next printf argument
;
; ENTRY:    RSI - symbol address
; RETURN:   NONE
; DESTR:    NONE
;------------------------------------------------------------------------------
print_number_hex:
            ret
;------------------------------------------------------------------------------

print_number:
            mov  rax, [rbp]
            pop  r10
            mov  rdi, NUMBER

            call itoa

            mov  [rbp], NUMBER
            call print_string

            ret

;------------------------------------------------------------------------------
; Function convert integer to string
;
; ENTRY:    RAX - number
;           R10 - radix
;           RDI - address to string, where number will be written
; RETURN:   NONE
; DESTR:    RAX RBX RCX RDX RSI
;------------------------------------------------------------------------------
itoa:       push rdi
            mov  rsi, rdi
            xor  rcx, rcx

    .process_digit:
            xor  rdx, rdx
            div  r10

            mov  rbx, DIGITS_LOW
            add  rbx, rdx

            push rax
            mov  al, [rbx]
            stosb
            pop  rax

            inc  rcx

            cmp  rax, 0
            jne  .process_digit

            mov  byte [rdi], 0
            dec  rdi
            shr  rcx, 1
    
    .reverse:
            mov al, [rdi]
            xchg al, [rsi]
            mov [rdi], al

            inc rsi
            dec rdi
            loop .reverse

    .end:
            pop rdi
            ret
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Function print message about unknown specificator and abort program
;
; ENTRY:    NONE
; RETURN:   NONE
; DESTR:    RDI [RBP]
;------------------------------------------------------------------------------
roarrrrrrr:
            mov  qword [rbp], ERROR_STR
            call print_string

            call print_symbol

            mov  qword [rbp], ERROR_END
            call print_string

            mov  rdi, _UNKNOW_SPECIFICATOR
            jmp  ABORT

            ret
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Function doing nothing
;
; ENTRY:    NONE
; RETURN:   NONE
; DESTR:    NONE
;------------------------------------------------------------------------------
purrrrrrr:  ret
;------------------------------------------------------------------------------
;==============================================================================


;==================================== DATA ====================================
section     .data

DIGITS_LOW  db "0123456789abcdef"
DIGITS_UPP  db "0123456789ABCDEF"
NUMBER      times 64 db "0"
            db 0

ERROR_STR   db STR_NEW, "Unknown specificator, got '%", STR_END
ERROR_END   db "'", STR_NEW, STR_END

FORMAT      db STR_NEW, "Hi %n '%s', is my %c message! %s", STR_NEW, STR_NEW, STR_END
ARG1        db "IvanBrekman", STR_END
ARG2        db '1'
ARG3        db "bye...", STR_END

;####################### JUMP TABLE #######################
JUMP_TABLE:
times       CODE_P                  dq roarrrrrrr
                                    dq print_symbol
times       CODE_B - CODE_P - 1     dq roarrrrrrr
                                    dq print_number_bin
times       CODE_C - CODE_B - 1     dq roarrrrrrr
                                    dq print_char
times       CODE_D - CODE_C - 1     dq roarrrrrrr
                                    dq print_number_oct
times       CODE_O - CODE_D - 1     dq roarrrrrrr
                                    dq print_number_bin
times       CODE_S - CODE_O - 1     dq roarrrrrrr
                                    dq print_string
times       CODE_X - CODE_S - 1     dq roarrrrrrr
                                    dq print_number_hex
;##########################################################

;==============================================================================