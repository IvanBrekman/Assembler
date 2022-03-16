;==============================================================================
; Printf lib                                                     (c) IvanBrekan
;                                                       Last update: 16.03.2022
;
; File contains logic for printf function which print string to STDOUT
; replacing specificators
;
; Implemented specificators:
;       %%      - print '%' symbol
;       %c      - print char
;       %s      - print string
;       %d      - print decimal number
;       %b      - print binary number
;       %o      - print octal number
;       %x      - print hex number (lower case)
;       %X      - print hex number (upper case)
;       %m      - print 'mrrr' phrase with random 'r' amount
;
;
; P.S.  I want to be a cat who will always sleep, and he will be loved,
;       fed and petted, and not this is all..
;==============================================================================


;================================= CONSTANTS ==================================
%define     CODE_P      '%'         ; <---------------------+
%define     CODE_B      'b'
%define     CODE_C      'c'
%define     CODE_D      'd'         ; printf specificators  |
%define     CODE_O      'o'
%define     CODE_S      's'
%define     CODE_X      'x'
%define     CODE_XX     'X'
%define     CODE_M      'm'         ; <---------------------+

%define     STR_END     0x00        ; '\0'
%define     STR_NEW     0x0A        ; '\n'
%define     MIN_MUR     3           ; Min 'r' symbols in 'mrrr' phrase
%define     MAX_MUR     20          ; Max 'r' symbols in 'mrrr' phrase

%define     REG_BYTES   8           ; max size of reg (in bytes)

%define     STDIN       0           ; <---------------------+
%define     STDOUT      1           ; files destcriptors    |
%define     STDERR      2           ; <---------------------+

%define     sys_WRITE   0x01        ; system write func
%define     sys_EXIT    0x3C        ; system exit  func

%define     _UNKNOW_SPECIFICATOR    0x0BAD
;==============================================================================


;=================================== MACRO ====================================
;------------------------------------------------------------------------------
; Macro print prefix to numbers
;
; NOTE:  Macro change first cell by NUMBER address
;
; ENTRY: prefix - prefix symbol
; DESTR: NONE
;------------------------------------------------------------------------------
%macro PRINT_PREFIX 1
            push rsi                    ; Save rsi
            mov  rsi, NUMBER

            mov  byte [NUMBER], '0'     ; print '0'
            call print_symbol

            mov  byte [NUMBER], %1      ; print prefix
            call print_symbol

            pop  rsi                    ; load rsi
%endm
;------------------------------------------------------------------------------
;==============================================================================


;==================================== CODE ====================================
section     .text

global      _start
_start:     
            ; FORMAT1
            push STRING2
            push 13
            push STRING1

            mov  rsi, FORMAT1
            call printf

            ; FORMAT2
            push 0xff
            push 0xff
            push 0xff
            push 0xff

            mov  rsi, FORMAT2
            call printf

            ; FORMAT3
            push 0xff
            push 0xff
            push STRING1
            push 0xff
            push 0xff
            push 'q'
            push 0xff

            mov  rsi, FORMAT3
            call printf

            ; FORMAT4
            mov  rsi, FORMAT4
            call printf

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

            push rsi                    ; Save RSI to calculate return value

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
; ENTRY:    RBP  - char address
; RETURN:   NONE
; DESTR:    RBP
;------------------------------------------------------------------------------
print_char:
            push rsi                    ; Save regs
            
            mov  rsi, rbp               ; Load char from args
            call print_symbol
            
            pop  rsi                    ; Load regs 

            add  rbp, REG_BYTES         ; Set RBP to next ARG
            ret
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Function print string in STDOUT
;
; ENTRY:    [RBP] - string address
; RETURN:   NONE
; DESTR:    RBP
;------------------------------------------------------------------------------
print_string:
            push rsi                    ; Save regs

            mov  rsi, [rbp]

    .process_symbol:                    ; <---------------------+
            cmp byte [rsi], STR_END     ; If ([rsi] == STR_END) |
            je  .end                    ;     finish printf     |
                                        ;                       |
            call print_symbol           ;                       |
            inc  rsi                    ;                       |
                                        ;                       |
            jmp  .process_symbol        ; print string ---------+
            
    .end:
            pop  rsi                    ; Load regs

            add  rbp, REG_BYTES         ; Set RBP to next ARG
            ret
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Function print binary number
;
; ENTRY:    [RBP] - number
; RETURN:   NONE
; DESTR:    RBP
;------------------------------------------------------------------------------
print_number_bin:
            PRINT_PREFIX 'b'
            mov  r10, 2
            mov  r8,  DIGITS_LOW

            call print_number

            add  rbp, REG_BYTES         ; Set RBP to next ARG
            ret
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Function print octal number
;
; ENTRY:    [RBP] - number
; RETURN:   NONE
; DESTR:    RBP
;------------------------------------------------------------------------------
print_number_oct:
            PRINT_PREFIX ''
            mov  r10, 8
            mov  r8,  DIGITS_LOW

            call print_number

            add  rbp, REG_BYTES         ; Set RBP to next ARG
            ret
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Function print decimal number
;
; ENTRY:    [RBP] - number
; RETURN:   NONE
; DESTR:    RBP
;------------------------------------------------------------------------------
print_number_dec:
            mov  r10, 10
            mov  r8,  DIGITS_LOW

            call print_number

            add  rbp, REG_BYTES         ; Set RBP to next ARG
            ret
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Function print hex number (lower letters)
;
; ENTRY:    [RBP] - number
; RETURN:   NONE
; DESTR:    RBP
;------------------------------------------------------------------------------
print_number_hex_low:
            PRINT_PREFIX 'x'
            mov  r10, 16
            mov  r8,  DIGITS_LOW

            call print_number

            add  rbp, REG_BYTES         ; Set RBP to next ARG
            ret
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Function print hex number (upper letters)
;
; ENTRY:    [RBP] - number
; RETURN:   NONE
; DESTR:    RBP
;------------------------------------------------------------------------------
print_number_hex_upp:
            PRINT_PREFIX 'x'
            mov  r10, 16
            mov  r8,  DIGITS_UPP

            call print_number

            add  rbp, REG_BYTES         ; Set RBP to next ARG
            ret
;------------------------------------------------------------------------------
    
;------------------------------------------------------------------------------
; Function print number in required radix
;
; ENTRY:    [RBP] - number
;           R10   - radix
; RETURN:   NONE
; DESTR:    NONE
;------------------------------------------------------------------------------
print_number:
            push rax                ; <---------------------+
            push rbx                ;                       |
            push rcx                ; Save regs             |
            push rdx                ;                       |
            push rsi                ; <---------------------+

            mov  qword rax, [rbp]
            mov  rdi, NUMBER        ; Prepare regs for ito call

            call itoa

            mov  qword [rbp], NUMBER
            call print_string       ; Print number to STDOUT
            sub  rbp, REG_BYTES     ; Balance RBP

            pop  rsi                ; <---------------------+
            pop  rdx                ;                       |
            pop  rcx                ; Load regs             |
            pop  rbx                ;                       |
            pop  rax                ; <---------------------+

            ret
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Function convert integer to string
;
; ENTRY:    RAX - number
;           R10 - radix
;           R8  - DIGITS address
;           RDI - address to string, where number will be written
; RETURN:   NONE
; DESTR:    RAX RBX RCX RDX RSI
;------------------------------------------------------------------------------
itoa:       push rdi                ; Save RDI
            mov  rsi, rdi           ; Save string start
            xor  rcx, rcx           ; Set RCX counter = 0

    .process_digit:                 ; <-------------------------------------+
            xor  rdx, rdx           ; Clear RDX to correct DIV              |
            div  r10                ; DIV on radix                          |
                                    ;                                       |
            mov  rbx, r8    ;                                       |
            add  rbx, rdx           ; Set RBX to needed digit in DIGITS     |
                                    ;                                       |
            push rax                ; Save number value (RAX)               |
            mov  al, [rbx]          ;                                       |
            stosb                   ; Print digit                           |
            pop  rax                ; Load number value (RAX)               |
                                    ;                                       |
            inc  rcx                ; Update counter                        |
                                    ;                                       |
            cmp  rax, 0             ;                                       |
            jne  .process_digit     ; repeat... ----------------------------+

            mov  byte [rdi], STR_END; Set STRING_END to result number
            dec  rdi
            shr  rcx, 1             ; len(string) / 2

            cmp  rcx, 0             ; If (len(string) == 0)
            je   .end               ;     no need to reverse so finish itoa
    
    .reverse:                       ; <-------------------------------------+
            mov al, [rdi]           ;                                       |
            xchg al, [rsi]          ; Swap symbols                          |
            mov [rdi], al           ;                                       |
                                    ;                                       |
            inc rsi                 ; Update ptrs                           |
            dec rdi                 ;                                       |
            loop .reverse           ; reverse string -----------------------+

    .end:
            pop rdi                 ; Load RDI
            ret
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Function generate random value from RBX to (RBX+RCX)
;
; ENTRY:  RBX - start value
;         RCX - diaposon length
;
; RETURN: RAX - random value
; DESTR:  RAX RDX
;------------------------------------------------------------------------------
random:     rdtsc                       ; Get random value

            xor  rdx, rdx               ; Clear RDX before DIV
            div  rcx

            mov  rax, rdx               ; Set remain
            add  rax, rbx               ; Add start value

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
            call print_string       ; Print error start msg

            call print_symbol       ; Print unknown specificator

            mov  qword [rbp], ERROR_END
            call print_string       ; Print error end msg

            mov  rdi, _UNKNOW_SPECIFICATOR
            jmp  ABORT              ; Set exit_code and ABORT from program

            ret
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Function doing nothing (just purrrrrrring :))
;
; ENTRY:    NONE
; RETURN:   NONE
; DESTR:    NONE
;------------------------------------------------------------------------------
purrrrrrr:  ret
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
;------------------------------------------------------------------------------
murrrrrrr:
            push rax                    ; <---------------------+
            push rbx                    ;                       |
            push rcx                    ; Save regs             |
            push rdx                    ;                       |
            push rsi                    ; <---------------------+

            mov  rbx, MIN_MUR           ; Prepare regs for random call
            mov  rcx, MAX_MUR - MIN_MUR + 1

            call random

            mov  rsi, NUMBER

            mov  byte [NUMBER], 'm'     ; print 'm'
            call print_symbol

            mov  byte [NUMBER], 'r'     ; print 'r' RCX times
.mr_sound:  call print_symbol

            dec  rax
            cmp  rax, 0
            jne  .mr_sound

            pop  rsi                    ; <---------------------+
            pop  rdx                    ;                       |
            pop  rcx                    ; Load regs             |
            pop  rbx                    ;                       |
            pop  rax                    ; <---------------------+

            ret
;------------------------------------------------------------------------------
;==============================================================================


;==================================== DATA ====================================
section     .data

DIGITS_LOW  db "0123456789abcdef"
DIGITS_UPP  db "0123456789ABCDEF"
NUMBER      times 64 db "0"         ; Allocated memory for number string
            db 0

; Unknown specificator message
ERROR_STR   db STR_NEW, "Unknown specificator, got '%", STR_END
ERROR_END   db "'", STR_NEW, STR_END

FORMAT1     db STR_NEW, "Hi '%s', it is my %d message! %s", STR_NEW, STR_NEW, STR_END
FORMAT2     db STR_NEW, "%d(10) = %X(16) = %o(8) = %b(2)", STR_NEW, STR_NEW, STR_END
FORMAT3     db STR_NEW, "%% %b %c %d %o %s %x %X", STR_NEW, STR_NEW, STR_END
FORMAT4     db STR_NEW, "'%m'", STR_NEW, STR_NEW, STR_END
STRING1     db "IvanBrekman", STR_END
STRING2     db "bye...", STR_END

;####################### JUMP TABLE #######################
JUMP_TABLE:
times       CODE_P                  dq roarrrrrrr
                                    dq print_symbol
times       CODE_XX - CODE_P - 1    dq roarrrrrrr
                                    dq print_number_hex_upp
times       CODE_B - CODE_XX - 1    dq roarrrrrrr
                                    dq print_number_bin
times       CODE_C - CODE_B - 1     dq roarrrrrrr
                                    dq print_char
times       CODE_D - CODE_C - 1     dq roarrrrrrr
                                    dq print_number_dec
times       CODE_M - CODE_D - 1     dq roarrrrrrr
                                    dq murrrrrrr
times       CODE_O - CODE_M - 1     dq roarrrrrrr
                                    dq print_number_oct
times       CODE_S - CODE_O - 1     dq roarrrrrrr
                                    dq print_string
times       CODE_X - CODE_S - 1     dq roarrrrrrr
                                    dq print_number_hex_low
;##########################################################

;==============================================================================
