
[org 0x0100]
jmp start
timerflag: dw 0
miliseconds: dw 0
seconds: dw 0
minutes: dw 0
hours: dw 0
oldisr: dd 0
oldkb: dd 0
initialm: dw 'space->start & capture, R->reset&stop, esc->end',0
buffer: times 2000 dw 0
index: dw 10

strlen: push bp             ;subroutine for string lenght
mov bp,sp
push es
push cx
push di
les di, [bp+4]               ; point es:di to string
mov cx, 0xffff               ; load maximum number in cx
xor al, al                   ; load a zero in al
repne scasb                  ; find zero in the string
mov ax, 0xffff               ; load maximum number in ax
sub ax, cx                   ; find change in cx
dec ax                       ; exclude null from length
pop di
pop cx
pop es
pop bp
ret 4

;------------------------------------------------
copyscreen:           ;subroutine to copy the screen
push bp
mov bp,sp
push ax
push bx
push cx
push dx
push di
push si


mov ax,0xb800
mov ds,ax
xor si,si
push cs
pop es
mov di,[bp+4]

mov cx,2000                ;copy the whole screen into the buffer
cld
rep movsw

pop si
pop di
pop dx
pop cx
pop bx
pop ax
pop bp
ret 2

;------------------------------------------------

;-----------------------------------------------
printscreen:     ;Print the Whole Screen 
push bp
mov bp,sp
push ax
push bx
push cx
push dx
push di
push si
push cs
pop ds
mov ax,0xb800
mov es,ax
                     
mov si,[bp+4]
xor di,di
mov cx,2000
mov ax,07
cld
rep movsw      ;print the whole screen from buffer to console


pop si
pop di
pop dx
pop cx
pop bx
pop ax
pop bp
ret 2
;------------------------------------------------
clrscr: push es         ;Clear Screen Subroutine
push ax
push cx
push di
mov ax, 0xb800
mov es, ax            ; point es to video base
xor di, di            ; point di to top left column
mov ax, 0x0720        ; space char in normal attribute
mov cx, 2000          ; number of screen locations
cld                   ; auto increment mode
rep stosw             ; clear the whole screen
pop di
pop cx 
pop ax
pop es
ret
;------------------------------------------------
printstr: push bp       ;Print string Subroutine

mov bp, sp
push es
push ax
push cx
push si
push di
push ds            ; push segment of string
mov ax, [bp+4]
push ax            ; push offset of string

call strlen        ; calculate string length
cmp ax, 0          ; is the string empty
jz exit            ; no printing if string is empty
mov cx, ax            ; save length in cx
mov ax, 0xb800
mov es, ax          ; point es to video base
mov al, 80          ; load al with columns per row
mul byte [bp+8]     ; multiply with y position
add ax, [bp+10]     ; add x position
shl ax, 1           ; turn into byte offset
mov di,ax           ; point di to required location
mov si, [bp+4]      ; point si to string
mov ah, [bp+6]       ; load attribute in ah
cld                  ; auto increment mode
nextchar: lodsb      ; load next char in al
stosw                 ; print char/attribute pair
loop nextchar        ; repeat for the whole string
exit: pop di
pop si
pop cx
pop ax
pop es
pop bp
ret 8

;------------------------------------------------

             ; subroutine to print a number at the screen
             ; takes the number and position to be printed as its parameter
printnum: push bp
mov bp, sp
push es
push ax
push bx
push cx
push dx
push di
mov ax, 0xb800
mov es, ax    ; point es to video base
mov ax, [bp+4]              ; load number in ax
mov bx, 10                        ; use base 10 for division
mov cx, 0                         ; initialize count of digits
nextdigit: mov dx, 0              ; zero upper half of dividend
div bx ; divide by 10
add dl, 0x30                      ; convert digit into ascii value
push dx                           ; save ascii value on stack
inc cx                             ;  increment count of values
cmp ax, 0                       ; is the quotient zero
jnz nextdigit                       ; if no divide it again
mov di, [bp+6]  
cmp word cx,1
jne nextpos
mov ax,0x0730
mov word[es:di],ax
add di,2         ;if its a single digit number than converting it into form 00
                       

               
nextpos:
 pop dx 
                         ; remove a digit from the stack
mov dh, 0x07              ; use normal attribute
mov [es:di],dx
                          ; print char on screen
add di, 2              ; move to next screen location
loop nextpos              ; repeat for all digits on stack
pop di
pop dx
pop cx
pop bx
pop ax
pop es
pop bp
ret 4

;------------------------------------------------
printmessage:     ;starting message subroutine print on thr right most corner
push ax
mov ax,30
push ax
mov ax,0
push ax
mov ax,0xf4
push ax
mov ax,initialm
push ax
call printstr
pop ax
ret
;------------------------------------------------

printcolon:      ;subroutine to print colons 
push bp
mov bp,sp
push es
push ax
push di
mov ax,0xb800
mov es,ax

mov di,[bp+4]
add di,6
mov ax,0xf13A
mov word [es:di],ax
pop di
pop ax
pop es
pop bp
ret 2




;------------------------------------------------
initial:           ;subroutine to print initial state 00 00 00 00
push word 40
push word 0
call printnum

push word 30
push word 0
call printnum
push word 30
call printcolon

push word 20
push word 0
call printnum
push word 20
call printcolon

push word 10
push word 0
call printnum

push word 10
call printcolon
call printmessage

ret 
 
;------------------------------------------------


              ;Subroutine to printtime on desired location index is passed
printtime:
push bp
mov bp,sp
push es
push ax
push dx
push bx
mov ax,0xb800
mov es,ax
mov ax,[bp+4]           ;index to print time
push ax
mov dx,[hours]          ;hours to print on index
push dx
call printnum
push ax
call printcolon         ;colon printing

add ax,10
push ax
mov dx,[minutes]          ;mins to print on index
push dx
call printnum
push ax
call printcolon

add ax,10
push ax
mov dx,[seconds]          ;sec to print on index
push dx
call printnum
push ax
call printcolon


add ax,10
push ax
mov dx,[miliseconds]       ;miliseconds to print on index
push dx
call printnum
pop bx
pop dx
pop ax
pop es
pop bp
ret 2


exittt:
jmp exitt


;------------------------------------------------

clock:      ;subroutine for time calculations
push es
push ax
mov ax, 0xb800
mov es, ax
push word 40
push word [cs:miliseconds]
call printnum                         ;printing miliseconds

cmp word[cs:miliseconds],990
jne exittt
mov word[cs:miliseconds],0
mov word[es:44],' ' 

inc word [cs:seconds]             ; increment seconds
push word 30                    
push word [cs:seconds]
call printnum                     ;printing seconds

cmp byte[cs:seconds],59
jne exitt
mov word[cs:seconds],0            ; seconds to 0
push word 30
push word [cs:seconds]
call printnum
mov word[es:32],' '  


inc word [cs:minutes]             ; increment minutes if 59 seconds
push word 20
push word [cs:minutes]
call printnum
cmp byte[cs:minutes],59
jne exitt
mov word[cs:minutes],0            ; minutes to 0 after 59th increment 
push word 20
push word [cs:minutes]
call printnum
mov word[es:22],' ' 
inc word [cs:hours]             ; increment hours
push word 10
push word [cs:hours]
call printnum



exitt:

pop ax
pop es
ret


;------------------------------------------------ 

; keyboard interrupt service routine
kbisr: push ax
in al, 0x60                    ; read char from keyboard port
cmp byte al, 19                ; has the r key pressed
jne nextcmp1                ; no, try next comparison
mov word [cs:timerflag], 0;   ;stop the watch till releasing
mov word[cs:miliseconds],0 
mov word[cs:seconds],0          ;reseting the time and index after clearing screen
mov word[cs:minutes],0
mov word[cs:hours],0
mov word[cs:index],10
call clrscr
call initial


jmp exitkbr ; leave the ISR


nextcmp1: cmp byte al,147         ; has the r released
jne nextcmp2
mov word [cs:timerflag], 0        ; set flag to start stopwatch
jmp exitkbr ; leave the interrupt routine

nextcmp2:cmp byte al,57           ;comparing with spacebar
jne nextcmp3                      ;no,jump to next cmp
mov word [cs:timerflag], 1;        ;yes,start watch
push dx
push bx
mov bx,buffer
push bx
call copyscreen              ;copy the screen and print the whole screen after clearing
call clrscr
mov bx,buffer
push bx
call printscreen
mov dx,[index]              ;print time on the ith row
push dx
call printtime



push es
push di
push ax
xor ax,ax
mov ax,0xb800
mov es,ax                ;changing the format of milliseconds from 000 to 00
mov ax,dx
add ax,34
mov di,ax
mov word[es:di],' '
pop ax
pop di
pop es



add dx,160                  ;go to next row

cmp dx,3850                 ;if its the last row move to second row
jnz adding
mov dx,170
adding:
mov [index],dx
pop bx
pop dx
jmp exitkbr


nextcmp3:cmp byte al,185      ;releasin spacebar
jne nomatch
mov word [cs:timerflag], 1;
jmp exitkbr
nomatch: pop ax
jmp far [cs:oldkb] ; call original ISR

exitkbr: mov al, 0x20
out 0x20, al ; send EOI to PIC
pop ax
iret





;------------------------------------------------ 



; timer interrupt service routine

timer: 
push ax
cmp word[cs:timerflag],1   ;comparing timerflag
jne skipall
add word [cs:miliseconds],55
push word 40
push word [cs:miliseconds]
call printnum
mov word[es:44],' '
cmp word [cs:miliseconds],990             
jne pe
call clock 
pe:               
skipall:mov al, 0x20
out 0x20, al                        ; end of interrupt
pop ax
iret 




;------------------------------------------------ 




                               ; return from interrupt
start:

mov cx,0
call clrscr
;call printmessage
call initial
                     ;hooking the ISRs
 xor ax, ax
mov es, ax
mov ax, [es:9*4]
mov [oldkb], ax ; save offset of old routine
mov ax, [es:9*4+2]
mov [oldkb+2], ax

xor ax, ax
mov es, ax
mov ax, [es:8*4]
mov [oldisr], ax ; save offset of old routine
mov ax, [es:8*4+2]
mov [oldisr+2], ax                          ; point es to IVT base
cli  
mov word [es:9*4], kbisr ; store offset at n*4
mov [es:9*4+2], cs                                ; disable interrupts
mov word [es:8*4], timer                          ; store offset at n*4
mov [es:8*4+2], cs                 ; store segment at n*4+2
sti            
                    ; enable interrupts
l1:
 mov ah, 0                       ; service 0 – get keystroke
int 0x16                           ; call BIOS keyboard service
cmp al, 27                          ; is the Esc key pressed
jne l1                               ; if no, check for next key

xor ax,ax
mov es,ax
mov ax, [oldisr]                    ; read old offset in ax
mov bx, [oldisr+2]                ; read old segment in bx
cli                ; disable interrupts
mov [es:8*4], ax                ; restore old offset from ax
mov [es:8*4+2], bx                ; restore old segment from bx
;sti
mov ax, [oldkb]                ; read old offset in ax
mov bx, [oldkb+2]                ; read old segment in bx
;cli                ; disable interrupts
mov [es:9*4], ax                ; restore old offset from ax
mov [es:9*4+2], bx                ; restore old segment from bx
sti

              
mov ax, 0x4c00                ; terminate program
int 0x21                                 ; number of paras
