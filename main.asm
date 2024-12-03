.MODEL small
.STACK 100h
.DATA
.CODE
row DW 190
barPos DW 130
barLen DW 60
ballx DW 160
bally DW 100
ballVelX DW 1
ballVelY DW 1
barSpeed DW 5
MAIN PROC FAR
    mov ax, @DATA
    mov ds, ax
    mov ax,0A000h
    mov es,ax
    mov ah,0h
    mov al,13h
    int 10h

GameLoop:
    call drawBackground
    mov row,190
    call drawBar
    call getKeyPress
    cmp ah,4Bh
    je moveLeft
    cmp ah,4Dh
    je moveRight
jmp GameLoop

drawBackground PROC
    mov di,0h
    mov al,03h
    mov cx,64000
    rep STOSB
ret
ENDP drawBackground
drawBar PROC
    mov ax, row
    mov bx, 320
    mul bx
    add ax, barPos
    mov di, ax
    mov al, 04h
    mov cx, barLen
    rep STOSB
    inc row
    cmp row,200
    jl drawBar
ret
ENDP drawBar
moveLeft:
    mov bx,barSpeed
    cmp barPos, bx    ; Ensure bar doesn't move out of bounds
    jl GameLoop
    sub barPos, bx    ; Move bar left
    jmp GameLoop
moveRight:
    mov bx,320
    sub bx,barLen
    cmp barPos, bx  ; Ensure bar doesn't move out of bounds (320 - barLength)
    jge GameLoop
    mov bx,barSpeed
    add barPos, bx    ; Move bar right 
    jmp GameLoop
getKeyPress PROC
    mov ah, 0h           ; Wait for key press
    int 16h              ; BIOS interrupt
ret
ENDP getKeyPress
Exit:
    MOV AX, 4C00h      ; Function to terminate program
    INT 21H            ; Call DOS interrupt to exit
MAIN ENDP
END MAIN 