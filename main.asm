;EXTERNDELAY = 5
.MODEL small
.STACK 100h
.DATA
    row            DW 190
    barPos         DW 130
    barLen         DW 60
    ballx          DW 155
    bally          DW 185
    ballSize       DW 5
    ballVelX       DW 3
    ballVelY       DW 3
    barSpeed       DW 5
    brickLen       DW 40
    brickHeight    DW 10
    CurrLevel      DW 0
    brickCnt       DW 6
    currBrick      DW 0
    brickPos       DW 10
    timeaux        db 0
    windowWidth    DW 320
    windowHeight   DW 200
    rowCount       DW 3
    brickrowOffset DW 2
    brickrow       DW 0
    brickSpacing   DW 10
    score DW 0
    Lives DW 3
    livesStr db 'lives: ', '$' ; String to display lives
    brick1x dw 10
    brick2x dw 60
    brick3x dw 110
    brick4x dw 160
    brick5x dw 210
    brick6x dw 260
    brick7x dw 10
    brick8x dw 60
    brick9x dw 110
    brick10x dw 160
    brick11x dw 210
    brick12x dw 260
    brick13x dw 10
    brick14x dw 60
    brick15x dw 110
    brick16x dw 160
    brick17x dw 210
    brick18x dw 260

    brick1y dw 10
    brick2y dw 10
    brick3y dw 10
    brick4y dw 10
    brick5y dw 10
    brick6y dw 10

    brick7y dw 30
    brick8y dw 30
    brick9y dw 30
    brick10y dw 30
    brick11y dw 30
    brick12y dw 30

    brick13y dw 50
    brick14y dw 50
    brick15y dw 50
    brick16y dw 50
    brick17y dw 50
    brick18y dw 50
    brick1Exist dw 1
    brick2Exist dw 1
    brick3Exist dw 1
    brick4Exist dw 1
    brick5Exist dw 1
    brick6Exist dw 1
    brick7Exist dw 1
    brick8Exist dw 1
    brick9Exist dw 1
    brick10Exist dw 1
    brick11Exist dw 1
    brick12Exist dw 1
    brick13Exist dw 1
    brick14Exist dw 1
    brick15Exist dw 1
    brick16Exist dw 1
    brick17Exist dw 1
    brick18Exist dw 1
.CODE
BreakBrick MACRO y, x
    mov dx, 0                ; Initialize the row counter
BreakBrickLoop:
    mov ax, x               
    add ax, dx               ; Add the current row offset
    mov bx, 320   
    push dx           
    mul bx                   ; Calculate memory offset
    pop dx
    add ax, y                ; Add base y-coordinate
    mov di, ax               
    mov al, 09h              
    mov cx, brickLen        
    rep stosb                ; Draw the brick row
    inc dx                   ; Increment the row counter
    cmp dx, brickHeight      
    jb BreakBrickLoop        
ENDM

MAIN PROC FAR
    ; Set up the data segment
                         mov   ax, @DATA
                         mov   ds, ax
    ; Switch to graphics mode
                         mov   ax, 0A000h
                         mov   es, ax
                         mov   ax, 003h
                         int   10h
                         mov   ah, 0h
                         mov   al, 13h
                         int   10h

    ; Initialize game (set bar, ball, etc.)
    GameProc PROC FAR
                            mov  ballx, 155
                            mov  bally, 185
                            mov  ballVelX, 3
                            mov  ballVelY, 3
                            mov  barPos, 130
                            mov  barLen, 60
                            mov  barSpeed, 5
                            mov  brickLen, 40
                            mov si,offset brick1Exist
                            mov bx,0
                            mov cx,18
                            initloop:
                                mov [si+bx],1
                                add bx,2
                            loop initloop
                         call  drawBackgroundinit
                         call  drawBrick
                         call  drawBall
                         call  drawBar
                         call  copyBufferToScreen
                         ;BreakBrick brick9x brick9y
                         call  getKeyPress
                         
    GameLoop:            
WaitForVSync:
    mov dx, 03DAh         ; VGA Input Status Register 1
WaitForRetrace:
    in al, dx
    test al, 08h          ; Check the Vertical Retrace bit (bit 3)
    jz WaitForRetrace     ; Loop until retrace starts
                         call  drawBackground            ; Draw the background
                         call checkBrickCollision
                         call  moveball
                         call  drawBall                  ; Draw the ball
                         call checkBrickCollision
                         call checkWin
                         ;call  drawBrick
                         mov   row, 190
                         call  drawBar

                         call  copyBufferToScreen
                         mov   ah, 01h                   ; Check if key is pressed
                         int   16h
                         jz    skipInput                 ; If no key pressed, skip input handling

    ; Handle key press
                         mov   ah, 00h                   ; Get the key
                         int   16h
                         cmp   ah, 4Bh                   ; Left arrow
                         je    moveLeft
                         cmp   ah, 4Dh                   ; Right arrow
                         je    moveRight

    skipInput:           
    ; Continue game loop
                         jmp   GameLoop

    moveLeft:            
                         mov   bx, barSpeed
                         cmp   barPos, bx                ; Ensure bar doesn't move out of bounds
                         jl    GameLoop
                         sub   barPos, bx                ; Move bar left
                         jmp   GameLoop

    moveRight:           
                         mov   bx, 320
                         sub   bx, barLen
                         cmp   barPos, bx                ; Ensure bar doesn't move out of bounds (320 - barLength)
                         jge   GameLoop
                         mov   bx, barSpeed
                         add   barPos, bx                ; Move bar right
                         jmp   GameLoop

    DRAWBALL PROC NEAR
		
                         MOV   CX,BALLX                  ;set the initial column (X)
                         MOV   DX,BALLY                  ;set the initial line (Y)
		
    DRAW_BALL_HORIZONTAL:
                         MOV   AH,0Ch                    ;set the configuration to writing a pixel
                         MOV   AL,0Dh                    ;choose white as color
                         MOV   BH,00h                    ;set the page number
                         INT   10h                       ;execute the configuration
			
                         INC   CX                        ;CX = CX + 1
                         MOV   AX,CX                     ;CX - BALL_X > BALL_SIZE (Y -> We go to the next line,N -> We continue to the next column
                         SUB   AX,BALLX
                         CMP   AX,BALLSIZE
                         JNG   DRAW_BALL_HORIZONTAL
			
                         MOV   CX,BALLX                  ;the CX register goes back to the initial column
                         INC   DX                        ;we advance one line
			
                         MOV   AX,DX                     ;DX - BALL_Y > BALL_SIZE (Y -> we exit this procedure,N -> we continue to the next line
                         SUB   AX,BALLY
                         CMP   AX,BALLSIZE
                         JNG   DRAW_BALL_HORIZONTAL
		
                         RET
DRAWBALL ENDP
ERASEBALL PROC NEAR
		
                         MOV   CX,BALLX                  ;set the initial column (X)
                         MOV   DX,BALLY                  ;set the initial line (Y)
		
    ERASE_BALL_HORIZONTAL:
                         MOV   AH,0Ch                    ;set the configuration to writing a pixel
                         MOV   AL,09h                    ;choose white as color
                         MOV   BH,00h                    ;set the page number
                         INT   10h                       ;execute the configuration
			
                         INC   CX                        ;CX = CX + 1
                         MOV   AX,CX                     ;CX - BALL_X > BALL_SIZE (Y -> We go to the next line,N -> We continue to the next column
                         SUB   AX,BALLX
                         CMP   AX,BALLSIZE
                         JNG   ERASE_BALL_HORIZONTAL
			
                         MOV   CX,BALLX                  ;the CX register goes back to the initial column
                         INC   DX                        ;we advance one line
			
                         MOV   AX,DX                     ;DX - BALL_Y > BALL_SIZE (Y -> we exit this procedure,N -> we continue to the next line
                         SUB   AX,BALLY
                         CMP   AX,BALLSIZE
                         JNG   ERASE_BALL_HORIZONTAL
		
                         RET
ERASEBALL ENDP
drawBackgroundinit PROC
                         mov   di,0h
                         mov   al,09h
                         mov   cx,64000
                         rep stosb
                         ret
drawBackgroundinit ENDP
drawBackground PROC
                         mov di,19200
                         mov al,09h
                         mov cx,44800
                         rep stosb


drawBackground ENDP
drawBar PROC
                         mov   ax, row
                         mov   bx, 320
                         mul   bx
                         add   ax, barPos
                         mov   di, ax
                         mov   al, 04h
                         mov   cx, barLen
                         rep   STOSB
                         inc   row
                         cmp   row,200
                         jl    drawBar
                         ret
drawBar ENDP
getKeyPress PROC
                         mov   ah, 0h                    ; Wait for key press
                         int   16h                       ; BIOS interrupt
                         ret
getKeyPress ENDP
moveball PROC
                             call  ERASEBALL
    ; Update ball's horizontal position
                         mov   ax, ballVelX              ; Load horizontal velocity
                         add   ballx, ax                 ; Update horizontal position
                         cmp   ballx, 0                  ; Check if ball has hit the left boundary
                         jl    negVelX                   ; Reverse horizontal direction if it has
                         mov   ax, windowWidth           ; Check against right boundary
                         sub   ax, ballSize              ; Adjust for the ball's size
                         cmp   ballx, ax
                         jg    negVelX                   ; Reverse horizontal direction if it has

    ; Update ball's vertical position
                         mov   ax, ballVelY              ; Load vertical velocity
                         add   bally, ax                 ; Update vertical position

    ; Check for collision with paddle (bar)
                         mov   ax, bally                 ; Ball's current vertical position
                         add   ax, ballSize              ; Ball's bottom edge
                         cmp   ax, 190                   ; Compare with paddle's top surface (y = 190)
                         jl    skipPaddleCheck           ; If above the paddle, skip collision check

    ; Ball is at or below the paddle's top surface, check horizontal overlap
                         mov   ax, ballx                 ; Ball's horizontal position
                         mov   bx, barPos                ; Bar's left position
                         cmp   ax, bx                    ; Is the ball to the right of the paddle's left edge?
                         jl    skipPaddleCheck           ; If not, skip collision check
                         mov   bx, barLen                ; Bar's length
                         add   bx, barPos                ; Calculate bar's right position
                         cmp   ax, bx                    ; Is the ball to the left of the paddle's right edge?
                         jg    skipPaddleCheck           ; If not, skip collision check

    ; Ball collided with the paddle
                         neg   ballVelY                  ; Reverse vertical direction
                         mov   ax, 190                   ; Place the ball just above the paddle
                         sub   ax, ballSize              ; Adjust for the ball's size
                         mov   bally, ax                 ; Update ball's vertical position

    skipPaddleCheck:     
                         cmp   bally, 0                  ; Check if ball has hit the top boundary
                         jl    negVelY                   ; Reverse vertical direction if it has
                         mov   ax, windowHeight          ; Check against bottom boundary
                         cmp   bally, ax
                         jg   checkEndGame                  ; Reverse vertical direction if it has
                         ret

    negVelX:             
                         neg   ballVelX                  ; Reverse horizontal direction
                         ret

    negVelY:             
                         neg   ballVelY                  ; Reverse vertical direction
                         ret
    checkEndGame:
                            mov ax, Lives
                            dec ax
                            mov Lives, ax
                            cmp ax, 0
                            je gameOver
                            call GameProc
    gameOver:            
    ; Switch to text mode (80x25, color)
                         mov   ax, 0003h
                         int   10h

    ; Calculate the starting position for centering "GAME OVER"
                         mov   ax, 0B800h                ; Video memory segment
                         mov   es, ax

    ; Calculate row and column
                         mov   cx, 12                    ; Center row (0-based, row 12)
                         mov   bx, 50                    ; Center column (0-based, column 35)

    ; Calculate the offset in video memory
                         mov   ax, cx                    ; Row * 80
                         mov   di, ax
                         shl   di, 6                     ; Multiply by 64 (80 columns * 2 bytes per character)
                         add   di, ax                    ; Multiply by 80 (80 = 64 + 16)
                         add   di, bx                    ; Add column index
                         shl   di, 1                     ; Multiply by 2 (2 bytes per character)

    ; Write "GAME OVER" with red background and white text
                         mov   ah, 4Fh                   ; Attribute byte: red background, white text
                         mov   al, 'G'
                         stosw
                         mov   al, 'A'
                         stosw
                         mov   al, 'M'
                         stosw
                         mov   al, 'E'
                         stosw
                         mov   al, ' '
                         stosw
                         mov   al, 'O'
                         stosw
                         mov   al, 'V'
                         stosw
                         mov   al, 'E'
                         stosw
                         mov   al, 'R'
                         stosw

    ; Wait for key press to exit
                         mov   ah, 00h
                         int   16h
                         jmp   Exit
moveball ENDP
checkBrickCollision PROC
    ; Initialize loop variables
    mov cx, 18                      ; Number of bricks to check
    mov si, OFFSET brick1x          ; Start with the first brick
BrickCollisionLoop:
    ; Check if the current brick exists
    mov ax, [si+72]                ; Load the corresponding brickExist flag
    cmp ax, 0                      ; Is the brick destroyed?
    je NextBrick                   ; Skip if destroyed

    ; Check collision with the ball
    mov ax, ballx
    cmp ax, [si]                   ; Compare ball x with brick x
    jl NextBrick                   ; If left of brick, skip
    mov bx, [si]
    add bx, brickLen
    cmp ax, bx                     ; Compare ball x with brick's right edge
    jg NextBrick                   ; If right of brick, skip

    mov ax, bally
    cmp ax, [si+36]                 ; Compare ball y with brick y
    jl NextBrick                   ; If above brick, skip
    mov bx, [si+36]
    add bx, brickHeight
    cmp ax, bx                     ; Compare ball y with brick's bottom edge
    jg NextBrick                   ; If below brick, skip

    ; Collision detected
    mov word ptr [si+72], 0        ; Mark brick as destroyed
    neg ballVelY                   ; Reverse ball's vertical velocity
    mov ax, [si]                   ; Load brick x position
    mov bx, [si+36]                ; Load brick y position
    BreakBrick [si] [si+36]        ; Call macro with brick position
    call Beep                  ; Make a beep sound
NextBrick:
    add si, 2                      ; Move to the next brick (x and y are 2 bytes each)
    loop BrickCollisionLoop        ; Repeat for all bricks

    ret
checkBrickCollision ENDP
checkWin PROC
        mov si,offset brick1Exist
        mov bx,0
        mov cx,18
    checkloop:
            cmp word [si+bx],1
            je endCheckWin
            add bx,2
        loop checkloop        
            ; Switch to text mode (80x25, color)
                         mov   ax, 0003h
                         int   10h

    ; Calculate the starting position for centering "WINNNER"
                         mov   ax, 0B800h                ; Video memory segment
                         mov   es, ax

    ; Calculate row and column
                         mov   cx, 12                    ; Center row (0-based, row 12)
                         mov   bx, 50                    ; Center column (0-based, column 35)

    ; Calculate the offset in video memory
                         mov   ax, cx                    ; Row * 80
                         mov   di, ax
                         shl   di, 6                     ; Multiply by 64 (80 columns * 2 bytes per character)
                         add   di, ax                    ; Multiply by 80 (80 = 64 + 16)
                         add   di, bx                    ; Add column index
                         shl   di, 1                     ; Multiply by 2 (2 bytes per character)

    ; Write "GAME OVER" with red background and white text
                         mov   ah, 4Fh                   ; Attribute byte: red background, white text
                         mov   al, 'W'
                         stosw
                         mov   al, 'I'
                         stosw
                         mov   al, 'N'
                         stosw
                         mov   al, 'N'
                         stosw
                         mov   al, 'E'
                         stosw
                         mov   al, 'R'
                         stosw

    ; Wait for key press to exit
                         mov   ah, 00h
                         int   16h
                         jmp   Exit
        endCheckWin:
            ret
checkWin ENDP

beep proc
        push ax
        push bx
        push cx
        push dx
        mov     al, 182         ; Prepare the speaker for the
        out     43h, al         ;  note.
        mov     ax, 400        ; Frequency number (in decimal)
                                ;  for middle C.
        out     42h, al         ; Output low byte.
        mov     al, ah          ; Output high byte.
        out     42h, al 
        in      al, 61h         ; Turn on note (get value from
                                ;  port 61h).
        or      al, 00000011b   ; Set bits 1 and 0.
        out     61h, al         ; Send new value.
        mov     bx, 2          ; Pause for duration of note.
.pause1:
        mov     cx, 65535
.pause2:
        dec     cx
        jne     .pause2
        dec     bx
        jne     .pause1
        in      al, 61h         ; Turn off note (get value from
                                ;  port 61h).
        and     al, 11111100b   ; Reset bits 1 and 0.
        out     61h, al         ; Send new value.

        pop dx
        pop cx
        pop bx
        pop ax

ret
beep endp

drawBrick PROC
    ; Initialize variables
    mov   rowCount, 0
    mov   brickrow, 10     ; Top spacing (10 pixels)
    mov   brickHeight, 10
    mov   si, 0

drawrow:
    mov   currBrick, 0
    mov   brickpos, 10     ; Left boundary spacing

loop1:
    mov   brickrowOffset, 0

loop2:
    mov   ax, brickrow
    add   ax, brickrowOffset
    mov   bx, 320
    mul   bx
    add   ax, brickpos
    mov   di, ax

    mov   al, 06h          ; Brick color
    mov   cx, brickLen
    rep   stosb
    inc   brickrowOffset
    mov   ax, brickHeight
    cmp   brickrowOffset, ax
    jl    loop2

    ; Update brick position for next column with spacing
    mov   ax, 40           ; Brick width
    add   ax, brickSpacing
    add   brickpos, ax

    inc   si
    inc   currBrick
    mov   ax, brickCnt
    cmp   currBrick, ax
    jl    loop1

    ; Update brickrow for next row with spacing
    mov   ax, 10           ; Brick height
    add   ax, brickSpacing
    add   brickrow, ax

    inc   rowCount
    cmp   rowCount, 3      ; Number of rows
    jl    drawrow

    ret
drawBrick ENDP

copyBufferToScreen PROC
                         push  ds
                         push  es
                         mov   ax, 0A000h
                         mov   es, ax
                         mov   ds, ax
                         mov   si, 0
                         mov   di, 0
                         mov   cx, 320*200
                         rep   movsb
                         pop   es
                         pop   ds
                         ret
copyBufferToScreen ENDP
    Exit:                
                         MOV   AX, 4C00h                 ; Function to terminate program
                         INT   21H                       ; Call DOS interrupt to exit
                         hlt
GameProc ENDP
MAIN ENDP
END MAIN