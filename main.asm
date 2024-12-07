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
    brickHeight    DW 20
    CurrLevel      DW 0
    brickCnt       DW 8
    currBrick      DW 0
    brickPos       DW 0
    timeaux        db 0
    windowWidth    DW 320
    windowHeight   DW 200
    rowCount       DW 5
    brickrowOffset DW 0
    brickColors    DB 04h, 05h, 06h, 07h, 08h, 0Ch, 0Ah, 0Bh
    brickrow       DW 0
.CODE
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
                         call  drawBackground
                         call  drawBrick
                         call  drawBall
                         call  drawBar
                         call  getKeyPress
    GameLoop:            
                         call  drawBackground            ; Draw the background
                         mov   brickrow, 0               ; Draw the background
                         mov   brickpos, 0
                         call  drawbrick
                         call  drawBall
                         call  moveball
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
drawBackground PROC
                         mov   di,0h
                         mov   al,09h
                         mov   cx,64000
                         rep   STOSB
                         ret
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
                         jg    gameOver                  ; Reverse vertical direction if it has

                         ret

    negVelX:             
                         neg   ballVelX                  ; Reverse horizontal direction
                         ret

    negVelY:             
                         neg   ballVelY                  ; Reverse vertical direction
                         ret
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
drawBrick PROC
    ; Draw to the back buffer
                         mov   rowCount, 0
                         mov   brickrow, 0
                         mov   brickHeight, 10
                         mov   si, 0
    drawrow:             
    
                         mov   currBrick, 0
                         mov   brickpos, 0

    loop1:               
                         mov   brickrowOffset, 0

    loop2:               
                         mov   ax, brickrow
                         add   ax, brickrowOffset
                         mov   bx, 320
                         mul   bx
                         add   ax, brickpos
                         mov   di, ax

                         mov   al, [brickColors + si]
                         mov   cx, brickLen
                         rep   stosb
                         inc   brickrowOffset
                         mov   ax, brickHeight
                         cmp   brickrowOffset, ax
                         jl    loop2

                         add   brickpos, 40
                         inc   si
                         inc   currBrick
                         mov   ax, brickCnt
                         cmp   currBrick, ax
                         jl    loop1

                         add   brickrow, 10
                         inc   rowCount
                         cmp   rowCount, 5
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
MAIN ENDP
END MAIN