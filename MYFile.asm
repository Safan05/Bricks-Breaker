EXTERNDELAY = 3
.model small
.stack 100h
.data

Welcomemsg db "  BRICK BREAKER GAME $ "
Entermsg db " 1-> NEW GAME $ "
Instructionmsg db " 2-> INSTRUCTIONS $ "
Highscr db " 3-> HIGH SCORE$"
Ext db " 4-> Exit$"
tip1msg db "1)You have Three levels in this game $"
tip2msg db "2) You need to hit the ball to direct it toward hitting the bricks above. When the ball comes into contact with a brick, the brick will  break .  $"

tip3msg db "3)You need to moves the paddle from left to right to keep the ball from falling. $"
tip4msg db "4)Life is used when the player fails to hitthe ball. $"
tip5msg db "5)Number of lives are displayed on top of the screen. $"

tip6msg db "6) Your score will increased by breaking the brick $"
tip7msg db "Score as much as you can and Enjoy the game $"
tip8msg db "BEST OF LUCK :)  $"

usermsg db "Enter the name of the user : $ "
username db 50 dup("$")
loadingmsg db "GAME IS LOADING...$"
mainmenumsg db "Press any key to go back to Main Menu $" 


f1 db "file.txt",0

handle dw 0
buffer db 100 DUP('$')

score db 'Score: $'
scoreCount dw 0
lives db  3h
livesCount db 51
ending db ' $'



ballY dw 163
ballX dw 158
ballLeft db 1
ballUp db 1
color db ?
startx dw ?
starty dw ?
endx dw ?
endy dw ?       
begin db 0
strikerX dw 140
strikerY dw 170
innerDelay db 0
boundaryEnd dw 250
boundaryStart dw 30

brick1x dw 45
brick1y dw 25
brick2x dw 85
brick2y dw 25
brick3x dw 125
brick3y dw 25
brick4x dw 165
brick4y dw 25
brick5x dw 205
brick5y dw 25
brick6x dw 245
brick6y dw 25
brick7x dw 45
brick7y dw 45
brick8x dw 85
brick8y dw 45
brick9x dw 125
brick9y dw 45
brick10x dw 165
brick10y dw 45
brick11x dw 205
brick11y dw 45
brick12x dw 245
brick12y dw 45


              

                                                                                                                           
.code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;redrawStriker macro;;;;;;;;;;
redrawStriker macro visColor

mov color, visColor
call drawStriker
endm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;redrawBall macro;;;;;;;;;;;;;;;;
redrawBall macro visColor
    mov color, visColor
    call drawball
endm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;BuildBrick macro;;;;;;;;;;;;;;;;
BuildBrick macro  A, B
    push ax
    push bx
    mov ax, A
    mov bx, B
    call AddBrick
    pop bx
    pop ax
endm

;;;;;;;;;;;;;;;;;;;;;;;;;;DestroyBrick macro;;;;;;;;;;;;

DestroyBrick macro  A, B
    push ax
    push bx
    mov ax, A
    mov bx, B
    call RemoveBrick
    call beep     
    inc scoreCount
    call DrawLivesScores
    pop bx
    pop ax
endm
;;;;;;;;;;;;;;;;;;;;;;;;;;DestroyBrick2 macro;;;;;;;;;;;;

DestroyBrick2 macro  A, B
    push ax
    push bx
    mov ax, A
    mov bx, B
    call RemoveBrick
    call beep     
    inc scoreCount
    call DrawLivesScores2
    pop bx
    pop ax
endm

;;;;;;;;;;;;;;;;;;;;;;;;;;DestroyBrick3 macro;;;;;;;;;;;;

DestroyBrick3 macro  A, B
    push ax
    push bx
    mov ax, A
    mov bx, B
    call RemoveBrick
    call beep     
    inc scoreCount
    call DrawLivesScores3
    pop bx
    pop ax
endm


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;BrickCollision macro;;;;;;;;;

BrickCollision MACRO X, Y
local copper
    push ax
    push bx
    push cx
    push dx
    mov ax, ballY
    mov bx, ballX
    mov cx, X
    mov dx, Y
    
    cmp dx, ballY
    jl copper
    sub dx, 7
    
    cmp ballY, dx
    jl copper
    
    
    mov dx, X 
    
    cmp ballX, dx
    jl copper
    add dx, 30
    cmp dx, ballX
    jl copper
    
    call switcher
    DestroyBrick X, Y
    mov Y, 300
    cmp scoreCount, 12
    jne copper

    call Level2
    mov ah,4ch
    int 21h
    
    copper:
    pop dx
    pop cx
    pop bx
    pop ax                      
    
endm


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;BrickCollision2 macro;;;;;;;;;

BrickCollision2 MACRO X, Y
local copper2
    push ax
    push bx
    push cx
    push dx
    mov ax, ballY
    mov bx, ballX
    mov cx, X
    mov dx, Y
    
    cmp dx, ballY
    jl copper2
    sub dx, 7
    
    cmp ballY, dx
    jl copper2
    
    
    mov dx, X 
    
    cmp ballX, dx
    jl copper2
    add dx, 30
    cmp dx, ballX
    jl copper2
    
    call switcher2
    DestroyBrick2 X, Y
    mov Y, 300
    cmp scoreCount, 24
    jne copper2

   
    call Level3
    mov ah,4ch
    int 21h
    
    copper2:
    pop dx
    pop cx
    pop bx
    pop ax                      
    
endm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;BrickCollision3 macro;;;;;;;;;

BrickCollision3 MACRO X, Y
local copper3
    push ax
    push bx
    push cx
    push dx
    mov ax, ballY
    mov bx, ballX
    mov cx, X
    mov dx, Y
    
    cmp dx, ballY
    jl copper3
    sub dx, 7
    
    cmp ballY, dx
    jl copper3
    
    
    mov dx, X 
    
    cmp ballX, dx
    jl copper3
    add dx, 30
    cmp dx, ballX
    jl copper3
    
    call switcher3
    DestroyBrick2 X, Y
    mov Y, 300
    cmp scoreCount, 36
    jne copper3

   
    call UWin
    mov ah,4ch
    int 21h
    
    copper3:
    pop dx
    pop cx
    pop bx
    pop ax                      
    
endm


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;main;;;;;;;;;;;;;;;;;;;;;;;
main proc
    mov ax,@data
    mov ds,ax
    call mainpage       
 main endp 

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;mainpage;;;;;;;;;;;;;;;;;;;;;;;;
mainpage proc

    

    
   
    call setVideoMode  
    call drawBoundary 

    BuildBrick brick1x brick1y
    BuildBrick brick2x brick2y
    BuildBrick brick3x brick3y
    BuildBrick brick4x brick4y
    BuildBrick brick5x brick5y
    BuildBrick brick6x brick6y
    BuildBrick brick7x brick7y
    BuildBrick brick8x brick8y
    BuildBrick brick9x brick9y
    BuildBrick brick10x brick10y
    BuildBrick brick11x brick11y
    BuildBrick brick12x brick12y
    
    redrawStriker 7
    redrawBall 3
    call DrawLivesScores
    pop bx
    pop ax
    call gameLoop
  
 ret  

mainpage endp







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Rec;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Rec proc
mov si,100
mov di,350
Aright:
MOV AH,0CH
mov al,14
INT 10H
mov bh,0
cmp si,di
je dwn
inc si
inc cx
jmp Aright
dwn:
mov si,0
Adown:
MOV AH, 0CH
mov al,14
INT 10H
mov bh,0
cmp si,di
je lft
inc si
inc dx
jmp Adown
lft:
mov si,0
Aleft:
MOV AH, 0CH
mov al,14
INT 10H
mov bh,0
cmp si,di
je ups
inc si
dec cx
jmp Aleft
ups:
mov si,0
Aup:
MOV AH, 0CH
mov al,14
INT 10H
mov bh,0
cmp si,di
je ex
inc si
dec dx
jmp Aup
ex:
ret 4
Rec endp


 ;;;;;;;;;;;;;;;;;;;;;;;DrawLivesScores;;;;;;;;;;;;;;;;;;;;

 DrawLivesScores proc
    push dx
    push ax
                 
    mov dh, 26 ;row
    mov dl, 20 ;col
    mov ah, 2 
    int 10h
    
    lea dx, score
    mov ah, 9
    int 21h
    
    call printScore
    
    mov dh, 26 ;row
    mov dl, 0 ;col
    mov ah, 2 
    int 10h

    lea dx, lives
    mov ah,9
    int 21h  
   

    
    pop ax
    pop dx
    ret
    DrawLivesScores endp

    ;;;;;;;;;;;;;;;;;;;;;;;;;CollisionStriker;;;;;;;;;;;;;

    CollisionStriker proc    
    push ax
    push bx
    push cx
    push dx
    
    mov dx, ballY
    cmp dx, 165 ; striker surface check
    jl bhaag
    cmp dx, 170 ; striker missed
    jg fail 
    
    
    
    mov cx,strikerX   
    mov ax, ballX   
    cmp ax, cx  
    jl bhaag
    add cx , 40 
    cmp ax, cx
    jg bhaag
    
    mov ballUp, 1
    jmp bhaag
    
    
    fail:
    mov begin,0 
    dec livesCount
    cmp livesCount,48    ; when lives become 0
    je  khatam           ; Game end
    push ax
    push bx
    push cx
    push dx
    
    
    redrawBall 0
    
    mov ax, strikerX
    mov ballX,ax
    add ballX,18
    
    mov ballY,  163
    
    redrawBall 3
    mov ballUp, 1     ;monis
    mov ballLeft,0
    
    
    
    pop dx
    pop cx
    pop bx
    pop ax
    
    call DrawLivesScores
    jmp bhaag
    
    
    
    khatam:

    mov dh, 37  ;row
    mov dl, 8 ;col
    mov ah, 2 
    int 10h
     
    lea dx, Gamovr
    mov ah,9
    int 21h

    call DrawLivesScores
    
    mov ah,4ch
    int 21h 
                  
    bhaag:  
    
    pop dx
    pop cx
    pop bx
    pop ax
    ret
    CollisionStriker endp

  
   
;;;;;;;;;;;;;;;;;;;;;;;;;setVideoMode;;;;;;;;;;;;;;;;;;;;;;;
setVideoMode proc
    
    mov ah, 0   ; set display mode function.
    mov al, 13h ; mode 13h = 320x200 pixels, 256 colors.
    int 10h     
    
    ret
    setVideoMode endp

;;;;;;;;;;;;;;;;;;;;;draw;;;;;;;;;;;;;;;;;;;;;;;;;;;

draw proc
    push ax
    push cx
    push dx
     
    mov dx,starty
    mov cx,startx
    mov ah,0ch
    mov al,color
    c1:
    inc cx
    int 10h
    cmp cx,endx
    jne c1

    mov cx,startx
    inc dx
    cmp dx,endy
    jne c1 
    
    pop dx
    pop cx
    pop ax
    ret
draw endp

repeat1:
gameLoop:        
   CALL    checkKeyboard
   cmp begin,1
   jne repeat1
   
   
   
   call Collisionwall
   call CollisionStriker 
   BrickCollision Brick1x, Brick1y
   BrickCollision Brick2x, Brick2y
   BrickCollision Brick3x, Brick3y
   BrickCollision Brick4x, Brick4y
   BrickCollision Brick5x, Brick5y
   BrickCollision Brick6x, Brick6y 
   BrickCollision Brick7x, Brick7y
   BrickCollision Brick8x, Brick8y
   BrickCollision Brick9x, Brick9y
   BrickCollision Brick10x, Brick10y
   BrickCollision Brick11x, Brick11y
   BrickCollision Brick12x, Brick12y
   
   CALL baller  
   CALL sleep

   JMP     gameLoop 
    
exit:

switcher:
    cmp ballUp, 1
    je DownT
    jne UpT
    UpT:
    inc ballUp
    ret
    DownT:
    dec ballUp
    ret

ajeebse:
ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;baller;;;;;;;;;;;;;;;;;;;;
baller proc  
    
    inc innerDelay
    cmp innerDelay, EXTERNDELAY
    jne ajeebse 
    mov innerDelay, 0
    redrawBall 0  
    
    mov bx,ballX 
    cmp ballLeft, 1
    je Left
    jne Right
    
    Left:   
    sub bx, 2 
    jmp P2;  
    Right:   
    add bx, 2
    
    P2:
    mov ballX,  bx
    mov bx, ballY
    cmp ballUp, 1   
    je Up
    jne Down
    Up:
    sub bx, 2
    jmp P3
    Down:
    add bx, 2
    P3:
    mov ballY,  bx
   
    redrawBall 3
    
ret
baller endp 

;;;;;;;;;;;;;;;;;;;;;Collisionwall;;;;;;;;;;;;;;;;;;;

Collisionwall proc     
    
    mov bx, ballX
    mov cx, ballY
    
    checkLeftRight:
    cmp bx, 25; max left
    jl goRight
    cmp bx, 290; Max Right
    jg goLeft
    jmp checkUpDown
    goRight:
    mov ballLeft, 0 
    jmp checkUpDown;
    goLeft:
    mov ballLeft, 1
    checkUpDown:
    
    cmp cx, 13;max top
    jl goDown
    cmp cx, 184;max bottom
    jg goUp
    
    
    jmp noInput
    goUp:                                            
    mov ballUp,1
    jmp noInput
    goDown: 
    mov ballUp, 0
  
    ret
    Collisionwall endp


;;;;;;;;;;;;;;;;;;;;;RemoveBrick;;;;;;;;;;;;;;;;;;

RemoveBrick proc 
    
    push ax
    push bx
    push cx
    push dx
       
    mov startx, ax
    mov color, 0  
    mov ax, bx
    mov bx, startx
    
    add bx, 30
    
    mov endx,bx
    
    mov starty, ax 
    
    mov bx,starty
    
    add bx,7
    mov endy,bx
     
    call draw 
    
    pop dx
    pop cx
    pop bx
    pop ax
    ret
    RemoveBrick endp


;;;;;;;;;;;;;;;;;;;;;;checkKeyboard;;;;;;;;;;;;;;;;;

checkKeyboard proc
    mov     ah,     1h
    int     16h         ; check keypress
    jz      noInput     ; no keypress
    mov     ah,     0h
    int     16h
    cmp     ax,     4D00h
    je      rightKey
    cmp     ax,     4B00h
    je      leftKey
    cmp     al,     27D
    je      exit
    cmp     ax,     3920h;space to begin
    je      beg
    jne     noInput
    
    beg:
    mov begin,1
    
    noInput:
    ret  

    rightKey:     
    mov bx, boundaryEnd
    cmp     strikerX, bx ;max right limit
    jg      noInput
    redrawStriker 0
    add     strikerX, 5
    redrawStriker 7
    cmp begin,0
    jz moveBallRight
    jmp     noInput
    
    
    leftKey:   
    mov bx, boundaryStart                            
    cmp     strikerX, bx ;max left limit
    jl      noInput
    redrawStriker 0
    sub     strikerX, 5
    redrawStriker 7
    cmp begin,0
    jz moveBallLeft
    jmp     noInput
    
    
    moveBallLeft:
    redrawBall 0
    sub     ballX, 5
    redrawBall 3
    jmp     noInput
    
    
    moveBallRight:
    redrawBall 0
    add     ballX, 5
    redrawBall 3
    jmp     noInput

checkKeyboard endp


;;;;;;;;;;;;;;;;;;;;;;;sleep;;;;;;;;;;;;;;;;;;;;;;;;;

sleep proc

mov cx,1111111111111111b 

l:
loop l
ret
sleep endp
;;;;;;;;;;;;;;;;;;;;;;;sleep2;;;;;;;;;;;;;;;;;;;;;;;;;

sleep2 proc

mov cx,111111111111111b 

.l:
loop .l
ret
sleep2 endp
;;;;;;;;;;;;;;;;;;;;;;;sleep3;;;;;;;;;;;;;;;;;;;;;;;;;

sleep3 proc

mov cx,11111111111111b 

slp:
loop slp
ret
sleep3 endp 


;;;;;;;;;;;;;;;;;;;;;;;;;;printScore;;;;;;;;;;;;;;;;;;;;;;


printScore proc
    push ax
    push bx
    push cx
    push dx
    
    mov cx,0
    
    mov ax,scoreCount
    ll:
    mov bx,10
    mov dx,0
    div bx
    push dx
    inc cx
    cmp ax,0
    jne ll
    
    l2:
    pop dx
    mov ah,2
    add dl,'0'
    int 21h
    loop l2
    
    pop dx
    pop cx
    pop bx
    pop ax
    
    ret
    printScore endp


;;;;;;;;;;;;;;;;;;;;;;;;;;;drawStriker;;;;;;;;;;;;;;;;;;;;;;;

drawStriker proc
    push bx
    push cx
        
    mov bx, strikerX
    mov cx, strikerY   
    mov startx,bx
    add bx, 40
    mov endx,bx
    mov starty,cx
    mov endy,175
    call draw
    
    pop cx
    pop bx
    ret
    drawStriker endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;AddBrick;;;;;;;;;;;;;;;;;;;;;;;;;;;;

AddBrick proc
    push ax
    push bx    
    mov startx, ax
    mov color, 13  
    mov ax, bx
    mov bx, startx
    
    add bx, 30
    
    mov endx,bx
    
    mov starty, ax 
    
    mov bx,starty
                    
    add bx,7
    mov endy,bx
     
    call draw
    pop bx
    pop ax 
    ret
    AddBrick endp
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;drawball;;;;;;;;;;;;;;;;;


drawball proc
    push bx
    mov bx, ballX
    mov startx, bx
    add bx, 4 
    mov endx,   bx
    mov bx, ballY
    mov starty, bx
    add bx, 4
    mov endy,   bx
    
    pop bx
    
    call draw
ret
drawball endp


    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;drawBoundary;;;;;;;;;;;;;;;

    drawBoundary proc
    mov color,6    
    ;------TOP------------
    mov startx,20
    mov endx,300
    mov starty,5
    mov endy,8
    call draw
    ;------RIGHT------------
    mov startx,297
    mov endx,300
    mov starty,7
    mov endy,180
    call draw
    ;------LEFT------------
    mov startx,20
    mov endx,23
    mov starty,7
    mov endy,180
    call draw
    ;------BOTTOM------------
    mov startx,20
    mov endx,300
    mov starty,177
    mov endy,180
    call draw 
   
    ret
    drawBoundary endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;beep;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;UWinn;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

UWin proc

    mov dh, 37  ;row
    mov dl, 8 ;col
    mov ah, 2 
    int 10h

lea dx, UWinn   
mov ah, 09
int 21h
 
mov ah,4ch
int 21h

UWin endp

end main
