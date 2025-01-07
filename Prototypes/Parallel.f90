program main
implicit none


integer :: board(4,4), bp(4,4,4), matarray(16), numbers(15), board_i(4,4)
real :: dist, random,temp
complex :: BV,bval
integer :: i,j,k,l,tmp
integer :: wn ! win number, 0 if no win detected, 1 if yes
integer :: bin(4)
integer :: r,s,t, nsteps, ciclo



call random_seed()
call random_number(random)

!write(*,*) 'random=', random



! CHOOSABLE PIECES
do i=2,16
 numbers(i-1)=i
enddo


write(*,*) 'How many steps do you want to try?'
read(*,*) nsteps




! TAKE THE FILE BOARD AND MAKE IT A MATRIX
open(10,file='board')
do i=1,4
read(10,*) board_i(i,:)
enddo
close(10)


open(19, file='results')

!$OMP PARALLEL DEFAULT(NONE) SHARED(board_i,nsteps) PRIVATE(r, i, j,k, wn, bin, s,t, matarray, board, bp,dist,temp,) 
!$OMP DO 


do r=1,nsteps

! NOW I STORE IT AS AN ARRAY
do i=1,4
do j=1,4
 matarray((i-1)*4+j)=board_i(i,j)
enddo
enddo


! FILL 
do i = 15,2,-1
    call random_number(temp)
    j = 1 + int(temp*i)
    temp = numbers(i)
    numbers(i)=numbers(j)
    numbers(j)=temp
enddo


do i=2,16
    matarray(i)=numbers(i-1)
enddo

!write(*,*) 'New:'
do i=1,4
do j=1,4
board(i,j)=matarray((i-1)*4+j)

enddo
!write(19,*) board(i,:)
enddo



!write(*,*) matarray



! RANDOM FILLING



!write(*,*) '--------------------------------------------------------------'

! SHOW THE BOARD
!do i=1,4
!write(*,*) board(i,:)
!enddo






! ASSIGN A BINARY NUMBER TO EACH PIECE
do i=1,4
 do j=1,4
    call binary(board(i,j)-1,bin)
     bp(i,j,:)=bin(:)
 enddo
enddo

! EVALUATE IN AN INVARIANT WAY THE BOARD
BV=BVal(board,bp)
!write(*,*) '--------------------------------------------------------------'

! SHOW THE BOARD
write(19,*) 'Board value: ', BV 

if (nsteps.eq.1) then
open(13,file='board_pieces')

write(13,*) 'Board value: ', BV 
write(13,*) '|i|', '|j|', '|vettore'
write(13,*) '--------------------------------------------------------------'
!write(13,*) '--------------------------------------------------------------'

do i=1,4
do j=1,4
    write(13,*) i,'|',j,'||', bp(i,j,:)
write(13,*) '--------------------------------------------------------------'

enddo
enddo
close(13)
endif
wn=0

call win(bp,wn)



if (wn.eq.0) then
    write(19,*) 'Non winning situation.'
elseif (wn.lt.0) then
    write(19,*) 'Non possible scenario.'
else
do i=1,4
write(19,*) board(i,:)
enddo
write(19,*) ''
!write(19,*) '--------------------------------------------------------------'
write(19,*) '==============================================================='
write(19,*) 'win number', wn

write(19,*) '==============================================================='
!write(19,*) '--------------------------------------------------------------'
    write(19,*) 'Board game with ', wn, 'wins.'
endif


!write(19,*) '**************************************************************'
write(19,*) '**************************************************************'
write(19,*) 'game number = ' , r
write(19,*) '**************************************************************'
!write(19,*) '**************************************************************'
write(19,*) ''





enddo

!$OMP END PARALLEL



close(19)


if (nsteps.eq.1) then
write(*,*) '--------------------------------------------------------------'
write(*,*) '--------------------------------------------------------------'
write(*,*) 'win number', wn 

write(*,*) '--------------------------------------------------------------'
write(*,*) '--------------------------------------------------------------'
if (wn.eq.0) then
    write(*,*) 'Non winning situation.'
elseif (wn.lt.0) then
    write(*,*) 'Non possible scenario.'
else
    write(*,*) 'Board game with ', wn, 'wins.'
endif
endif

end program


subroutine permute(i1,j1,i2,j2,bp)

implicit none

integer :: bp(4,4,4)
integer :: i1,i2,j1,j2
integer :: tmp(4)

tmp=bp(i1,j1,:)
bp(i1,j1,:)=bp(i2,j2,:)
bp(i2,j2,:)=tmp

end subroutine











complex function BVal(board,bp)

implicit none


integer :: board(4,4), bp(4,4,4)
integer :: i,j, ir, jr
real :: val, diff=0
real :: dist


! FINDS THE POSITION i,j OF THE REFERENCE PIECE IN THE BOARD
do i=1,4
    do j=1,4
        if (board(i,j).eq.1) then
            ir=i
            jr=j
        exit
        endif
enddo
enddo



! since the piece 'ref=1' usually is (0,0,0,0)
! every summing the number of 1 in the n-th piece gives the difference
! in variables between the two of them



! FINDS THE DISTANCE OF EACH OTHER PIECE
! THEN IT MULTIPLIES AND ACCUMULATES
val=0.0
diff=0.0
do i=1,4
 do j=1,4
    if (i.eq.ir.and.j.eq.jr) cycle !salta l'iterazione se sono sullo stesso che tanto mi da 0.
    dist=sqrt(real(i-ir)**2+real(j-jr)**2)
    diff=diff+dist*sum(bp(i,j,:))
    val=val+dist*board(i,j)
enddo
enddo

BVal=cmplx(val,diff)
endfunction









subroutine binary(num,bin)
implicit none

integer :: num, tmp, i,j,k, expmax
integer :: bin(4)




expmax=3
if (num.ge.0) then
do i=0,expmax
    tmp=num/(2**(expmax-i))
    num=num-tmp*2**(expmax-i)
    bin(4-i)=tmp
    !write(*,*) expmax-i, tmp
!    write(*,*) 'num', num
enddo
else
bin(:)=-1
endif

end subroutine













subroutine win(bp,wn)
implicit none

integer :: bp(4,4,4)
integer :: i,j,k, sumline, wn,twn
integer :: hwn,vwn,dwn1,dwn2,dwn
! preset the board as NON-WINNNING for now
hwn=0
do i=1,4
    do k=1,4
        sumline=sum(bp(i,:,k))
        if (sumline.eq.0.or.sumline.eq.4) then
            hwn=hwn+1
        exit
        endif    
    enddo
enddo
if (twn.ge.2) then
    wn=-1
    return
endif

vwn=0
do i=1,4
    do k=1,4
        sumline=sum(bp(:,i,k))
        if (sumline.eq.0.or.sumline.eq.4) then 
            vwn=vwn+1
        exit
        endif
    enddo
enddo
if (twn.ge.2) then
    wn=-1
    return
endif




dwn1=0
do i=1,4
    do k=1,4
        sumline=0
        sumline=sumline+bp(k,k,i)
    enddo
        if (sumline.eq.0.or.sumline.eq.4) then
            dwn1=dwn1+1
        exit
        endif
enddo


dwn2=0
do i=1,4
    do k=1,4
        sumline=0
        sumline=sumline+bp(k,5-k,i)
    enddo
        if (sumline.eq.0.or.sumline.eq.4) then
            dwn2=dwn2+1
        exit
        endif
enddo

if (dwn1.ge.1.and.dwn2.ge.1) then
    wn=-1
    return
endif

dwn=dwn1+dwn2

wn=dwn+vwn+hwn
end subroutine

