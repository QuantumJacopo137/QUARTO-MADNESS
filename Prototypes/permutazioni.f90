program main
implicit none

integer :: i,j,k, tmp
integer :: board(4,4), ba(16)
integer :: r,s,t,u

open(unit=10,file='board')
do i=1,4
 read(10,*) board(i,:)
enddo
close(10)

do i=1,4
write(*,*) board(i,:)
enddo


do i=1,4
 do j=1,4
    tmp=4*(i-1)+j
    ba(tmp)=board(i,j)
 enddo
enddo


open(11,file='FinalBoard16')
u=0
do i=1,4
write(11,*) board(i,:)
enddo
do i=2,16
 do j=i+1,16
  call swap(i,j,ba)
  call array2matrix(ba,board)
        u=u+1
        write(11,*) '------------------------------------'
        write(11,*) 'SWAP NUBER:', u
        write(11,*) '------------------------------------'
    do k=1,4
        write(11,*) board(k,:)
    enddo
 enddo
enddo

close(11)


write(*,*) '********************************************'
write(*,*) 'Ho eseguito ', u, 'permutazioni.'









end program


subroutine swap(i,j,ba)
implicit none

integer :: i,j
integer :: ba(16)
integer :: tmp


tmp=ba(i)
ba(i)=ba(j)
ba(j)=tmp



end subroutine

subroutine array2matrix(ba,board)
implicit none

integer :: i,j,k
integer :: ba(16), board(4,4)

do i=2,16
    board(1+((i-1)/4),1+mod(i-1,4))=ba(i)
enddo



end subroutine

