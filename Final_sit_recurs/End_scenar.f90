program main
implicit none

integer :: bp(4,4,4), win_var(8)
integer :: pieces(16,4)
integer :: tmp_ind
integer :: i,j,k,l,r


logical :: reset


bp=0

!call show_board(bp) 

open(19,file='Resuts', status='replace')
close(19)

! GENERATE THE PIECES LIST
tmp_ind=1
do i=-1,1,2
do j=-1,1,2
do k=-1,1,2
do l=-1,1,2

pieces(tmp_ind,1)=i
pieces(tmp_ind,2)=j
pieces(tmp_ind,3)=k
pieces(tmp_ind,4)=l
tmp_ind=tmp_ind+1
enddo
enddo
enddo
enddo

!do i= 1,16
! write(*,*) pieces(i,:)
!enddo


open(15,file='Board')
 do k=1,4
  do i=1,4
   read(15,*) bp(i,:,k)
  enddo
 enddo
close(15)

write(*,*) 'Initial Board'
call show_board(bp)

call solve(bp, pieces)

write(*,*) '=================================='



write(*,*) 'Reset the initial board? (t/f)'
read(*,*) Reset


open(15,file='Board')
if (Reset) then
 bp = 0
 bp(1,1,:) = pieces(1,:)
 do i=1,4
  do k=1,4
   write(15,*) bp(i,:,k)
  enddo
 enddo
endif
close(15)




end program


recursive subroutine solve(bp, pieces)
implicit none

integer :: bp(4,4,4), pieces(16,4)
integer :: i ,j ,k

logical :: possible
logical :: solved

    ! the solved state means if there are any holes in the board 
    solved = .true. ! initiates as "there are no holes in the board"


    ! starts looking for holes
    do i = 1, 4
        do j = 1, 4
            if (bp(i, j,1) .eq. 0) then ! looks for a 0 (hole) in one of the 4 proprietis
                solved = .false.        ! if it finds it, sets solved false, and exits the scouting loop
!               write(*,*) 'Not solved'
                exit
            endif
        enddo
        if (.not. solved) exit
    enddo


    ! a little debug
!   call show_board(bp)


    ! If there are no holes, thus the board is completely filled, it goes on printing the final result in the file 'result'
        open(unit=12,file='Result', position='append')
    if (solved) then
         do i=1,4
           write(12,*) 'Proprietà numero: ', i
          do j=1,4
           write(12,*) bp(j,:,i)
          enddo
         enddo
        write(12,*) '======================================'
        close(12)
!        write(*,*) 'Resolved Game:'
!        call show_board(bp)
        return ! at this point comes back to the previous stack and redo


    ! if it's still not solved?
    else
    ! if there are still some holes, scan the positions
    do i = 1, 4
        do j = 1, 4
            if (bp(i, j, 1) .eq. 0) then ! when it finds the hole
                
                do k = 1, 16  ! tries to fetch on of the pieces and tries the firs non used one
                    
                    ! remember synthax: possible(row, col, bp, piec_index, pieces)
                    
                    if (possible(i, j, bp, k, pieces)) then
!                       write(*,*) 'Place piece', k, 'in (', i,',',j,')'
                        bp(i, j,:) = pieces(k,:)
                        call solve(bp, pieces)
!                       write(*,*) 'Reset to 0 the piece', k, 'In (', i, ',',j,')'
                        ! when everything is full, the subroutine returns, exiting here, setting the last as zero back
                        bp(i, j,:) = 0 
!                       call show_board(bp)
                    else 
!                       write(*,*) 'Piece', k, 'Is impossible to place on the board'
                        bp(i,j,:)=0
                    endif
                
                enddo
                !return

            endif
        enddo
    enddo
    return
    endif
end subroutine


subroutine show_board(b)
implicit none
integer :: b(4,4,4),i,k

do k=1,4
write(*,*) 'k=',k
do i=1,4
write(*,*) b(i,:,k)
enddo
enddo


end subroutine




function possible(row, col, bp, piec_index, pieces) result(is)
implicit none

integer :: row, col, piec_index
integer :: bp(4,4,4) 
integer, intent(in) :: pieces(16,4)
integer :: i,j,k
integer :: sumline

logical :: is

is=.true.

! scans all the positions to see if the piece fetched (index) was already on the boarda
do i = 1,4
 do j = 1,4
  if (bp(i,j,1).eq.pieces(piec_index,1).and. bp(i,j,2).eq.pieces(piec_index,2).and.bp(i,j,3).eq.pieces(piec_index,3).and.bp(i,j,4).eq.pieces(piec_index,4)) then
  is = .false.
  return
  endif
 enddo
enddo



! if the piece was not on the board, it tries to see if it's winning in the current position if placed
bp(row, col, :) = pieces(piec_index,:)

sumline=0
! sums by columns for each feature, if it wins the sum is +/- 4
do i=1,4
    do k=1,4
        sumline=sum(bp(i,:,k))
        if (abs(sumline).eq.4) then
        is=.false.
        return
        endif
    enddo
enddo


sumline=0
! same but for rows
do i=1,4
    do k=1,4
        sumline=sum(bp(:,i,k))
        if (abs(sumline).eq.4) then
        is = .false.
        return
        endif

    enddo
enddo

! now on the diagonals
do i=1,4
        sumline=0
    do k=1,4
        sumline=sumline+bp(k,k,i)
    enddo

        if (abs(sumline).eq.4) then
        is = .false.
        return
        endif
enddo

! antidiagonal
do i=1,4
        sumline=0
    do k=1,4
        sumline=sumline+bp(k,5-k,i)
    enddo
        if (abs(sumline).eq.4) then
        is = .false.
        return
        endif
enddo



end function
















! FIND ALL THE "ENDANGERED VARIABLES"
subroutine win_check(bp,win_var,win)
implicit none

integer :: bp(4,4,4),win_var(8)
integer :: i,j,k, sumline,twn
logical :: win

win_var=0


do i=1,4
    do k=1,4
        sumline=sum(bp(i,:,k))
        if (sumline.eq.3) then
         win_var(2*k-1)=1
        else if (sumline.eq.-3) then
         win_var(2*k)=1
        else if (abs(sumline).eq.4) then
        win=.true.
        return
        endif
    enddo
enddo

do i=1,4
    do k=1,4
        sumline=sum(bp(:,i,k))
        if (sumline.eq.3) then
         win_var(2*k-1)=1
        else if (sumline.eq.-3) then
         win_var(2*k)=1
        else if (abs(sumline).eq.4) then
        win=.true.
        return
        endif

    enddo
enddo


do i=1,4
        sumline=0
    do k=1,4
        sumline=sumline+bp(k,k,i)
    enddo

        if (sumline.eq.3) then
         win_var(2*i-1)=1
        else if (sumline.eq.-3) then
         win_var(2*i)=1
        else if (abs(sumline).eq.4) then
        win=.true.
        return
        endif
enddo


do i=1,4
        sumline=0
    do k=1,4
        sumline=sumline+bp(k,5-k,i)
    enddo
        if (sumline.eq.3) then
         win_var(2*i-1)=1
        else if (sumline.eq.-3) then
         win_var(2*i)=1
        else if (abs(sumline).eq.4) then
        win=.true.
        return
        endif
enddo


end subroutine

