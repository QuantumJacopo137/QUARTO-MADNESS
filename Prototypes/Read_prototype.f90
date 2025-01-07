program main
implicit none
integer :: bp(4,4,4)
integer :: holes, tpiec, i,k


call read_input('Board.inp', bp, holes, tpiec)

do k = 1,4
 do i=1,4
write(*,*) bp(i,:,k)
enddo
enddo 


end program


subroutine read_input(input_file_name, current_board, tot_holes, tot_piec)
implicit none
character*16 :: input_file_name
integer :: current_board(4,4,4)
integer :: tot_holes,tot_piec
integer :: i, j, k
character*3 :: cbp(4,4), string

open(unit=29, file=input_file_name)

tot_holes = 0

do i=1,4
  read(29,*) cbp(i,:)
enddo

! color, height, shape, filling



do i=1,4
 do j=1,4
  string = cbp(i,j)
  select case (string)
! take as a reference white, tall, square, hole
    case ('Bsh')
    current_board(i,j,:) = [+1,+1,+1,+1]
    case ('Bss')
    current_board(i,j,:) = [+1,+1,+1,-1]
    case ('Bch')
    current_board(i,j,:) = [+1,+1,-1,+1]
    case ('Bcs')
    current_board(i,j,:) = [+1,+1,-1,-1]
    case ('bsh')
    current_board(i,j,:) = [+1,-1,+1,+1]
    case ('bss')
    current_board(i,j,:) = [+1,-1,+1,-1]
    case ('bch')
    current_board(i,j,:) = [+1,-1,-1,+1]
    case ('bcs')
    current_board(i,j,:) = [+1,-1,-1,-1]
    case ('Wsh')
    current_board(i,j,:) = [-1,+1,+1,+1]
    case ('Wss')
    current_board(i,j,:) = [-1,+1,+1,-1]
    case ('Wch')
    current_board(i,j,:) = [-1,+1,-1,+1]
    case ('Wcs')
    current_board(i,j,:) = [-1,+1,-1,-1]
    case ('wsh')
    current_board(i,j,:) = [-1,-1,+1,+1]
    case ('wss')
    current_board(i,j,:) = [-1,-1,+1,-1]
    case ('wch')
    current_board(i,j,:) = [-1,-1,-1,+1]
    case ('wcs')
    current_board(i,j,:) = [-1,-1,-1,-1]


    case default
    current_board(i,j,:) = [0,0,0,0]
    tot_holes = tot_holes + 1
    end select
 enddo
enddo




tot_piec = 16 - tot_holes

close(29)
end subroutine
