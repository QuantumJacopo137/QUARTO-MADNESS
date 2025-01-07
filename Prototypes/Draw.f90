program main

implicit none


integer :: board(4,4), bp(4,4,4), matarray(16), numbers(15), tmp_bp(4,4,4), tmp_bp1(4,4,4),rndm_piece(4)
integer :: Opponent_choice(4),my_choice(4),win_lose_board(2,4,4) !nell'ultimo metto la percentuale di vincite e di sconfitte in ogni pun
integer :: i,j,k,l,tmp 
logical :: check_exit=.false., detect_in=.false., win=.false., exit_var=.false.
integer :: wn ! win number, 0 if no win detected, 1 if yes
integer :: bin(4), win_var(8)
integer :: r,s,t, nsteps, ciclo, ii, jj, ri, rj, draw
integer :: max_indices(1,2) !extracts the index of the most probable winning state
integer :: max_index
integer :: blocked ! the positions in which the opponent won't be able to place the next piece



character*7 :: Prop(4)
character*1 :: mode
integer :: Unavaliable_pos(16,2), Unavaliable_pieces(16,4), Unavaliable_pos_backup(16,2)
integer :: Unav=1 !index for scrolling through
integer :: Unav_backup
integer :: holes
integer :: w,x,y,z !altri indici per altri loop
real :: Real_random
integer :: INT_RANDOM
integer :: fi
real, allocatable :: win_board_scores(:,:)
integer :: wb_index=1
integer :: win_index=0
! OTHER BACKUP VARIABLES
integer :: Unav_backup_2
integer :: Unavaliable_pieces_bu(16,4)
integer :: avail_piec_bu(16,5)
integer :: tmp_bp_bu(4,4,4)
integer :: Unavaliable_pos_bu(16,2)
logical :: advanced, exit_var2

integer :: avail_piec(16,5),avail_backup(16,5),tmp_ind ! array with the 16 pieces, the first column is says how many pieces are there (0 or 1)
character*3 :: string, name_piec




integer :: max_ahead=4 !index and max value of forward predicting
integer,parameter :: samples = 50000



! PIECES AVAIALBLE ARRAY

avail_piec(:,1)=1
tmp_ind=1

do i=-1,1,2
do j=-1,1,2
do k=-1,1,2
do l=-1,1,2

avail_piec(tmp_ind,2)=i
avail_piec(tmp_ind,3)=j
avail_piec(tmp_ind,4)=k
avail_piec(tmp_ind,5)=l
tmp_ind=tmp_ind+1
enddo   
enddo   
enddo   
enddo


call random_seed()

102 format(A10, 2X)

 board=0
 bp=0
 Unavaliable_pos=0
 Unavaliable_pieces=0


!read_input(input_file_name, current_board, tot_holes, tot_piec)

call read_input('Board.inp', bp, holes, Unav, avail_piec, Unavaliable_pos, Unavaliable_pieces, advanced)

if (advanced) then
    write(*,*) '=================================================='
    write(*,*) 'Advanced rules.'
    write(*,*) '=================================================='
else
    write(*,*) '=================================================='
    write(*,*) 'Standard rules.'
    write(*,*) '=================================================='
endif

! quik debug
!write(*,*) 'Unav =', Unav, 'Holes =', holes

!write(*,*) 'Available pieces:'
!do i = 1,16
! write(*,*) avail_piec(i,:)
!enddo


!write(*,*) 'Unavailable Positions'
!do i = 1,16
! write(*,*) Unavaliable_pos(i,:)
!enddo
!write(*,*) 'Unavailable Pieces'
!do i = 1,16
! write(*,*) Unavaliable_pieces(i,:)
!enddo




allocate(win_board_scores(holes,3))
win_board_scores=0



!call show_board(bp)

! CHECKS IF THE INPUT COULD EXIST
call reality_check(Unavaliable_pos, Unavaliable_pieces)
call win_check(bp,win_var,win, advanced)
if (win) then
 write(*,*) 'Error: the provided structure is already winning.'
 stop
endif



write(*,*) 'Board:'
call show_board(bp)

if (max_ahead.lt.holes) then
max_ahead = holes
endif








!************************************************************
!************************************************************
!|||| START THE COMPUTATION OF THE ACTUAL GAME           ||||
!************************************************************
!************************************************************




do draw = 1,16
if (avail_piec(draw,1).eq.0) then
    cycle
endif

do ii=1,4
    Opponent_choice(ii) = avail_piec(draw,ii+1)
enddo

call decode(Opponent_choice, name_piec)




blocked = 0
wb_index = 1 
win_var = 0
avail_backup = avail_piec
Unavaliable_pos_backup = Unavaliable_pos
Unav_backup = Unav
exit_var2 = .false.
do r=1,4

if (exit_var2) exit
do s=1,4
! RESET THE VARIABLES AS BEFORE THIS LOOP
 Unavaliable_pos=Unavaliable_pos_backup
 Unav=Unav_backup
 detect_in=.false.
 win=.false.
 win_var=0

! this looks for matchs between the currently occupied position and the analised one
 do l=1,16
 if (r.eq.Unavaliable_pos(l,1).and.s.eq.Unavaliable_pos(l,2)) then
    detect_in=.true.
    exit
 endif
 enddo
 
! if this is the case, skip to the next one
 if (detect_in) cycle

 tmp_bp=bp !Temporarely clone the table
 tmp_bp(r,s,:)=Opponent_choice(:) ! Put the given piece in the unoccupied spot

 call win_check(tmp_bp,win_var,win,advanced) ! Check for wich properties, putting this piece here (r,s) would result in a win for the opponent
 if (win) then
     write(*,*) 'DO NOT GIVE THE PIECE  [', name_piec, ']  TO THE OPPONENT'
     exit_var2 = .true.
   exit
 endif
 check_exit=.false.

 do l=1,4

  if (win_var(2*l-1).eq.1.and.win_var(2*l).eq.1) then
   check_exit=.true. ! If this moove will make both opposite of a variable win than it will not bother calculate, since it looses
   exit
  endif

 enddo

 ! Inform the player what should absolutely not do
 if (check_exit) then
  blocked = blocked + 1
  if (blocked.eq.holes) then
      write(*,*) 'GIVE THE PIECE  [', name_piec, ']  TO THE OPPONENT!'
      stop
  endif
 check_exit=.false.
 cycle
 endif
 
 ! Now that: no win move is possible and you warned the user about suicidal moves, let's scan the scenario landscape  
 Unavaliable_pos(Unav,1) = r
 Unavaliable_pos(Unav,2) = s
! 
 !win_board_scores(wb_index,1)=r
 !win_board_scores(wb_index,2)=s
!



 Unavaliable_pieces(Unav,:)=Opponent_choice(:)

 call UPDATE_AVAIL(avail_piec,Opponent_choice(1),Opponent_choice(2),Opponent_choice(3),Opponent_choice(4))
 
 Unav=Unav+1


! backups to initiate every montecarlo loop in the same board state
 Unav_backup_2=Unav
 Unavaliable_pieces_bu=Unavaliable_pieces
 avail_piec_bu=avail_piec
 tmp_bp_bu=tmp_bp
 Unavaliable_pos_bu=Unavaliable_pos


 ! THIS IS THE MONTECARLO PART
win_index=0
do x=1,samples

 win=.false.
 Unav=Unav_backup_2
 Unavaliable_pieces=Unavaliable_pieces_bu
 avail_piec=avail_piec_bu
 tmp_bp_bu=tmp_bp
 Unavaliable_pos=Unavaliable_pos_bu
 
 do fi=1,max_ahead
  
!  write(*,*) 'fi=', fi
  
  call  place_random_piece(Unav, exit_var, Unavaliable_pos, avail_piec, tmp_bp)
  !call show_board(tmp_bp)
  win=.false.
  
  call win_check(tmp_bp,win_var,win, advanced) 
 
  if (win.and.modulo(fi,2).eq.1) then
   win_index=win_index-(1+max_ahead-((fi+1)/2))
 
  else if (win.and.modulo(fi,2).eq.0) then
   win_index=win_index+(1+max_ahead-(fi/2))
   exit
  endif


 enddo


enddo
 

 ! win board scores is an array that display the current positioning of the opponent's piece in the first two columns
 ! in the 3rd one it stores the number of weighted win scores 
 win_board_scores(wb_index,1)=r
 win_board_scores(wb_index,2)=s
 win_board_scores(wb_index,3)=win_index


 wb_index=wb_index+1
 !Unav=Unav+1


avail_piec=avail_backup
!call show_board(tmp_bp)

!if (exit_var) exit
enddo
!if (exit_var) exit
enddo



if (.not.exit_var2) then
write(*,*) name_piec, minval(win_board_scores(:,3)), maxval(win_board_scores(:,3))
endif
win_board_scores = 0


enddo


close(13)










end program




!*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
!*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
!*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
!*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
!*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
!*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
!*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-


subroutine decode(Opp_ch, name_piec)
implicit none
integer :: Opp_ch(4)
character*1 :: v1,v2,v3
character*3 :: name_piec

if (Opp_ch(1).eq.+1) then
    if (Opp_ch(2).eq.+1) then
        v1 = 'B'
    elseif (Opp_ch(2).eq.-1) then
        v1 = 'b'
    endif

elseif (Opp_ch(1).eq.-1) then
    if (Opp_ch(2).eq.+1) then
        v1 = 'W'
    elseif (Opp_ch(2).eq.-1) then
        v1 = 'w'
    endif
endif

if (Opp_ch(3).eq.+1) then
        v2 = 's'
    elseif (Opp_ch(3).eq.-1) then
        v2 = 'c'
endif

if (Opp_ch(4).eq.+1) then
        v3 = 'h'
    elseif (Opp_ch(4).eq.-1) then
        v3 = 's'
endif


name_piec = v1 // v2 // v3

end subroutine




subroutine read_input(input_file_name, current_board, tot_holes, tot_piec,avail_piec, Unavaliable_pos, Unavaliable_pieces, advanced)
implicit none
character*9 :: input_file_name
integer :: current_board(4,4,4), avail_piec(16,5)
integer :: Unavaliable_pos(16,2), Unavaliable_pieces(16,4)
integer :: tot_holes,tot_piec
integer :: i, j, k, tmp=1
character*3 :: cbp(4,4), string
character*1 :: rules
logical :: advanced 

open(unit=29, file=input_file_name)

tot_holes = 0

do i=1,4
  read(29,*) cbp(i,:)
enddo

 read(29,*) rules

if (rules.eq.'A'.or.rules.eq.'a') then
 advanced = .true.
elseif (rules.eq.'S'.or.rules.eq.'s') then
 advanced = .false.
else
 write(*,*) 'Error: No game rules specified in the input!'
 stop
endif
 

! color, height, shape, filling

!UPDATE_AVAIL(AL,v1,v2,v3,v4): synthax of the subroutine that updates the available pieces

do i=1,4
 do j=1,4
  string = cbp(i,j)
  select case (string)
! take as a reference black, tall, square, hole = [+1,+1,+1,+1]
    case ('Bsh')
    current_board(i,j,:) = [+1,+1,+1,+1]
    call UPDATE_AVAIL(avail_piec,1,1,1,1)
    Unavaliable_pos(tmp,1)= i
    Unavaliable_pos(tmp,2)= j
    Unavaliable_pieces(tmp,:) = [+1,+1,+1,+1]
    tmp = tmp + 1
    case ('Bss')
    current_board(i,j,:) = [+1,+1,+1,-1]
    call UPDATE_AVAIL(avail_piec,1,1,1,-1)
    Unavaliable_pos(tmp,1)= i
    Unavaliable_pos(tmp,2)= j
    Unavaliable_pieces(tmp,:) = [+1,+1,+1,-1]
    tmp = tmp + 1
    case ('Bch')
    current_board(i,j,:) = [+1,+1,-1,+1]
    call UPDATE_AVAIL(avail_piec,1,1,-1,1)
    Unavaliable_pos(tmp,1)= i
    Unavaliable_pos(tmp,2)= j
    Unavaliable_pieces(tmp,:) = [+1,+1,-1,+1]
    tmp = tmp + 1
    case ('Bcs')
    current_board(i,j,:) = [+1,+1,-1,-1]
    call UPDATE_AVAIL(avail_piec,1,1,-1,-1)
    Unavaliable_pos(tmp,1)= i
    Unavaliable_pos(tmp,2)= j
    Unavaliable_pieces(tmp,:) = [+1,+1,-1,-1]
    tmp = tmp + 1
    case ('bsh')
    current_board(i,j,:) = [+1,-1,+1,+1]
    call UPDATE_AVAIL(avail_piec,1,-1,1,1)
    Unavaliable_pos(tmp,1)= i
    Unavaliable_pos(tmp,2)= j
    Unavaliable_pieces(tmp,:) = [+1,-1,+1,+1]
    tmp = tmp + 1
    case ('bss')
    current_board(i,j,:) = [+1,-1,+1,-1]
    call UPDATE_AVAIL(avail_piec,1,-1,1,-1)
    Unavaliable_pos(tmp,1)= i
    Unavaliable_pos(tmp,2)= j
    Unavaliable_pieces(tmp,:) = [+1,-1,+1,-1]
    tmp = tmp + 1
    case ('bch')
    current_board(i,j,:) = [+1,-1,-1,+1]
    call UPDATE_AVAIL(avail_piec,1,-1,-1,1)
    Unavaliable_pos(tmp,1)= i
    Unavaliable_pos(tmp,2)= j
    Unavaliable_pieces(tmp,:) =  [+1,-1,-1,+1]
    tmp = tmp + 1
    case ('bcs')
    current_board(i,j,:) = [+1,-1,-1,-1]
    call UPDATE_AVAIL(avail_piec,1,-1,-1,-1)
    Unavaliable_pos(tmp,1)= i
    Unavaliable_pos(tmp,2)= j
    Unavaliable_pieces(tmp,:) = [+1,-1,-1,-1]
    tmp = tmp + 1
    case ('Wsh')
    current_board(i,j,:) = [-1,+1,+1,+1]
    call UPDATE_AVAIL(avail_piec,-1,1,1,1)
    Unavaliable_pos(tmp,1)= i
    Unavaliable_pos(tmp,2)= j
    Unavaliable_pieces(tmp,:) = [-1,+1,+1,+1]
    tmp = tmp + 1
    case ('Wss')
    current_board(i,j,:) = [-1,+1,+1,-1]
    call UPDATE_AVAIL(avail_piec,-1,1,1,-1)
    Unavaliable_pos(tmp,1)= i
    Unavaliable_pos(tmp,2)= j
    Unavaliable_pieces(tmp,:) =  [-1,+1,+1,-1]
    tmp = tmp + 1
    case ('Wch')
    current_board(i,j,:) = [-1,+1,-1,+1]
    call UPDATE_AVAIL(avail_piec,-1,1,-1,1)
    Unavaliable_pos(tmp,1)= i
    Unavaliable_pos(tmp,2)= j
    Unavaliable_pieces(tmp,:) = [-1,+1,-1,+1]
    tmp = tmp + 1
    case ('Wcs')
    current_board(i,j,:) = [-1,+1,-1,-1]
    call UPDATE_AVAIL(avail_piec,-1,1,-1,-1)
    Unavaliable_pos(tmp,1)= i
    Unavaliable_pos(tmp,2)= j
    Unavaliable_pieces(tmp,:) = [-1,+1,-1,-1]
    tmp = tmp + 1
    case ('wsh')
    current_board(i,j,:) = [-1,-1,+1,+1]
    call UPDATE_AVAIL(avail_piec,-1,-1,1,1)
    Unavaliable_pos(tmp,1)= i
    Unavaliable_pos(tmp,2)= j
    Unavaliable_pieces(tmp,:) = [-1,-1,+1,+1]
    tmp = tmp + 1
    case ('wss')
    current_board(i,j,:) = [-1,-1,+1,-1]
    call UPDATE_AVAIL(avail_piec,-1,-1,1,-1)
    Unavaliable_pos(tmp,1)= i
    Unavaliable_pos(tmp,2)= j
    Unavaliable_pieces(tmp,:) = [-1,-1,+1,-1]
    tmp = tmp + 1
    case ('wch')
    current_board(i,j,:) = [-1,-1,-1,+1]
    call UPDATE_AVAIL(avail_piec,-1,-1,-1,1)
    Unavaliable_pos(tmp,1)= i
    Unavaliable_pos(tmp,2)= j
    Unavaliable_pieces(tmp,:) =  [-1,-1,-1,+1]
    tmp = tmp + 1
    case ('wcs')
    current_board(i,j,:) = [-1,-1,-1,-1]
    call UPDATE_AVAIL(avail_piec,-1,-1,-1,-1)
    Unavaliable_pos(tmp,1)= i
    Unavaliable_pos(tmp,2)= j
    Unavaliable_pieces(tmp,:) =  [-1,-1,-1,-1]
    tmp = tmp + 1


    case default
    current_board(i,j,:) = [0,0,0,0]
    tot_holes = tot_holes + 1
    end select
 enddo
enddo




tot_piec = 16 - tot_holes

close(29)
end subroutine








subroutine place_random_piece(Unav, exit_var, Unavaliable_pos, avail_piec, tmp_bp)
implicit none

real :: Real_random
integer :: x,y,l
integer :: Unav, INT_RANDOM
integer :: Unavaliable_pos(16,2), avail_piec(16,5), rndm_piece(4), tmp_bp(4,4,4)
logical :: detect_in, exit_var

exit_var=.false.

 call random_seed()
! NOW, FIRST IT EXTRACTS A RANDOM NUMBER FROM 1 TO 16-UNAV (Holes in the checkboard)  
 call random_number(Real_random)
 INT_RANDOM=1+int((15-Unav)*Real_random)

! SCANS ALL THE 16 POSITIONS, EVERY TIME IT FINDS A HOLE THE NUMBER IS REDUCED UNTILL IS =1, THEN IT PLACES A RANDOM PIECE IN THIS POSITION

 do x=1,4
 if (exit_var) exit
 do y=1,4
 detect_in=.false.
 do l=1,16
 if (x.eq.Unavaliable_pos(l,1).and.y.eq.Unavaliable_pos(l,2)) then
    detect_in=.true.
    exit
 endif
 enddo

! if this is the case, skip to the next one
 if (detect_in) cycle

 if (INT_RANDOM.eq.1) then
  call random_piece(avail_piec,rndm_piece, 16-Unav)
  tmp_bp(x,y,:)=rndm_piece(:)
  call UPDATE_AVAIL(avail_piec,rndm_piece(1),rndm_piece(2),rndm_piece(3),rndm_piece(4))
  exit_var=.true.
  Unavaliable_pos(Unav,1)=x
  Unavaliable_pos(Unav,2)=y
  exit
 endif
 INT_RANDOM=INT_RANDOM-1



 enddo
 enddo
 
Unav=Unav+1        !HO CAMBIATO QUESTO CHE PRIMA ERA IN FONDO


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




! UPDATE THE AVAIL PIECES: by deleting the piece with variables v1,v2,v3,v4
subroutine UPDATE_AVAIL(AL,v1,v2,v3,v4)
implicit none

integer :: AL(16,5), v1,v2,v3,v4, l

do l=1,16
if (AL(l,2).eq.v1.and.AL(l,3).eq.v2.and.AL(l,4).eq.v3.and.AL(l,5).eq.v4) then
AL(l,1)=0
exit
endif
enddo

return

end subroutine











subroutine random_piece(av,piece, holes)
implicit none

integer :: av(16,5), piece(4), l, rndm, holes, i
real :: rrandom

call random_seed()
call random_number(rrandom)

rndm=int(1+rrandom*(holes-1))

do l=1,16
 if (av(l,1).eq.0) then
  cycle
 else
  if (rndm.eq.1) then
  do i=2,5
   piece(i-1)=av(l,i)
  enddo
  endif
 rndm=rndm-1
 endif
enddo

return
end subroutine










! THIS ROUTINE LOOKS FOR INCONSISTENCIES IN THE INPUT
! IS AN ERROR SEEKING ROUTINE
subroutine reality_check(up, upiec)
implicit none

integer i,j
integer up(16,2), upiec(16,4)


! NOW IF THE PROGRAM FINDS THAT THERE ARE 2 PIECES IN THE SAME PLACE IT STOPS
do i=1,15


do j=1+i,16

if (Up(j,1).ne.0.and.Up(i,1).eq.Up(j,1).and.Up(i,2).eq.Up(j,2)) then
 write(*,*) 'Error: Something is wrong! 2 Pieces are in the same place!'
 stop
else if (Up(j,1).eq.0.and.Up(j,1).eq.0) then
 exit
endif

enddo

if (Up(i,1).eq.0.and.Up(i,1).eq.0) then
 exit
endif

enddo

do i=1,16
do j=1,4
if (abs(upiec(i,j)).gt.1) then
 write(*,*) 'Error: Only acceptacle inputs are +1 o -1!'
 stop
endif
enddo


do j=i+1,16
 if (upiec(i,1).ne.0.and.upiec(i,1).eq.upiec(j,1).and.upiec(i,2).eq.upiec(j,2).and.upiec(i,3).eq.upiec(j,3).and.upiec(i,4).eq.upiec(j,4)) then
  write(*,*) 'Error: you used 2 identical pieces!'
  stop
 else if (upiec(j,1).eq.0.and.upiec(j,2).eq.0.and.upiec(j,3).eq.0.and.upiec(j,4).eq.0) then
  exit
 endif
enddo

if (upiec(i,1).eq.0.and.upiec(i,2).eq.0.and.upiec(i,3).eq.0.and.upiec(i,4).eq.0) exit

enddo

endsubroutine




! IS HERE JUST IN CASE I WANT TO CONVERT A PIECE IN THE CORRESPONDANT BINARY
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







! FIND ALL THE "ENDANGERED VARIABLES"
subroutine win_check(bp,win_var,win,advanced)
implicit none

integer :: bp(4,4,4),win_var(8)
integer :: i,j,k, sumline,twn
logical :: win, advanced

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


if (advanced) then

do i = 1,3
 do j = 1,3
  do k = 1,4
   sumline = 0
   sumline = bp(i,j,k) + bp(i+1,j,k) + bp(i,j+1,k) + bp(i+1,j+1,k)
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
enddo

endif







end subroutine
