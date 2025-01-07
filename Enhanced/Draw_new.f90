program main
use, intrinsic :: ieee_arithmetic
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
integer :: avrg_loss, avrg_win, avrg_prob, avrg_counter
integer,allocatable :: further_infos(:,:)

character*7 :: Prop(4)
character*1 :: mode
integer :: Unavaliable_pos(16,2), Unavaliable_pieces(16,4), Unavaliable_pos_backup(16,2), Unavaliable_pos_backup0(16,2)
integer :: Unav=1 !index for scrolling through
integer :: Unav_backup
integer :: holes
integer :: w,x,y,z !altri indici per altri loop
real :: Real_random
integer :: INT_RANDOM
integer :: fi
real, allocatable :: win_board_scores(:,:)
integer :: wb_index=1
integer :: win_index, loss_index, n_loss, n_win
! OTHER BACKUP VARIABLES
integer :: Unav_backup_2
integer :: Unavaliable_pieces_bu(16,4)
integer :: avail_piec_bu(16,5)
integer :: tmp_bp_bu(4,4,4)
integer :: Unavaliable_pos_bu(16,2)
logical :: advanced, exit_var2

integer :: avail_piec(16,5),avail_backup(16,5), avail_backup_init(16,5),tmp_ind ! array with the 16 pieces, the first column is says how many pieces are there (0 or 1)
character*3 :: string, name_piec




integer :: max_ahead=4 !index and max value of forward predicting
integer,parameter :: samples = 10



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
avail_backup_init=avail_piec


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




allocate(win_board_scores(holes,6))
allocate(further_infos(holes,4))
win_board_scores=0




! CHECKS IF THE INPUT COULD EXIST
call reality_check(Unavaliable_pos, Unavaliable_pieces)
call win_check(bp,win_var,win, advanced)
if (win) then
 write(*,*) 'Error: the provided structure is already winning.'
 stop
endif



write(*,*) 'Board:'
call show_board(bp)

if (max_ahead.lt.holes-1) then
max_ahead = holes-1
endif








!************************************************************
!************************************************************
!|||| START THE COMPUTATION OF THE ACTUAL GAME           ||||
!************************************************************
!************************************************************

Unavaliable_pos_backup0 = Unavaliable_pos
Unav_backup = Unav

open(unit=14, file='More_detail_draw.txt') ! IN THIS FILE THERE WILL BE ALL THE INFOS ABOUT EACH DRAWING PIECE






do draw = 1,16 ! scans all the pieces

avail_piec = avail_backup_init
Unav = Unav_backup
! Checks if this piece is available, if not it goes to the next one
if (avail_piec(draw,1).eq.0) then
    cycle
endif


! Now that we are sure that we have an available piece, let's set the Opponent_choice (my choice in this case) as the current piece
do ii=1,4
    Opponent_choice(ii) = avail_piec(draw,ii+1)
enddo
    avail_piec(draw,1) = 0
avail_piec_bu = avail_piec



! This is just to convert the 4 +1/-1 vector into a 3 letters word
call decode(Opponent_choice, name_piec)

! INITIALIZE SOME THINGS
blocked = 0 ! NUM OF POSITION BLOCKED (In which the opponent can't place the piece, if so it will loose)
wb_index = 1 ! Index for the win board array
win_var = 0 ! Array of 8 numbers, that are 0, then they are changed to 1 if there is a almost win in one of the 8 possible proprieties
exit_var2 = .false. ! Variable to exit the loops in some conditions (when true)

! I COPY THE INITIAL STATE IN SOME BACKUP VAR FOR RIGENERATING THEM LATER ON
!avail_backup = avail_backup_init 
Unavaliable_pos_backup = Unavaliable_pos_backup0



do r=1,4

! if it's true it will skipp till the end.
if (exit_var2) exit

do s=1,4

! RESET THE VARIABLES AS BEFORE THIS LOOP
 Unavaliable_pos=Unavaliable_pos_backup
 Unav=Unav_backup
 win=.false.
 win_var=0
 detect_in=.false.


! this looks for matchs between the currently occupied position and the analised one
 do l=1,16
 if (r.eq.Unavaliable_pos(l,1).and.s.eq.Unavaliable_pos(l,2)) then
    detect_in=.true.
    exit
 endif
 enddo
! if this is the case, skip to the next one
 if (detect_in) cycle



! PLACING IN (r,s) THE DRAWN PIECE
 tmp_bp=bp !Temporarely clone the table
 tmp_bp(r,s,:)=Opponent_choice(:) ! Put the given piece in the unoccupied spot



! Check for wich properties, putting this piece here (r,s) would result in a win for the opponent
 call win_check(tmp_bp,win_var,win, advanced)
! If, by placing this drawn piece you open a winning scenario for the opponent it will warn you not to give it this piece. 
 if (win) then
     write(*,*) '============================================='
     write(*,*) 'DO NOT GIVE THE PIECE  [', name_piec, ']  TO THE OPPONENT'
     write(*,*) '============================================='
     exit_var2 = .true. ! sets this exit_var2 as true so that it dosen't bother computing the odds... NEXT PIECE!
   exit
 endif
 check_exit=.false.


! If, by placing the drawn piece in (r,s) your opponent opens the possibility to win for 2 opposite proprieties (e.g. black and white) you will win!
  if (win_var(1)+win_var(2).eq.2.or.win_var(3)+win_var(4).eq.2.or.win_var(5)+win_var(6).eq.2.or.win_var(7)+win_var(8).eq.2) then
   check_exit=.true.
   n_loss = 0 ! none here will loose
   n_win = samples ! all of them will win
   win_index = 1 * samples ! in just one move for all samples
   loss_index = 0 ! no losses
   blocked = blocked + 1 ! This position will be blocked for this piece
! Now, if the blocked positions are the same as the holes in the game, none of them are good and you will win in either case!
  if (blocked.eq.holes) then
      write(*,*) 'GIVE THE PIECE  [', name_piec, ']  TO THE OPPONENT!'
      stop
  endif
! Sets the win boad scors in the standard format
     win_board_scores(wb_index,1) = r
     win_board_scores(wb_index,2) = s
     win_board_scores(wb_index,3) = real(win_index)/real(n_win) !computes the average number of mooves to win
     win_board_scores(wb_index,4) = real(loss_index)/real(n_loss) ! the loosing situations (none)
     win_board_scores(wb_index,5) = 100.d0 * real(n_win)/(real(n_win)+real(n_loss)) ! probability to win or loose
     win_board_scores(wb_index,6) = 100.d0 * real(n_loss)/(real(n_win)+real(n_loss))

! Bonus info for understanding better
     further_infos(wb_index,1) = n_win      
     further_infos(wb_index,2) = win_index
     further_infos(wb_index,3) = n_loss
     further_infos(wb_index,4) = loss_index
     wb_index=wb_index+1
    win_var = 0
  cycle
  endif


 Unav = Unav +1 ! I added a piece to the board, the unavailable pieces number gets bigger
 ! Now that: no win move is possible and you warned the user about suicidal moves, let's scan the scenario landscape  
 Unavaliable_pos(Unav,1) = r
 Unavaliable_pos(Unav,2) = s
 Unavaliable_pieces(Unav,:)=Opponent_choice(:)
 ! No need to update the avail array, already done at the very beginning


 Unav = Unav + 1 ! this is for the next step




! backups to initiate every montecarlo loop in the same board state
 Unav_backup_2=Unav
 Unavaliable_pieces_bu=Unavaliable_pieces
 tmp_bp_bu=tmp_bp
 Unavaliable_pos_bu=Unavaliable_pos


 ! THIS IS THE MONTECARLO PART
win_index = 0
loss_index = 0
n_win = 0
n_loss = 0
do x=1,samples

 win=.false.
 win_var=0
 Unav=Unav_backup_2
 Unavaliable_pieces=Unavaliable_pieces_bu
 avail_piec=avail_piec_bu
 tmp_bp=tmp_bp_bu
 Unavaliable_pos=Unavaliable_pos_bu
 
do fi=1,max_ahead

  
  call  place_random_piece(Unav, exit_var, Unavaliable_pos, avail_piec, tmp_bp)
  win=.false.

  call win_check(tmp_bp,win_var,win, advanced)


  ! now, the first move is the chosen piece (by the initial draw loop), that is done by the opponent (fi=0)
  ! this means that if the win is achieved in an even fi the the opponent won and you lost, if it's odd then you won.
  

  if (win.and.modulo(fi,2).eq.1.or.(win_var(1)+win_var(2).eq.2.or.win_var(3)+win_var(4).eq.2.or.win_var(5)+win_var(6).eq.2.or.win_var(7)+win_var(8).eq.2.and.modulo(fi,2).eq.0)) then
   win_index= win_index + fi ! set win index as the curret step in the MONTECARLO simulation
   n_win = n_win + 1 ! Count the number of winning situations 
 !write(*,*) '// WIN \\'
 !call show_pretty_board(tmp_bp)

   
   exit
  else if (win.and.modulo(fi,2).eq.0.or.(win_var(1)+win_var(2).eq.2.or.win_var(3)+win_var(4).eq.2.or.win_var(5)+win_var(6).eq.2.or.win_var(7)+win_var(8).eq.2.and.modulo(fi,2).eq.1)) then
   loss_index= loss_index + fi
   n_loss = n_loss + 1
   !write(*,*) '// LOSS \\'
 !call show_pretty_board(tmp_bp)
   exit
  endif


 enddo


enddo

 ! win board scores is an array that display the current positioning of the opponent's piece in the first two columns
 win_board_scores(wb_index,1) = r
 win_board_scores(wb_index,2) = s
 win_board_scores(wb_index,3) = real(win_index)/real(n_win)
 win_board_scores(wb_index,4) = real(loss_index)/real(n_loss)
 win_board_scores(wb_index,5) = 100.d0 * real(n_win)/(real(n_win)+real(n_loss))
 win_board_scores(wb_index,6) = 100.d0 * real(n_loss)/(real(n_win)+real(n_loss))


 further_infos(wb_index,1) = n_win
 further_infos(wb_index,2) = win_index
 further_infos(wb_index,3) = n_loss
 further_infos(wb_index,4) = loss_index

 wb_index=wb_index+1


!avail_piec=avail_backup
enddo
enddo

if (exit_var2) cycle ! if the piece makes the opponent instantely win there is no point in computing any further

avrg_loss = 0
avrg_win = 0
avrg_prob = 0
avrg_counter = 0

do i = 1,holes
    if (win_board_scores(i,1).eq.0) cycle
    if (ieee_is_nan(win_board_scores(i,5))) cycle
    avrg_win = avrg_win + win_board_scores(i,3)
    avrg_loss = avrg_loss + win_board_scores(i,4)
    avrg_prob = avrg_prob + win_board_scores(i,5)
    avrg_counter = avrg_counter + 1
enddo



write(14,*) '========================================'
 write(14,*) 'Piece: [', name_piec, ']'
 write(14,*) 'r;  ', 's;    ', 'avrg win moves;   ', 'avrg loss moves;  ', 'win prob;     ', 'loss prob;    ', 'win scenarios;   ', 'win moves tot;   ', 'loss scenarios;  ', 'loss moves tot  '
 do ii = 1, holes
     if (win_board_scores(ii,1).eq.0d0) cycle
     write(14,*) win_board_scores(ii,:),'|', further_infos(ii,:)
 enddo
    write(14,*) 'avrg_win', real(avrg_win)/real(avrg_counter)
    write(14,*) 'avrg_loss', real(avrg_loss)/real(avrg_counter)
    write(14,*) 'avrg_prob', real(avrg_prob)/real(avrg_counter)
    write(14,*) 'avrg_counter', avrg_counter
if (real(avrg_prob) .eq.0) then
    write(*,*) '============================================='
    write(*,*) 'DO NOT GIVE THE PIECE  [', name_piec, ']  TO THE OPPONENT.'
    write(*,*) '============================================='
    win_board_scores = 0.d0
    cycle
endif

    write(*,*) '---------------------------------------------'
    write(*,*) 'FOR PIECE  [', name_piec,']'
    write(*,*) 'Average moves to loose:' ! in this case the opponent's win is a loss for me
    write(*,*) real(avrg_loss) / real(avrg_counter)
    write(*,*) 'Average moves to win:' ! in this case the opponent's win is a loss for me
    write(*,*) real(avrg_win) / real(avrg_counter)
    write(*,*) 'Average probability of win:'
    write(*,*) real(avrg_prob) / real(avrg_counter)

win_board_scores = 0.d0

enddo


close(13)
close(14)









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

if (Opp_ch(1).eq.0) then
    v1 = '0' 
    v2 = ' '
    v3 = ' '
endif
name_piec = v1 // v2 // v3

end subroutine


subroutine show_pretty_board(bp)
implicit none
integer :: bp(4,4,4), piec(4)
integer :: i,j
character*3 :: name_piec
character*3 :: string_board(4)
do i = 1,4
    do j = 1,4
        piec(:) = bp(i,j,:)
        call decode(piec,name_piec)
        string_board(j) = name_piec
    enddo
    write(*,110) string_board(:)
enddo
        

110 format(A3,5X,A3,5X,A3,5X,A3)
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
character*3 :: string
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
! is the (x,y) position in the list of unavailable positions?
 do l=1,16
 if (x.eq.Unavaliable_pos(l,1).and.y.eq.Unavaliable_pos(l,2)) then
    detect_in=.true.
    exit
 endif
 enddo

! if this is the case, skip to the next one
 if (detect_in) cycle

! if the (x,y) position is available then it reaches here 
 if (INT_RANDOM.eq.1) then
  call random_piece(avail_piec,rndm_piece, 16-Unav)
  call decode(rndm_piece,string)
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
 rndm=rndm-1
  if (rndm.eq.0) then
  do i=2,5
   piece(i-1)=av(l,i)
  enddo
  return
  endif
 endif
enddo

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
        elseif (sumline.eq.-3) then
         win_var(2*k)=1
        elseif (abs(sumline).eq.4) then
        win=.true.
        return
   endif
  enddo
 enddo
enddo

endif







end subroutine
