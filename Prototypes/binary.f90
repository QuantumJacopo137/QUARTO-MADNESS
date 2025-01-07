program main

implicit none

integer :: num, tmp, i,j,k, expmax


write(*,*) 'Numero trial:'
read(*,*) num


do i=1,num
    tmp=2**i
    if (tmp.ge.num) then
    expmax=i
        exit 
    endif
enddo

write(*,*) 'max exponent', expmax


do i=0,expmax
    tmp=num/(2**(expmax-i))
    num=num-tmp*2**(expmax-i)
    write(*,*) expmax-i, tmp
!    write(*,*) 'num', num
enddo

end program
