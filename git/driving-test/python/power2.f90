PROGRAM power
 INTEGER :: i, value
 CHARACTER(len=32) :: arg
 CALL getarg(1, arg)
 read (arg,'(I10)') value
 call power2(value)
END PROGRAM
!
subroutine power2(i)
implicit none
integer:: i,x,c,p
integer::products(10)
x = 1
c = 0
do
   if(i<=0) exit
   p = 1
   if(p == 1) then
      products(c)=x
       c=c+1
   end if
   x = x*2
   i = ishft(i,-1)
end do
! Print the powers
do i=1,c-1
   write(*,*) products(i)
end do
end
