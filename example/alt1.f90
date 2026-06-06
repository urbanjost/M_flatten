program demo_M_flatten
use M_flatten, only : flatten
implicit none
integer :: a
integer :: b0, b1(-1:1), b2(2,2), b3(2,2,1)

   a=0
   call wanted ( a, flatten(b0) )
   print *, 'a=', a, 'b0=', b0
   call wanted ( a, flatten(b1) )
   print *, 'a=', a, 'b1=', b1
   call wanted ( a, flatten(b2) )
   print *, 'a=', a, 'b2=', b2
   call wanted ( a, flatten(b3) )
   print *, 'a=', a, 'b3=', b3

contains

subroutine wanted( a, b)
integer, intent(inout) :: a
integer, intent(out)   :: b(:)
integer                :: i
   do i=1,size(b)
      a = a + 1
      b(i) = a
   enddo
end subroutine wanted
end program demo_M_flatten
