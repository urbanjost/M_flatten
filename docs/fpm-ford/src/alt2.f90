program elem
implicit none
integer :: a
integer :: b0, b1(-1:1), b2(2,2), b3(2,2,1)

   a=0
   call wanted ( a, b0 )
   print *, 'a=', a, 'b0=', b0
   call wanted ( a, b1 )
   print *, 'a=', a, 'b1=', b1
   call wanted ( a, b2 )
   print *, 'a=', a, 'b2=', b2
   call wanted ( a, b3 )
   print *, 'a=', a, 'b3=', b3

contains

subroutine wanted( a, b)
use M_flatten, only : flatten
! This technique is known as pointer rank remapping (introduced in
! Fortran 2003 and expanded in Fortran 2008).
! It requires the multi-dimensional target array to be simply contiguous.
integer, intent(inout)                  :: a
integer,target, contiguous, intent(out) :: b(..)
integer                                 :: i
integer,pointer                         :: p_b(:)
   p_b=>flatten(b)
   do i=1,size(b)
      a = a + 1
      p_b(i) = a
   enddo
end subroutine wanted

end program elem
