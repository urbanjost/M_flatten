program elem
   implicit none
   integer :: a = 0
   integer :: b0=-1, b1(-1:1)=-1, b2(2,2)=-1, b3(2,2,2)=-1

   print *, 'scalar:'
   call wanted( a, b0 )
   print *, 'a=', a, 'b0=', b0

   print *, 'multi-dimensional:'
   call wanted( a, b1 )
   print *, 'a=', a, 'b1=', b1

   call wanted( a, b2 )
   print *, 'a=', a, 'b2=', b2

   call wanted( a, b3 )
   print *, 'a=', a, 'b3=', b3

   print *, 'non-contigouous:'
   call wanted( a, b3(2,:,:) )  ! noncontiguous slice
   print *, 'a=', a, 'b3=', b3

contains

subroutine wanted( a, b )
   use, intrinsic :: iso_c_binding
   use, intrinsic :: iso_fortran_env, only: int64
   integer, intent(inout)       :: a
   integer, target, intent(out) :: b(..)
   integer, pointer             :: p_b(:)
   integer                      :: i, n

   n=size(b)
   select rank (b)
   rank (0);  call c_f_pointer(c_loc(b), p_b, [1]) ! Map the scalar's memory address to a 1D array pointer of size 1
   rank (1);  p_b(1:n)=>b
   rank (2);  p_b(1:n)=>b
   rank (3);  p_b(1:n)=>b
   rank (4);  p_b(1:n)=>b
   rank (5);  p_b(1:n)=>b
   rank (6);  p_b(1:n)=>b
   rank (7);  p_b(1:n)=>b
   rank (8);  p_b(1:n)=>b
   rank (9);  p_b(1:n)=>b
   rank (10); p_b(1:n)=>b
   rank (11); p_b(1:n)=>b
   rank (12); p_b(1:n)=>b
   rank (13); p_b(1:n)=>b
   rank (14); p_b(1:n)=>b
   rank (15); p_b(1:n)=>b
   rank (*)
   print *, 'assumed size is unsupported'
   rank default
   print *,  'unsupported rank'
   end select
   do i = 1, size(p_b)
      a = a + 1
      p_b(i) = a
   enddo
end subroutine wanted

end program elem
