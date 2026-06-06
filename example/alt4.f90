program elem
   implicit none
   integer :: a
   integer :: b0, b1(-1:1), b2(2,2), b3(2,2,1)

   a=0
   call wanted( a, b0 )
   print *, 'a=', a, 'b0=', b0
   call wanted( a, b1 )
   print *, 'a=', a, 'b1=', b1
   call wanted( a, b2 )
   print *, 'a=', a, 'b2=', b2
   call wanted( a, b3 )
   print *, 'a=', a, 'b3=', b3

contains

   subroutine wanted( a, b )
      integer, intent(inout) :: a
      integer, intent(out)   :: b(..)

      integer :: i, j, k

      select rank (b)
      rank (0)
      a = a + 1
      b = a

      rank (1)
      do i = 1, size(b)
         a = a + 1
         b(i) = a
      enddo

      rank (2)
      do j = 1, size(b,dim=2)
         do i = 1, size(b,dim=1)
            a = a + 1
            b(i,j) = a
         enddo
      enddo

      rank (3)
      do k = 1, size(b,dim=3)
         do j = 1, size(b,dim=2)
            do i = 1, size(b,dim=1)
               a = a + 1
               b(i,j,k) = a
            enddo
         enddo
      enddo

      rank (*)
      print *, 'assumed size is unsupported'
      stop 1

      rank default
      print *,  'unsupported rank'
      stop 2

   end select

end subroutine wanted
end program elem
