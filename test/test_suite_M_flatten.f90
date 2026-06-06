program runtest
use,intrinsic :: iso_c_binding, only: c_int, c_char, c_null_char
use, intrinsic :: iso_fortran_env, only : stdout=>OUTPUT_UNIT, stderr=>ERROR_UNIT
use M_framework, only : unit_test, unit_test_end, unit_test_mode
use M_framework, only : unit_test_start, unit_test_msg, unit_test_level
use M_framework, only : unit_test_stop
use M_framework, only: str

use M_flatten,  only: flatten
implicit none
logical, parameter :: T=.true., F=.false.
integer,parameter  :: dp=kind(0.0d0)

call unit_test_mode(  &
   keep_going=T,      &
   flags=[0],         &
   luns=[stdout],     &
   command='',        &
   brief=F,           &
   match='',          &
   interactive=F,     &
   CMDLINE=T,         &
   debug=F)

unit_test_level=0


call unit_test_msg('M_flatten','This section contains unit tests for procedures in the M_flatten(3f) module.')

call test_flatten()
call unit_test_stop('M_flatten tests completed')

contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_flatten()
integer,allocatable   :: expected(:)
integer,allocatable   :: returned(:)
integer :: a
integer :: b0, b1(-1:1), b2(2,2), b3(2,2,1)

   call unit_test_start('flatten        ','scalar test')

   a=0
   call wanted ( a, b0 )
   call unit_test('flatten',b0==1 ,msg="scalar")
   expected=[2,3,4]
   call wanted ( a, b1 )
   call unit_test('flatten',all(b1==expected) ,"rank one",str(b1))
   expected=[5,6,7,8]
   call wanted ( a, b2 )
   call unit_test('flatten',all(pack(b2,.true.)==expected) ,"rank two",str(pack(b2,.true.)))
   expected=[9,10,11,12]
   call wanted ( a, b3 )
   call unit_test('flatten',all(pack(b3,.true.)==expected) ,"rank three",str(pack(b3,.true.)))

   ! Alternatively, to avoid using pointers directly
   ! write the called routine to expect a flattened
   ! array and call the argument with flatten().

   a=0
   call wanted1 ( a, flatten(b0) )
   call unit_test('flatten',b0==1 ,msg="scalar")
   expected=[2,3,4]
   call wanted1 ( a, flatten(b1) )
   call unit_test('flatten',all(b1==expected) ,"rank one",str(b1))
   expected=[5,6,7,8]
   call wanted1 ( a, flatten(b2) )
   call unit_test('flatten',all(pack(b2,.true.)==expected) ,"rank two",str(pack(b2,.true.)))
   expected=[9,10,11,12]
   call wanted1 ( a, flatten(b3) )
   call unit_test('flatten',all(pack(b3,.true.)==expected) ,"rank three",str(pack(b3,.true.)))

   call unit_test_end('flatten')

end subroutine test_flatten

subroutine wanted1( a, b)
integer, intent(inout) :: a
integer, intent(out)   :: b(:)
integer                :: i
   do i=1,size(b)
      a = a + 1
      b(i) = a
   enddo
end subroutine wanted1

subroutine wanted( a, b)
! This technique is known as pointer rank remapping (introduced in Fortran 2003 and expanded in Fortran 2008).
! requires the multi-dimensional target array is simply contiguous.
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
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT

end program runtest
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
