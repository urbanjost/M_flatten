# M_flatten

The **M_flatten** module provides the procedure **flatten(3)** which
provides a function that returns pointer which is a rank one array that
points to a scalar or an array of any shape.

## Calling FLATTEN(3) in the call to the user procedure

To avoid using pointers directly write the called routine to expect a
flattened array and pass the arguments with varying rank in a call to 
FLATTEN(3).

Note that the called procedure WANTED(3) will not know the original rank
or shape unless it is passed, but will know the size of the input array.

```fortran
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
program demo_M_flatten

```
### Result
```text
 a=           1 b0=           1
 a=           4 b1=           2           3           4
 a=           8 b2=           5           6           7           8
 a=          12 b3=           9          10          11          12
```

## Calling FLATTEN(3) in the user procedure

A minor use of pointers is required in this alternate use of
FLATTEN(3f) but the called routine can query the original input
argument with RANK(3) and SHAPE(3) not just SIZE(3).

```fortran
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
! This technique is known as pointer rank remapping (introduced in
! Fortran 2003 and expanded in Fortran 2008). See below for details
use M_flatten, only : flatten
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
```
## Result

```text
 a=           1 b0=           1
 a=           4 b1=           2           3           4
 a=           8 b2=           5           6           7           8
 a=          12 b3=           9          10          11          12
```
### A single-dimension array pointer can point to a multi-dimensional array?

Yes, a single-dimension (rank-1) array pointer can point to a
multi-dimensional array in Fortran, provided that the multi-dimensional
target array is simply contiguous.

This technique is known as pointer rank remapping (introduced in Fortran
2003 and expanded in Fortran 2008).

#### Rules and Requirements

  **Explicit Bounds:** You must specify the explicit upper and lower
  bounds on the left-hand side of the pointer assignment. You cannot
  use a deferred colon (:).

  **Contiguity:** The multi-dimensional target array must occupy
  a continuous block of memory. Regular allocated or static arrays
  are automatically contiguous, but array slices with strides (e.g.,
  matrix(::2, :)) are not.

  **Array Size:** The size of the pointer must exactly match or
  be less than the total number of elements in the target array.
  Target Attribute: The multi-dimensional array must be declared with
  the target attribute.

## Do you really require something as general as FLATTEN(3)?

### Elemental procedures
If you wish to make a procedure that can be called with a specific 
argument having different ranks you can use elemental procedures if
all the arrays passed are of the same size and all the scalar arguments
are INTENT(IN). 

Since the scalars are not able to be changed this allows for pure procedures
to be called in parallel where each call to the scalar routine can be made
in any order.

## Generic interface
One could also write separate subroutines for each rank, and then make a
_generic interface_ for all of them. If there are only a few combinations
of arguments with varying rank it has the advantage that there are not
restrictions on the arrays all having the same rank or scalars being
only INTENT(IN) and unsupported ranks are detected at compile time rather
than run time. If there are multiple values that can be of different ranks
independent of each other the number of permutations and thus the number
of interface routines can get excessive. A preprocessor might help generate
a large number of interfaces but FLATTEN(3) provides a simple alternative
for such cases.

### Assumed rank and SELECT RANK
Another approach is to define a dummy argument to be assumed rank.
The programmer must then account for each rank manually.  It is possible
to catch the assumed size case and treat it specially; in the following
code it just writes a message saying it is unsupported.

```fortran
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
```
## Alternative 1: Allowing argument mismatch and using assumed-size arrays (Fortran 77 Style) 

Modern compilers frequently treat rank mismatches as a fatal error
by default, which frequently impacts legacy Fortran 77 code. It was
extremely common for pre-f90 compilers to allow argument rank mismatch
and the extension was heavily used in many codes.

So to allow a rank mismatch between an actual argument and a dummy
argument in Fortran, you can generally apply a compiler-specific flag to
bypass the error during compilation or (with most compilers) build the
routine with no explicit interface in a separate file from the calls to
the procedure.
  
Explicit interfaces prevent a number of common errors and are a strongly
desirable feature, so allowing rank mismatches is not recommended in
general, but if you are compiling older code and cannot modify the source
to use a modern method, here are examples of appropriate flags to your
compiler to downgrade the error to a warning:

    gfortran (GCC 10+): Add the flag -fallow-argument-mismatch. You
    can also use -std=legacy, though it may introduce other unintended
    behaviors.

    Intel Fortran (ifort / ifx): Add the flag -warn noargument-mismatch
    or use directives like !DIR$ ATTRIBUTES NO_ARG_CHECK specifically
    before the dummy argument declaration to bypass checking on an
    individual routine.

Now if you want to pass a multi-dimensional array or a specific element of
an array to a flat 1D sequence, you can use an _assumed-size array_.

 * Declare the final dimension of the dummy argument with an asterisk (\*).
 * Pass the first element of the array or array section explicitly
   (known as _sequence association_).

```fortran
program arbitrary
implicit none
integer :: a
integer :: b0, b1(-1:1), b2(2,2), b3(2,2,1)
external wanted

   a=0
   call wanted ( a, b0 ,1)
   print *, 'a=', a, 'b0=', b0
   call wanted ( a, b1 ,size(b1))
   print *, 'a=', a, 'b0=', b1
   call wanted ( a, b2 ,size(b2))
   print *, 'a=', a, 'b0=', b2
   call wanted ( a, b3 ,size(b3))
   print *, 'a=', a, 'b0=', b3
contains

end program arbitrary
```
```fortran
subroutine wanted( a, b, n )
integer, intent(inout) :: a
integer, intent(out)   :: b(*)
integer, intent(in)    :: n
integer                :: i
   do i=1,n
      a = a + 1
      b(i) = a
   enddo
end subroutine wanted
```
```bash
$ gfortran arbitrary.f90 -fallow-argument-mismatch -o arbitrary
arbitrary.f90:8:20:

    8 |    call wanted ( a, b0 ,1)
      |                    1
Warning: Rank mismatch in argument ‘b’ at (1) (rank-1 and scalar)

./arbitrary
 a=           1 b0=           1
 a=           4 b1=           2           3           4
 a=           8 b2=           5           6           7           8
 a=          12 b3=           9          10          11          12
```

## Alternative 2: Experimental method proposed for F202Y

Using GNU Fortran (GCC) 16.0.0 20250727 (experimental) and a compiler
switch to allow using proposed features lets you try a proposed 
feature now that simplifies using assumed rank targets, eliminating
the SELECT RANK requirement.

By definition it is not certain this is how it will work in the next
Fortran standard release.

If you have a recent gfortran release compile the following program
with
```bash
gfortran -std=f202y  point.f90
```
```fortran
program proposed
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
! NOTE: This technique uses pointer rank remapping (introduced in
! Fortran 2003 and expanded in Fortran 2008), which requires the 
! multi-dimensional target array be simply contiguous.
integer, intent(inout)                  :: a
integer,target, contiguous, intent(out) :: b(..)
integer                                 :: n
integer,pointer                         :: p_b(:)
   ! NOTE: The assumed rank target is an experimental F202y feature. 
   p_b(1:n)=>b  ! will this be allowed outside of SELECT RANK 
                ! and work with a scalar?
   ! NOTE: Explicit bounds are required. That is, you must 
   ! specify the explicit upper and lower bounds
   ! on the left-hand side of the pointer assignment.
   do n=1,size(b)
      a = a + 1
      p_b(n) = a
   enddo
end subroutine wanted

end program proposed
```
## Summary

   The **FLATTEN(3)** procedure generically allows for multi-dimensional
   arrays to be accessed as flattened arrays efficiently without having
   to copy the data to and from other shapes.

   Consider other methods carefully to decide on whether an alternative to
   **FLATTEN(3C)** is more appropriate:

    + elemental procedures
    + use of intrinsics such as **RESHAPE(3), PACK(3), UNPACK(3)**.
    + (legacy) sequence association 
    + (proposed) pointer assignment to
---

    Allowing this was a de-facto standard behavior but never part of the standard
    so you generally can do this with a compiler option to allow legacy behavior.

    A little more standard way is to use sequence association. It is generally
    acceptable to do this with legacy code but is probably frowned upon when
    writing new code.

    You can also create a flattened copy of the arrays and pass the temporary
    and then store it back into the original, which can be lot of overhead.

    For most cases you do not have to do that many dimensions, so I would
    generally recommend the standard methods, as most (certainly not all
    though) codes use four dimensions or less, or do just a few multi-dimensional
    sizes so in practice they are reasonable methods.

    That being said ...

    You can use sequence association. Place the "wanted" procedure in a 
    seperate file without an interface, like you often would with F77 code.

    Declare the dummy argument dimension with an asterisk (*).

    Pass the number of elements as a new argument.

    Pass the first element of the array or array section explicitly (known as sequence association). 

    You might have to add a compiler-specific option depending on how much the
    compiler wants to prevent you from using sequence association.
