module M_flatten
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
use,intrinsic :: iso_fortran_env, only : real32, real64
implicit none
private
public :: flatten
interface flatten
   module procedure flatten_int32
   module procedure flatten_int64
   module procedure flatten_real32
   module procedure flatten_real64
end interface flatten
contains

function flatten_int32(arr) result(p_arr)
use, intrinsic :: iso_c_binding
integer(kind=int32),target, contiguous :: arr(..)
integer(kind=int32),pointer            :: p_arr(:)
integer                                :: n
   n=size(arr)
   ! Note Explicit Bounds must be specified for the upper and lower bounds
   ! on the left-hand side of the pointer assignment.
   select rank (arr)                                      
   rank (0);     ! p_arr(1:1)=>arr  ! not allowed up to f2023 at least
      ! Map the scalar's memory address to a 1D array pointer of size 1
      call c_f_pointer(c_loc(arr), p_arr, [1])
      ! unsafe in general, but I think is OK since using a SELECT RANK to ensure a scalar
   rank (1);     p_arr(1:n)=>arr                            
   rank (2);     p_arr(1:n)=>arr                            
   rank (3);     p_arr(1:n)=>arr                            
   rank (4);     p_arr(1:n)=>arr                            
   rank (5);     p_arr(1:n)=>arr                            
   rank (6);     p_arr(1:n)=>arr                            
   rank (7);     p_arr(1:n)=>arr                            
   rank (8);     p_arr(1:n)=>arr                            
   rank (9);     p_arr(1:n)=>arr                            
   rank (10);    p_arr(1:n)=>arr                            
   rank (11);    p_arr(1:n)=>arr                            
   rank (12);    p_arr(1:n)=>arr                            
   rank (13);    p_arr(1:n)=>arr                            
   rank (14);    p_arr(1:n)=>arr                            
   rank (15);    p_arr(1:n)=>arr                            
   rank (*)                                                 
   print *, 'assumed size is unsupported'  
   rank default                                             
   print *, 'unsupported rank'                    
   end select                                              
end function flatten_int32

function flatten_int64(arr) result(p_arr)
use, intrinsic :: iso_c_binding
integer(kind=int64),target, contiguous :: arr(..)
integer(kind=int64),pointer            :: p_arr(:)
integer                                :: n
   n=size(arr)
   ! Note Explicit Bounds must be specified for the upper and lower bounds
   ! on the left-hand side of the pointer assignment.
   select rank (arr)                                      
   rank (0);     ! p_arr(1:1)=>arr  ! not allowed up to f2023 at least
      ! Map the scalar's memory address to a 1D array pointer of size 1
      call c_f_pointer(c_loc(arr), p_arr, [1])
      ! unsafe in general, but I think is OK since using a SELECT RANK to ensure a scalar
   rank (1);     p_arr(1:n)=>arr                            
   rank (2);     p_arr(1:n)=>arr                            
   rank (3);     p_arr(1:n)=>arr                            
   rank (4);     p_arr(1:n)=>arr                            
   rank (5);     p_arr(1:n)=>arr                            
   rank (6);     p_arr(1:n)=>arr                            
   rank (7);     p_arr(1:n)=>arr                            
   rank (8);     p_arr(1:n)=>arr                            
   rank (9);     p_arr(1:n)=>arr                            
   rank (10);    p_arr(1:n)=>arr                            
   rank (11);    p_arr(1:n)=>arr                            
   rank (12);    p_arr(1:n)=>arr                            
   rank (13);    p_arr(1:n)=>arr                            
   rank (14);    p_arr(1:n)=>arr                            
   rank (15);    p_arr(1:n)=>arr                            
   rank (*)                                                 
   print *, 'assumed size is unsupported'  
   rank default                                             
   print *, 'unsupported rank'                    
   end select                                              
end function flatten_int64

function flatten_real32(arr) result(p_arr)
use, intrinsic :: iso_c_binding
real(kind=real32),target, contiguous :: arr(..)
real(kind=real32),pointer            :: p_arr(:)
integer                              :: n
   n=size(arr)
   ! Note Explicit Bounds must be specified for the upper and lower bounds
   ! on the left-hand side of the pointer assignment.
   select rank (arr)                                      
   rank (0);     ! p_arr(1:1)=>arr  ! not allowed up to f2023 at least
      ! Map the scalar's memory address to a 1D array pointer of size 1
      call c_f_pointer(c_loc(arr), p_arr, [1])
      ! unsafe in general, but I think is OK since using a SELECT RANK to ensure a scalar
   rank (1);     p_arr(1:n)=>arr                            
   rank (2);     p_arr(1:n)=>arr                            
   rank (3);     p_arr(1:n)=>arr                            
   rank (4);     p_arr(1:n)=>arr                            
   rank (5);     p_arr(1:n)=>arr                            
   rank (6);     p_arr(1:n)=>arr                            
   rank (7);     p_arr(1:n)=>arr                            
   rank (8);     p_arr(1:n)=>arr                            
   rank (9);     p_arr(1:n)=>arr                            
   rank (10);    p_arr(1:n)=>arr                            
   rank (11);    p_arr(1:n)=>arr                            
   rank (12);    p_arr(1:n)=>arr                            
   rank (13);    p_arr(1:n)=>arr                            
   rank (14);    p_arr(1:n)=>arr                            
   rank (15);    p_arr(1:n)=>arr                            
   rank (*)                                                 
   print *, 'assumed size is unsupported'  
   rank default                                             
   print *, 'unsupported rank'                    
   end select                                              
end function flatten_real32

function flatten_real64(arr) result(p_arr)
use, intrinsic :: iso_c_binding
real(kind=real64),target, contiguous :: arr(..)
real(kind=real64),pointer            :: p_arr(:)
integer                              :: n
   n=size(arr)
   ! Note Explicit Bounds must be specified for the upper and lower bounds
   ! on the left-hand side of the pointer assignment.
   select rank (arr)                                      
   rank (0);     ! p_arr(1:1)=>arr  ! not allowed up to f2023 at least
      ! Map the scalar's memory address to a 1D array pointer of size 1
      call c_f_pointer(c_loc(arr), p_arr, [1])
      ! unsafe in general, but I think is OK since using a SELECT RANK to ensure a scalar
   rank (1);     p_arr(1:n)=>arr                            
   rank (2);     p_arr(1:n)=>arr                            
   rank (3);     p_arr(1:n)=>arr                            
   rank (4);     p_arr(1:n)=>arr                            
   rank (5);     p_arr(1:n)=>arr                            
   rank (6);     p_arr(1:n)=>arr                            
   rank (7);     p_arr(1:n)=>arr                            
   rank (8);     p_arr(1:n)=>arr                            
   rank (9);     p_arr(1:n)=>arr                            
   rank (10);    p_arr(1:n)=>arr                            
   rank (11);    p_arr(1:n)=>arr                            
   rank (12);    p_arr(1:n)=>arr                            
   rank (13);    p_arr(1:n)=>arr                            
   rank (14);    p_arr(1:n)=>arr                            
   rank (15);    p_arr(1:n)=>arr                            
   rank (*)                                                 
   print *, 'assumed size is unsupported'  
   rank default                                             
   print *, 'unsupported rank'                    
   end select                                              
end function flatten_real64

end module M_flatten
