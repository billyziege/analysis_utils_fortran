! Tests the interpolation of data on a line
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! Main Program
!	- Tests the interpolation with an easy function.
!-------------------------------------------------------------------------------
program main

  use interp
  use myarray

  !Declare the variables
  implicit none
  type(my_array) :: x1, xrel, yrel, y1
  real*8 :: xmin, xmax
  logical :: debug
  integer :: stat

  call init_my_array(6,x1)
  call init_my_array(3,xrel)
  call scaffold_my_array(xrel,yrel)

  xmin=1.
  xmax=2.

  call lin_range_my_array(xrel,xmin,xmax,stat)
  call lin_range_my_array(yrel,2*xmin,2*xmax,stat)
  x1%contents(1) = 0
  x1%contents(2) = 1
  x1%contents(3) = 1.5
  x1%contents(4) = 2.6
  x1%contents(5) = 2.9
  x1%contents(6) = 4

  call interpolate(x1,xrel,yrel,y1)
  write(*,*) "Should be 0:"
  write(*,*) y1%contents(1)
  write(*,*) "Should be 2:"
  write(*,*) y1%contents(2)
  write(*,*) "Should be 3:"
  write(*,*) y1%contents(3)
  write(*,*) "Should be 5.2:"
  write(*,*) y1%contents(4)
  write(*,*) "Should be 5.8:"
  write(*,*) y1%contents(5)
  write(*,*) "Should be 8:"
  write(*,*) y1%contents(6)

  call free_my_array(x1)
  call free_my_array(xrel)
  call free_my_array(yrel)
  call free_my_array(y1)

end program main
