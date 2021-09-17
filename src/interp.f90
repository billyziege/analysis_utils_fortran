module interp
!-------------------------------------------------------------------------------
! Module interp:
! A module that applies interpolation algorithms
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!subroutine interpolate:
!  - Given two sets of data, interpolates (or extrapolates) the y value
!    of the provided x-data given the x and y related values.
! input:
!	- x1   = The x-data for which we would like to have y values.
!	- xrel = The x-data for which we have y-vales.
!   - yrel =  The related y-data for xrel.
! output:
!	- y1   = The interpolated y-values (assuming linear interpolation or
!            extrapolation)
!Need to notate run_evelope
use myarray
implicit none

contains

subroutine interpolate(x1,xrel,yrel,y1)
  type(my_array), intent(in) :: x1,xrel,yrel !x1 and xrel need to be sorted lowest to highest.
  type(my_array), intent(inout) :: y1
  integer :: i,j
  real*8:: slope

  i = 1
  j = 1
  write(*,*) "Interpolation begin:"
  do while ( (x1%a(i) <= xrel%a(1)))
    !extrapolate
    !write(*,*) "Extrap"
    !write(*,*) x1%a(i)
    !write(*,*) xrel%a(1)
    !write(*,*) xrel%a(2)

    y1%a(i) = (yrel%a(1)-yrel%a(2))*(x1%a(i)-xrel%a(1))/(xrel%a(1)-xrel%a(2))&
                      + yrel%a(1)
    i = i + 1
    if (i > y1%N) then
      return
    end if
  end do
  j = 2
  do while ( j < xrel%N) 
    if (x1%a(i) > xrel%a(j)) then
      j = j + 1
      !write(*,*) xrel%a(j)
    else 
      !interpolate
      !write(*,*) "Interp"
      !write(*,*) x1%a(i)
      !write(*,*) xrel%a(j)
      !write(*,*) xrel%a(j-1)
      y1%a(i) = (yrel%a(j-1)-yrel%a(j))&
                        *(x1%a(i)-xrel%a(j))/(xrel%a(j-1)-xrel%a(j))&
                        + yrel%a(j)
      i = i + 1
      if (i > y1%N) then
        return
      end if
    end if
  end do
  j = xrel%N
  !write(*,*) xrel%a(j)
  do while ( i <= y1%N )
    !extrapolate
    !write(*,*) "Extrap"
    !write(*,*) x1%a(i)
    !write(*,*) xrel%a(j)
    !write(*,*) xrel%a(j-1)
    y1%a(i) = (yrel%a(j)-yrel%a(j-1))&
                       *(x1%a(i)-xrel%a(j))/(xrel%a(j)-xrel%a(j-1))&
                       + yrel%a(j)
    i = i + 1
  end do
end subroutine

end module interp
