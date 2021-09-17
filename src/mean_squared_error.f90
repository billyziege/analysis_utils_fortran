module mean_squared_error
!-------------------------------------------------------------------------------
! Module mean_squared_error:
! A module to provide an interface to compare models and experimental data.
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!subroutine calc_scaled_mean_squared_error:
!  - Given a set of model and experimental data, finds the scaled mean
!    squared error.
! input:
!	- x_meas   = The measured x-data.
!	- y_meas   = The measured y-data.
!	- y_err    = The measured y-data error.
!	- x_mod    = The modelled x-data.
!	- y_mod    = The modelled y-data.
!	- y_exp    = Container to hold the expected y-data from interpolation.
! output:
!	- smse     = The scaled mean squared error.
!-------------------------------------------------------------------------------
use myarray
use interp
implicit none

contains

subroutine calc_scaled_mean_squared_error(x_meas,y_meas,y_err,x_mod,y_mod,y_exp,smse)
  type(my_array), intent(in) :: x_meas,y_meas,y_err,x_mod,y_mod
  type(my_array), intent(inout) :: y_exp 
  real*8, intent(out) :: smse
  character(30) :: smse_str
  integer :: i
  
  smse = 0
  call interpolate(x_meas,x_mod,y_mod,y_exp)
  do i = 1, y_meas%N
    smse = smse + (y_meas%a(i) - y_exp%a(i))**2/y_err%a(i)**2
  end do
  smse = smse/y_meas%N
end subroutine

end module mean_squared_error
