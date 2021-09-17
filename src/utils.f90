module utils
!-------------------------------------------------------------------------------
! Module interp:
! A module that applies interpolation algorithms
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!real*8 function cmp_function:
!  - Returns the first element less the second.
! input:
!	- element1   = The value of the first element.
!	- element2   = The value of the second element.
!-------------------------------------------------------------------------------

implicit none

contains

subroutine file_len(filename,n_lines)
  character(*), intent(in) :: filename
  integer, intent(out) :: n_lines
  integer :: ierr
  character(300) :: buffer
  n_lines = 0
  ierr = 0

  open(unit = 101,file = filename)
  ! ierr will be zero unless an error reading the file happens
  do while(ierr == 0)
    read(101,*,iostat=ierr) buffer
    if(ierr == 0) then
      n_lines = n_lines + 1
    end if
  end do
  close(101)
end subroutine

real*8 function cmp_function(element1, element2)
  real*8, intent (in) :: element1, element2
  cmp_function = element1 - element2
end function

character(30) function real_as_str(x)
  real*8, intent(in) :: x
  character(30) :: myformat
  character(30) :: x_str
  myformat = "(e12.4)"
  write(real_as_str,myformat) x
end function

subroutine extract_value_from_filename(mod_filename,param_substr,param)
  character(*), intent(in) :: mod_filename
  character(*), intent(in) :: param_substr
  real*8, intent(out) :: param
  integer :: begin_i, last_i, substr_len
  substr_len = len_trim(param_substr)
  begin_i = index(mod_filename,"_"//trim(param_substr))
  last_i = scan(mod_filename(begin_i+substr_len+2:),"_")
  if (last_i == 0) then
    last_i = scan(mod_filename(begin_i+substr_len+2:),'csv') -1
  end if
  read(mod_filename(begin_i+substr_len+2:begin_i + substr_len + last_i ),*) param
end subroutine    

end module utils
