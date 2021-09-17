module io
!-------------------------------------------------------------------------------
! Module interp:
! A module that contains subroutines for input/output.
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!subroutine file_len
!	- Counts the number of lines in a file.  Empty lines are skipped and do
!     not add to the total.
! input:
!	- filename = name of the file
! output:
!	- n_lines = number of lines in the file
!
!subroutine read_file_into_2d_array:
!  - Given a filepath, read the data into my_arrays.
! input:
!	- filename  =  Specifies file path with array data.
!	- Nrow      =  length of file/first axis of array
!   - Ncol      =  number of entries/second axis of array
! output:
!	- a2d       =  2D array containing content from filepath
!-------------------------------------------------------------------------------
implicit none

contains

subroutine read_file_into_2d_array(filename,Nrow,Ncol,a2d)
  character(300), intent(in) :: filename 
  integer, intent(in) :: Nrow, Ncol
  real*8, dimension(Nrow,Ncol), intent(out) :: a2d
  integer :: i,j
  open(unit=37,file=filename)

  do i = 1, Nrow, 1
    read(37,*) a2d(i,:)
  end do

  close(37)
end subroutine

end module io
