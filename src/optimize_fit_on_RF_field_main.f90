! RMS envelope integration code.
! Integration is conducted by the leap frog alogorithm
! as written by Brandon Zerbe.
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! Main Program
!	- Evolves the provided statistics according to the RMS envelope equation
!     with assumed cylindrically symmetry
!-------------------------------------------------------------------------------
program main

  use utils
  use io
  use myarray
  use mean_squared_error
  use f90argparser

  !Declare the variables
  implicit none
  type(my_array) :: sx, sz, mux, muz, muz0
  type(my_array) :: RF_Efield, deltaE_exp, deltaE_err
  type(my_array) :: rev_deltaE_exp, rev_deltaE_err
  type(my_array) :: scaled_Efield, deltaE_mod, deltaE_matched
  type(my_array) :: Cz, Cz0
  type(my_array) :: a, b
  real*8 :: a_best, b_best
  real*8 :: emx, emz
  real*8 :: sx0, sz0
  character(300) :: exp_filename, models_filename, mod_filename, min_str_front
  character(256) :: sx_str, sz_str, mux_str, muz_str, muz0_str
  character(256) :: emx_str, emz_str, sx0_str, sz0_str
  character(256) :: a_best_str, b_best_str, smse_min_str
  character(1) :: delimiter
  integer :: actual_Nchirp, Nrow, Ncol, Nrow_mod, N_a, N_b
  real*8 :: smse, smse_min
  real*8 :: a_uncert, a_min, a_max, b_min, b_max
  real*8 :: E_over_sqrtP_RF, L_RF, f_RF
  real*8 :: q, m, gam, c, betaz, d
  real*8, dimension(:,:), allocatable :: exp_data, mod_data
  integer :: i, j, stat, ierr
  type(opt_container):: opt_c
  logical :: debug

  !Define what to expect as input and provide the user help.

  call opt_container_init(opt_c,16)
  opt_c%description = 'Fits experimental data to mutliple envelope models with various RF E field parameters.'
  call add_option( opt_c, &
                   'The experimental energy spread data file with columns of Efield,DeltaE,DeltaE_err', &
                   has_arg = .true. )
  call add_option( opt_c, &
                   'The file containing a list of envelope models files each with 5 columns: '&
                   //'mux0,sx,sz,mux,muz with the last 4 taken at d.', &
                   has_arg = .true. )
  call add_option( opt_c, 'The number of chirps explored in the envelope model files.', long = 'Nchirps', &
                   has_arg = .true., val = '400')
  call add_option( opt_c, 'Slope between the sqrt of the power and the electric field.', &
                   long = 'E_slope', has_arg = .true., val = '8.4e4')
  call add_option( opt_c, 'The uncertainty in the RF E field slope measurement as a fraction of the measurement (0.1 = 10%).', &
                   long = 'a_uncert', has_arg = .true., val = '0.1')
  call add_option( opt_c, 'The number of values for the RF E field slope.', long = 'N_a', &
                   has_arg = .true., val = '20')
  call add_option( opt_c, 'Length of the RF cavity in m.', &
                   long = 'L_RF', has_arg = .true., val = '0.0225')
  call add_option( opt_c, 'Frequency of the RF cavity in Hz.', &
                   long = 'f_RF', has_arg = .true., val = '1.0135e9')
  call add_option( opt_c, 'The minimum intercept for the RF E field.', long = 'b_min', &
                   has_arg = .true., val = '0')
  call add_option( opt_c, 'The maximum intercept for the RF E field.', long = 'b_max', &
                   has_arg = .true., val = '3e8')
  call add_option( opt_c, 'The number of intercept values for the RF E field.', long = 'N_b', &
                   has_arg = .true., val = '100')
  call add_option( opt_c, 'The charge, in Coulombs, of each particle.', long = 'charge', short='q', &
                   has_arg = .true., val = '1.602176634e-19')
  call add_option( opt_c, 'The mass, in kg, of each particle.', long = 'mass', short='m', &
                   has_arg = .true., val = '9.10938356e-31')
  call add_option( opt_c, 'The distance between the RF cavity and the sample.', short='d', &
                   has_arg = .true., val = '0.35')
  call add_option( opt_c, 'The Lorentz factor of the beam.', long = 'gamma', short='g', &
                   has_arg = .true., val = '1.04892367906067')
  call add_option( opt_c, 'Flag that turns on debugging.', &
                   long='debug', has_arg = .false. )

  !Fill the options container from the command line
  call process_cli( opt_c )
  
  !Store the command line and/or default values to the appropriate variables.
  
  if (debug) then
    write(*,*) "Options added."
  end if
  exp_filename = opt_c%opts(1)%val
  models_filename = opt_c%opts(2)%val
  read(opt_c%opts(3)%val,*) Nrow_mod
  read(opt_c%opts(4)%val,*) E_over_sqrtP_RF
  read(opt_c%opts(5)%val,*) a_uncert
  read(opt_c%opts(6)%val,*) N_a
  read(opt_c%opts(7)%val,*) L_RF
  read(opt_c%opts(8)%val,*) f_RF
  read(opt_c%opts(9)%val,*) b_min
  read(opt_c%opts(10)%val,*) b_max
  read(opt_c%opts(11)%val,*) N_b
  read(opt_c%opts(12)%val,*) q
  read(opt_c%opts(13)%val,*) m
  read(opt_c%opts(14)%val,*) d
  read(opt_c%opts(15)%val,*) gam
  if (opt_c%opts(16)%val == "") then
    debug = .false.
  else
    debug = .true.
  end if

  if (debug) then
    write(*,*) "Through options"
  end if

  betaz = sqrt(1. - 1./gam**2)
  c = 299792458
  delimiter=","

  if (debug) then
    write(*,*) "Options assigned."
  end if
  ! The experimental data
  !Specify the amount of data
  Ncol = 3
  call file_len(exp_filename,Nrow)

  !Allocate containers
  allocate(exp_data(Nrow,Ncol))
  RF_Efield = new_my_array(Nrow)
  deltaE_exp = new_my_array(Nrow)
  deltaE_err = new_my_array(Nrow)
  deltaE_matched = new_my_array(Nrow)
  rev_deltaE_exp = new_my_array(Nrow)
  rev_deltaE_err = new_my_array(Nrow)
  scaled_Efield = new_my_array(Nrow)

  if (debug) then
    write(*,*) "Containers allocated."
  end if

  !Read data in and store
  call read_file_into_2d_array(exp_filename,Nrow,Ncol,exp_data)
  RF_Efield = table_column_into_my_array(exp_data,Nrow,Ncol,1)
  deltaE_exp = table_column_into_my_array(exp_data,Nrow,Ncol,2)
  deltaE_err = table_column_into_my_array(exp_data,Nrow,Ncol,3)
  !Convert to J.
  deltaE_exp%a(:) = deltaE_exp%a(:)*q
  deltaE_err%a(:) = deltaE_err%a(:)*q
  !Handle the reverse
  rev_deltaE_exp%a(Nrow:1:-1) = deltaE_exp%a(:)
  rev_deltaE_err%a(Nrow:1:-1) = deltaE_err%a(:)

  if (debug) then
    write(*,*) "Experimental data read."
  end if

  !The envelope model data
  !Specify the amount of data
  Ncol = 5
  Nrow = Nrow_mod

  !Allocate containers and fill if needed
  allocate(mod_data(Nrow,Ncol))
  muz0 = new_my_array(Nrow)
  Cz0 = new_my_array(Nrow)
  deltaE_mod = new_my_array(Nrow)
  sx = new_my_array(Nrow)
  sz = new_my_array(Nrow)
  mux = new_my_array(Nrow)
  muz = new_my_array(Nrow)
  Cz = new_my_array(Nrow)
  a = new_my_array(N_a)
  a_max = -2.0*q*E_over_sqrtP_RF*L_RF*3.14159*f_RF/(m*c**2*betaz**2*gam)*(1. - a_uncert)
  a_min = -2.0*q*E_over_sqrtP_RF*L_RF*3.14159*f_RF/(m*c**2*betaz**2*gam)*(1. + a_uncert)
  call lin_range_my_array(a,a_min,a_max,stat)
  b = new_my_array(N_b)
  call lin_range_my_array(b,b_min,b_max,stat)

  if (debug) then
    write(*,*) "Model array allocated."
    write(*,*) -2.0*q*E_over_sqrtP_RF*L_RF*3.14159*f_RF/(m*c**2*betaz**2*gam)
  end if

  !Loop through files and calculate smse
  open(unit = 51,file = models_filename)
  read(51,'(A)',iostat=ierr) mod_filename
  if (debug) then
    write(*,*) ierr
    write(*,*) trim(models_filename)
    write(*,*) trim(mod_filename)
  end if
  do while(ierr == 0)
    !Added to loop to handle partially missing data
    call file_len(mod_filename,actual_Nchirp)
    do while (actual_Nchirp /= Nrow)
      call extract_value_from_filename(mod_filename,'sx',sx0)    
      call extract_value_from_filename(mod_filename,'sz',sz0)    
      call extract_value_from_filename(mod_filename,'ex',emx)    
      call extract_value_from_filename(mod_filename,'ez',emz)    
      sx0_str = real_as_str(sx0)
      sz0_str = real_as_str(sz0)
      emx_str = real_as_str(emx)
      emz_str = real_as_str(emz)
      write(min_str_front,*) adjustl(trim(sx0_str)),trim(delimiter),adjustl(trim(sz0_str))
      write(min_str_front,*)adjustl(trim(min_str_front)),trim(delimiter),adjustl(trim(emx_str))
      write(min_str_front,*)adjustl(trim(min_str_front)),trim(delimiter),adjustl(trim(emz_str))
      write(*,*) adjustl(trim(min_str_front)),trim(delimiter),trim(delimiter),trim(delimiter)
      read(51,'(A)',iostat=ierr) mod_filename
      call file_len(mod_filename,actual_Nchirp)
    end do

    !Read in the model data and store.
    call read_file_into_2d_array(mod_filename,Nrow,Ncol,mod_data)
    if (debug) then
      write(*,*) "2d array allocated."
    end if
    muz0 = table_column_into_my_array(mod_data,Nrow,Ncol,1)
    sx = table_column_into_my_array(mod_data,Nrow,Ncol,2)
    sz = table_column_into_my_array(mod_data,Nrow,Ncol,3)
    mux = table_column_into_my_array(mod_data,Nrow,Ncol,4)
    muz = table_column_into_my_array(mod_data,Nrow,Ncol,5)
    if (debug) then
      write(*,*) "table to columns."
    end if
    call extract_value_from_filename(mod_filename,'sx',sx0)    
    call extract_value_from_filename(mod_filename,'sz',sz0)    
    call extract_value_from_filename(mod_filename,'ex',emx)    
    call extract_value_from_filename(mod_filename,'ez',emz)    
    if (debug) then
      write(*,*) "Model data read."
      write(*,*) muz0%a(1)
      write(*,*) muz0%a(2)
      write(*,*) sz0
      write(*,*) emz
    end if

    !Rescale to get chirps and energy
    Cz0%a(:) = muz0%a(:)/sz0
    if (debug) then
      write(*,*) "Cz0."
    end if
    Cz%a(:)  = muz%a(:)/sz%a(:)
    if (debug) then
      write(*,*) "Cz."
    end if
    !This is the interacting model --- I had noninteracting at first
    deltaE_mod%a(:) = gam*m*c**2*betaz*sqrt(emz**2/sz%a(:)**2 + muz%a(:)**2/c**2)   

    if (debug) then
      write(*,*) "Model data scaled."
      sx0_str = real_as_str(sx0)
      write(*,*) adjustl(trim(sx0_str))
    end if

    smse_min = -1
    do i = 1, a%N, 1
      do j = 1, b%N, 1
        !write(*,*) "Values:"
        if (a%a(i) > 0) then
          scaled_Efield%a(:) = a%a(i)*RF_Efield%a(:)+b%a(j)
          call calc_scaled_mean_squared_error(scaled_Efield,deltaE_exp,deltaE_err,Cz0,deltaE_mod,deltaE_matched,smse)
        else
          scaled_Efield%a(RF_Efield%N:1:-1) = a%a(i)*RF_Efield%a(:)+b%a(j)
          !write(*,*) "Hello"
          !write(*,*) Cz0%a(1)
          !write(*,*) scaled_Efield%a(1)
          !write(*,*) scaled_Efield%a(2)
          !write(*,*) scaled_Efield%a(5)
          !call calc_scaled_mean_squared_error(scaled_Efield,rev_deltaE_exp,rev_deltaE_err,Cz0,deltaE_mod,deltaE_matched,smse)
        end if
        !write(*,*) b%a(j)
        !write(*,*) Cz0%a(1)
        !write(*,*) Cz0%a(400)
        !write(*,*) scaled_Efield%a(3)/3e8
        !write(*,*) deltaE_exp%a(3)
        !write(*,*) deltaE_matched%a(3)
        !write(*,*) scaled_Efield%a(10)
        !write(*,*) smse
        !write(*,*) smse_min
        if ( (smse < smse_min) .OR. (smse_min == -1) ) then
          smse_min = smse
          a_best = a%a(i)
          b_best = b%a(j)
        end if
      end do
    end do

    if (debug) then
      write(*,*) "smse_min calculated."
      write(*,*) smse
      write(*,*) sx0
    end if


    !Convert number to strings so that they can be output.
    sx0_str = real_as_str(sx0)
    if (debug) then
      write(*,*) "sx0_str created"
    end if
    sz0_str = real_as_str(sz0)
    emx_str = real_as_str(emx)
    emz_str = real_as_str(emz)
    a_best_str = real_as_str(a_best)
    b_best_str = real_as_str(b_best)
    smse_min_str = real_as_str(smse_min)
    write(min_str_front,*) adjustl(trim(sx0_str))//adjustl(trim(delimiter))//adjustl(trim(sz0_str))
    write(min_str_front,*) adjustl(trim(min_str_front))//adjustl(trim(delimiter))//adjustl(trim(emx_str))
    write(min_str_front,*) adjustl(trim(min_str_front))//adjustl(trim(delimiter))//adjustl(trim(emz_str))
    write(min_str_front,*) adjustl(trim(min_str_front))//adjustl(trim(delimiter))//adjustl(trim(a_best_str))
    write(min_str_front,*) adjustl(trim(min_str_front))//adjustl(trim(delimiter))//adjustl(trim(b_best_str))
    write(*,*) adjustl(trim(min_str_front))//adjustl(trim(delimiter))//adjustl(trim(smse_min_str))
    if (debug) then
      write(*,*) mod_filename
      write(*,*) "File done."
    end if
    read(51,'(A)',iostat=ierr) mod_filename

  end do
  close(51)
 

  !Garbage collection
  call free_my_array(sx)
  call free_my_array(sz)
  call free_my_array(mux)
  call free_my_array(muz)
  call free_my_array(muz0)
  call free_my_array(RF_Efield)
  call free_my_array(deltaE_exp)
  call free_my_array(deltaE_err)
  call free_my_array(scaled_Efield)
  call free_my_array(deltaE_mod)
  call free_my_array(deltaE_matched)
  call free_my_array(Cz)
  call free_my_array(Cz0)
  deallocate(exp_data)
  deallocate(mod_data)

end program main
