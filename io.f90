!> Contains INput/Output related routines
module io

  use formatting
  implicit none

contains

  !> Reads in the data for solving the schroedinger equation
  subroutine reading(xmin, xmax, npoints, base, inttype, mass, valrange)
    !> Input number of x values
    !!
    !! must be an integer
    integer, intent(out) :: npoints

    !> Input range of eigenvalues
    !!
    !! Range of eigenvalues must be a vector with two values
    integer, allocatable, intent(out) :: valrange(:)

    !> Input minimum and maximum x values and mass of particle
    !!
    !! xmin, xmax and mass must be a real number
    real(dp), intent(out) :: xmin, xmax, mass

    !> Input bases for potential
    !!
    !! base must be an array with two rows
    real(dp), allocatable, intent(out) :: base(:,:)

    !> Input character for chosing interpolation type
    !!
    !! If inttype is "linear" linear interpolation is used, if inttype is "polynom" newtonian
    !! polynomial interpolation ist used 
    character(len=7), intent(out) :: inttype
    integer :: intpoints

    !> Read data from file schrodinger.inp
    open(11, file="schrodinger.inp", status="old", form="formatted", action="read")

    read(11,*) mass
    read(11,*) xmin, xmax, npoints
    
    allocate(valrange(2))
    read(11,*) valrange
    
    read(11,*) inttype
    read(11,*) intpoints
    allocate(base(2,intpoints))
    read(11,*) base
  end subroutine reading
  
  !>  Calculates x-values and writes potential interpolation in data discrpot and 
  subroutine writingpot(potvec, xmin, xmax, npoints)

    !> Input y-values of potential
    !!
    !! must be a vector of the same length as the x-datas
    real(dp), intent(in) :: potvec(:)

    !> Input minimum and maximum x values
    !!
    !! xmin and xmax must be a real number 
    real(dp), intent(in) :: xmin, xmax

    !> Input number of x values
    !!
    !! must be an integer
    integer, intent(in) :: npoints

    !> Output of xdatas and y-values of the potential
    real(dp), allocatable :: xdata(:), output(:,:)
    real(dp) :: deltax
    integer :: ii

    allocate(xdata(npoints+1))
    deltax = (xmax-xmin)/npoints
    do ii = 1 , npoints+1
      xdata(ii) = ((ii-1)*deltax+xmin)
    end do

    allocate(output(2,npoints+1))
    
    output(1,:) = xdata
    output(2,:) = potvec

    !> Writes potential in file discrpot.dat
    open(21, file="discrpot.dat", status="replace", form="formatted", action="write")
    

    write(21,"(2F8.2)") output
  
  end subroutine writingpot
  !>  Calculates x-values and writes values for eigenvalue, engeries and wavefunctions in output-files
  subroutine writingew(npoints, xmin, xmax, DD, eigvec, valrange)

    !> Input eigenvalues(DD) and eigenvectors(eigvec)
    !!
    !! Eigenvalues must be a vector and eigenvectors must be an array
    real(dp), intent(in) :: DD(:), eigvec(:,:)

    !> Input minimum and maximum x values
    !!
    !! xmin and xmax must be a real number
    real(dp), intent(in) :: xmin, xmax

    !> Input number of x values
    !!
    !! must be an integer
    integer, intent(in) :: npoints

    !> Input range of eigenvalues
    !!
    !! Range of eigenvalues must be a vector with two values
    integer, intent(in) :: valrange(:)
    real(dp) :: deltax
    real(dp), allocatable :: xval(:), output(:,:)
    integer :: ii, range

    !> Writes the energies in file energies.dat, the wavefunctions in file wfuncs.dat and the
    !eigenvalues in file ewfuncs.dat
    open(22, file="energies.dat", status="replace", form="formatted", action="write")
    open(23, file="wfuncs.dat", status="replace", form="formatted", action="write")
    open(24, file="ewfuncs.dat", status="replace", form="formatted", action="write")

    write(22,"(F12.3)") DD

    deltax = (xmax - xmin) / npoints
    allocate(xval(npoints+1))
    xval(1) = xmin
    do ii=2,npoints
      xval(ii) = xval(ii-1) + deltax
    end do

    range = valrange(2) - valrange(1)
    allocate(output(npoints+1,range+1))

    output(:,1) = xval
    output(:,2:range+1) = eigvec(:,valrange(1):valrange(2))
    !output = transpose(output)

    do ii=1,size(output(:,1),1)
      write(23,"(10000000000000F8.4)") output(ii,:)
    end do
    

    do ii=1,range
      output(:,ii+1) = output(:,ii+1) + DD(valrange(1)+ii-1)
    end do
    
    !write(*,*) output
    do ii=1,size(output(:,1),1)
      write(24,"(10000000000000F8.4)") output(ii,:)
    end do
    
    
  end subroutine writingew
    
end module io
