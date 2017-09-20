program schroedinger
  
  use formatting
  use io
  use calculations
  implicit none

  integer :: npoints, intpoints, ii
  integer, allocatable :: valrange(:)
  real(dp) :: xmin, xmax, mass
  real(dp), allocatable :: potvec(:), DD(:), base(:,:), eigvec(:,:)
  character(len=7) :: inttype

  !> using module reading
  call reading(xmin, xmax, npoints, base, inttype, mass, valrange)

  !> using either linear interpolation, or polynomial interpolation
  select case (inttype)
    case("linear")
      call interpolationlin(npoints, xmin, xmax, base, potvec)
    case("polynom")
      call interpolationpol(npoints, xmin, xmax, base, potvec)
  end select

  !> calculation of eigenvalue, using the interpolatet potential
  call eigenvalue(npoints, xmin, xmax, potvec, mass, DD, eigvec)

  !> writing the resultes of the potential in an output-file
  call writingpot(potvec, xmin, xmax, npoints)

  !> writing results of eigenvalues, wavefunctions and energies in output-files
  call writingew(npoints, xmin, xmax, DD, eigvec, valrange)

  deallocate(base)
end program schroedinger
