program schroedinger
  
  use formatting
  use io
  use calculations
  implicit none

  integer :: npoints, intpoints, ii
  real(dp) :: xmin, xmax, mass
  real(dp), allocatable :: potvec(:), DD(:), base(:,:), eigvec(:,:)
  character(len=7) :: inttype

  call reading(xmin, xmax, npoints, base, inttype, mass)

  select case (inttype)
    case("linear")
      call interpolationlin(npoints, xmin, xmax, base, potvec)
    case("polynom")
      call interpolationpol(npoints, xmin, xmax, base, potvec)
  end select

  call eigenvalue(npoints, xmin, xmax, potvec, mass, DD, eigvec)

  call writingpot(potvec, xmin, xmax, npoints)

  call writingew(npoints, xmin, xmax, DD, eigvec)

  deallocate(base)
end program schroedinger
