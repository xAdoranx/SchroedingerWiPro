program schroedinger
  
  use formatting
  use io
  use calculations
  implicit none

  integer :: npoints, intpoints, ii
  real(dp) :: xmin, xmax, mass
  real(dp), allocatable :: potvec(:), DD(:), base(:,:), eigvec(:,:)
  character :: inttype

  call reading(xmin, xmax, npoints, base, inttype, mass)

  call interpolationlin(npoints, xmin, xmax, base, potvec)

  call eigenvalue(npoints, xmin, xmax, potvec, mass, DD, eigvec)

  call writingpot(potvec)

  call writingew(npoints, xmin, xmax, DD, eigvec)

  deallocate(base)
end program schroedinger
