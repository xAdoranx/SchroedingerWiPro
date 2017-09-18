program schroedinger
  
  use formatting
  use io
  use calculations
  implicit none

  integer :: npoints, intpoints, ii
  real(dp) :: xmin, xmax, mass
  real(dp), allocatable :: potvec(:), DD(:), base(:,:)
  character :: inttype

  call reading(xmin, xmax, npoints, base, inttype, mass)

  call interpolationlin(npoints, xmin, xmax, base, potvec)

  call eigenvalue(npoints, xmin, xmax, potvec, mass, DD)

  call writing(potvec)  

  deallocate(base)
end program schroedinger
