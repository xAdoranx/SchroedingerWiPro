program schroedinger
  
  use formatting
  use io
  use calculations
  implicit none

  integer :: npoints, intpoints, ii
  real(dp) :: xmin, xmax
  real(dp), allocatable :: potvec(:), base(:,:)
  character :: inttype

  call reading(xmin, xmax, npoints, base, inttype)

  call interpolationlin(npoints, xmin, xmax, base, potvec)

  call writing(potvec)  

  deallocate(base)
end program schroedinger
