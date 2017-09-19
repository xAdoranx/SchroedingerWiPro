program schroedinger
  
  use formatting
  use io
  use calculations
  implicit none

  integer :: npoints, intpoints, ii
  real(dp) :: xmin, xmax
  real(dp), allocatable :: potvec(:), base(:,:)
  character(len=7) :: inttype

  call reading(xmin, xmax, npoints, base, inttype)
  select case (inttype)
    case("linear")
      call interpolationlin(npoints, xmin, xmax, base, potvec)
    case("polynom")
      call interpolationpol(npoints, xmin, xmax, base, potvec)
    end select
    
  
  call writing(potvec,xmin,xmax,npoints)  

  deallocate(base)
end program schroedinger
