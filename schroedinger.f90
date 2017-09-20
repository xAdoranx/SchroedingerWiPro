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

  call reading(xmin, xmax, npoints, base, inttype, mass, valrange)

  select case (inttype)
    case("linear")
      call interpolationlin(npoints, xmin, xmax, base, potvec)
    case("polynom")
      call interpolationpol(npoints, xmin, xmax, base, potvec)
<<<<<<< HEAD
    end select
    
  
  call writing(potvec,xmin,xmax,npoints)  
=======
  end select

  call eigenvalue(npoints, xmin, xmax, potvec, mass, DD, eigvec)

  call writingpot(potvec, xmin, xmax, npoints)

  call writingew(npoints, xmin, xmax, DD, eigvec, valrange)
>>>>>>> b155cb8094cb3d17cda2289da32ebf776e59a5c5

  deallocate(base)
end program schroedinger
