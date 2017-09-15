program schroedinger
  
  use formatting
  use calculations
  implicit none

  integer :: npoints, intpoints, ii
  real(dp) :: xmin, xmax
  real(dp), allocatable :: potvec(:), base(:,:)

  write(*,*) 'enter xmin'
  read(*,*) xmin
  write(*,*) 'enter xmax'
  read(*,*) xmax
  write(*,*) 'enter npoints'
  read(*,*) npoints
  write(*,*) 'enter intpoints'
  read(*,*) intpoints
  allocate(base(2,intpoints))
  write(*,*) 'enter base(:,:)'
  read(*,*) base

  call interpolationlin(npoints, xmin, xmax, base, potvec)

  do ii=1,size(potvec)
    write(*,*) potvec(ii)
  end do
  

  deallocate(base)
end program schroedinger
