program autotest

  use formatting
  use io
  implicit none

  logical, allocatable :: compare(:,:)

  call autotestread(compare)

  write(*,*) compare
  
end program autotest
