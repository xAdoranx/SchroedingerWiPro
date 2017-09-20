!> writes autotest results
program autotest

  use formatting
  use io
  implicit none

  !> Input test results from autotest
  logical, allocatable :: compare(:,:)
  logical :: compenerg, comppot, compwfun, compewfun

  call autotestread(compare)

  compenerg = compare(1,1).and.compare(1,2).and.compare(1,3)
  comppot = compare(2,1).and.compare(2,2).and.compare(2,3)
  compwfun = compare(3,1).and.compare(3,2).and.compare(3,3)
  compewfun = compare(4,1).and.compare(4,2).and.compare(4,3)

  if(compenerg.and.comppot.and.compwfun.and.compewfun) then
    write(*,*) "Test succesfully done."
  else
    write(*,*) "ERROR: Values differ too much from test values."
  end if
  
end program autotest
