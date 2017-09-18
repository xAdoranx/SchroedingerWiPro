module externs

  use formatting
  use f95_lapack
  implicit none

contains

  subroutine solvetridiag(DD, EE)
    real(dp), allocatable, intent(inout) :: DD(:)
    real(dp), allocatable, intent(in) :: EE(:)
    
    call la_stev(DD, EE)
    
  end subroutine solvetridiag

end module externs
