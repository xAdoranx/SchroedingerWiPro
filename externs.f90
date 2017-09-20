module externs

  use formatting
  use f95_lapack
  implicit none

contains

  subroutine solvetridiag(DD, EE, eigvec)
    real(dp), allocatable, intent(inout) :: DD(:)
    real(dp), allocatable, intent(inout) :: EE(:)
    real(dp), allocatable, intent(out) :: eigvec(:,:)
    integer :: INFO
    allocate(eigvec(size(DD),size(DD)))
    
    call la_stev(DD, EE, eigvec, INFO)
     
    write(*,*) INFO
    
  end subroutine solvetridiag

end module externs
