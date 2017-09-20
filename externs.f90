!> using Lapack-package to calculate diagonalmatrix
module externs

  use formatting
  use f95_lapack
  implicit none

contains

  !> calculate diagonalmatrix using lapack-package
  subroutine solvetridiag(DD, EE, eigvec)

    !> Input diagonal elements and output eigenvalues 
    !!
    !! must be in a vector
    real(dp), allocatable, intent(inout) :: DD(:)

    !> Input diagonal elements
    !!
    !! will be deallocated during calculation
    real(dp), allocatable, intent(inout) :: EE(:)

    !> Output eigenvectors
    !!
    !! must be an array
    real(dp), allocatable, intent(out) :: eigvec(:,:)
    integer :: INFO
    allocate(eigvec(size(DD),size(DD)))
    
    call la_stev(DD, EE, eigvec, INFO)
     
    write(*,*) INFO
    
  end subroutine solvetridiag

end module externs
