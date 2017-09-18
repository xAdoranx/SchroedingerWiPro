module io

  use formatting
  implicit none

contains
  
  subroutine reading(xmin, xmax, npoints, base, inttype)
    integer, intent(out) :: npoints
    real(dp), intent(out) :: xmin, xmax
    real(dp), allocatable, intent(out) :: base(:,:)
    character(len=7), intent(out) :: inttype
    integer :: intpoints

    open(11, file="schrodinger.inp", status="old", form="formatted", action="read")

    read(11,*) xmin, xmax, npoints
    read(11,*) inttype
    write(*,*) inttype
    read(11,*) intpoints
    write(*,*) intpoints
    allocate(base(2,intpoints))
    read(11,*) base
    write(*,*) base
  end subroutine reading
  
  subroutine writing(potvec)

    real(dp), intent(in) :: potvec(:)
    open(21, file="discrpot.dat", status="replace", form="formatted", action="write")

    write(21,"(2F8.2)") potvec
  
  end subroutine writing

  
end module io
