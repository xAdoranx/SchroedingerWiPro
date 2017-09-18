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
  
  subroutine writing(potvec,xmin,xmax,npoints)

    real(dp), intent(in) :: potvec(:)
    real(dp), intent(in) :: xmin, xmax
    integer, intent(in) :: npoints
    real(dp), allocatable :: xdata(:),output(:,:)
    real(dp) :: deltax
    integer :: ii
    allocate(xdata(npoints+1))
    deltax=(xmax-xmin)/npoints
    do ii = 1, npoints+1
      xdata(ii) = ((ii-1)*deltax + xmin)
    end do
    allocate(output(2,npoints+1))
    output(1,:)=xdata
    output(2,:)=potvec
    open(21, file="discrpot.dat", status="replace", form="formatted", action="write")
      
    write(21,"(2F8.2)") output
  
  end subroutine writing

  
end module io
