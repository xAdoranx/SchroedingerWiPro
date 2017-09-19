module io

  use formatting
  implicit none

contains
  
  subroutine reading(xmin, xmax, npoints, base, inttype, mass)
    integer, intent(out) :: npoints
    real(dp), intent(out) :: xmin, xmax, mass
    real(dp), allocatable, intent(out) :: base(:,:)
    character, intent(out) :: inttype
    integer :: intpoints

    open(11, file="schrodinger.inp", status="old", form="formatted", action="read")

    read(11,*) mass
    read(11,*) xmin, xmax, npoints
    read(11,*) inttype
    read(11,*) intpoints
    allocate(base(2,intpoints))
    read(11,*) base
  end subroutine reading
  
  subroutine writingpot(potvec)

    real(dp), intent(in) :: potvec(:)
    open(21, file="discrpot.dat", status="replace", form="formatted", action="write")

    write(21,"(F8.2)") potvec
  
  end subroutine writingpot

  subroutine writingew(npoints, xmin, xmax, DD, eigvec)

    real(dp), intent(in) :: DD(:), eigvec(:,:)
    real(dp), intent(in) :: xmin, xmax
    integer, intent(in) :: npoints
    real(dp) :: deltax
    real(dp), allocatable :: xval(:), output(:,:)
    integer :: ii

    open(12, file="energies.dat", status="replace", form="formatted", action="write")
    open(13, file="wfuncs.dat", status="replace", form="formatted", action="write")
    open(14, file="ewfuncs.dat", status="replace", form="formatted", action="write")

    write(12,"(F12.3)") DD

    deltax = (xmax - xmin) / npoints
    allocate(xval(npoints+1))
    xval(1) = xmin
    do ii=2,npoints
      xval(ii) = xval(ii-1) + deltax
    end do

    allocate(output(npoints+1,npoints+2))

    output(:,1) = xval
    output(:,2:npoints+2) = eigvec
    !output = transpose(output)

    do ii=1,size(output(:,1),1)
      write(13,"(F8.4,100000000000F8.4)") output(ii,:)
    end do
    
  end subroutine writingew
    
end module io
