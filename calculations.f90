module calculations
  use formatting
  implicit none

contains
  
  subroutine interpolationlin(npoints, xmin, xmax, base, potvec)

    integer, intent(in) :: npoints
    real(dp), intent(in) :: xmin, xmax
    real(dp), intent(in) :: base(:,:)
    real(dp), allocatable, intent(out) :: potvec(:)
    integer :: ii, jj, nn, iipoints, xtemp
    real(dp) :: deltax, deltay
    real(dp), allocatable :: fullbase(:,:)
    logical :: stepcase

    deltax = (xmax - xmin) / npoints
    nn = size(base(1,:),dim=1)
    allocate(fullbase(2,nn+2))
    fullbase(:,1) = (/xmin, 0.0_dp/)
    fullbase(:,nn+2) = (/xmax, 0.0_dp/)
    fullbase(:,2:nn+1) = base

    xtemp = 1
    allocate(potvec(npoints))
    potvec(1) = 0
    
    do jj=1,nn+1
      stepcase = (fullbase(1,jj+1) - fullbase(1,jj)) == 0
      
      if (stepcase) then
        iipoints = xtemp
        deltay = (fullbase(2,jj+1) - fullbase(2,jj))
      else
        iipoints = floor((fullbase(1,jj+1) - fullbase(1,1)) / deltax)
        deltay = (fullbase(2,jj+1) - fullbase(2,jj)) / (iipoints-xtemp+1)
      end if
      
      do ii=xtemp, iipoints
        if (jj==1) then
          potvec(ii) = deltay
        else
          potvec(ii) = potvec(ii-1) + deltay
        end if
      end do
      
      xtemp = iipoints+1
    end do
    
  end subroutine interpolationlin

  subroutine interpolationpol(npoints, xmin, xmax, base, potvec)

    integer, intent(in) :: npoints
    real(dp), intent(in) :: xmin, xmax
    real(dp), intent(in) :: base(:,:)
    real(dp), allocatable, intent(out) :: potvec(:)
    integer :: nn, ii, jj, xx
    real(dp) :: deltax
    real(dp), allocatable :: L(:)
    

    deltax= (xmax-xmin) / npoints
    nn = size(base(1,:),dim=1)
    allocate(L(nn))
    allocate(potvec(npoints+1))

    do xx = 1 , npoints+1
      L = 1.0_dp
      do ii = 1, nn
        do jj = 1, nn
          if (ii /= jj) then
            L(ii) = ((((xx-1)*deltax+xmin)- base(1,jj))/(base(1,ii)-base(1,jj)))*L(ii)
          end if
        end do
        if (ii==1) then
          potvec(xx) = L(ii) * base(2,ii)
        else
          potvec(xx) = potvec(xx) + L(ii) * base(2,ii)
        end if
      end do
    end do
    
  end subroutine interpolationpol
  
    
  
  

end module calculations
