!> Contains calculations for linear and polynomial interpolation and eigenvalues
module calculations
  use formatting
  use externs
  implicit none

contains

  !> calculations for linear interpolation
  subroutine interpolationlin(npoints, xmin, xmax, base, potvec)

    !> Input number of x values
    !!
    !! must be an integer
    integer, intent(in) :: npoints

    !> Input minimum and maximum x values
    !!
    !! xmin and xmax must be a real number
    real(dp), intent(in) :: xmin, xmax

    !> Input bases for potential
    !!
    !! base must be an array with two rows
    real(dp), intent(in) :: base(:,:)

    !> Output y-values of potential
    !!
    !! is a vector of the same length as the x-datas
    real(dp), allocatable, intent(out) :: potvec(:)
    integer :: ii, jj, nn, jjstart, iipoints, xtemp
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
    allocate(potvec(npoints+1))
    if(fullbase(1,1)==fullbase(1,2)) then
      potvec(1) = fullbase(2,2)
      jjstart = 2
    else
      potvec(1) = fullbase(2,1)
      jjstart = 1
    end if    

    do jj=jjstart,nn+1
      stepcase = (fullbase(1,jj+1) - fullbase(1,jj)) == 0
      
      if (stepcase) then
        iipoints = xtemp
        deltay = (fullbase(2,jj+1) - fullbase(2,jj))
      else
        iipoints = floor((fullbase(1,jj+1) - fullbase(1,1)) / deltax)
        deltay = (fullbase(2,jj+1) - fullbase(2,jj)) / (iipoints-xtemp+1)
      end if
      
      do ii=xtemp, iipoints
          potvec(ii+1) = potvec(ii) + deltay
      end do
      
      xtemp = iipoints+1
    end do
    
  end subroutine interpolationlin

  !> calculations for polynomial interpolation
  subroutine interpolationpol(npoints, xmin, xmax, base, potvec)

    !> Input number of x values
    !!
    !! must be an integer
    integer, intent(in) :: npoints

    !> Input minimum and maximum x values
    !!
    !! xmin and xmax must be a real number
    real(dp), intent(in) :: xmin, xmax

    !> Input bases for potential
    !!
    !! base must be an array with two rows
    real(dp), intent(in) :: base(:,:)

    !> Output y-values of potential
    !!
    !! is a vector of the same length as the x-datas
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
  

  subroutine eigenvalue(npoints, xmin, xmax, potvec, mass, DD, eigvec)

    !> Input number of x values
    !!
    !! must be an integer
    integer, intent(in) :: npoints

    !> Input minimum and maximum x values and mass of particle
    !!
    !! xmin, xmax and mass must be a real number
    real(dp), intent(in) :: xmin, xmax, mass

    !> Input y-values of potential
    !!
    !! must be a vector of the same length as the x-datas
    real(dp), intent(in) :: potvec(:)

    !> Output eigenvalues(DD) and eigenvectors(eigvec)
    !!
    !! Eigenvalues are in a vector and eigenvectors are in an array
    real(dp), allocatable, intent(inout) :: DD(:), eigvec(:,:)
    real(dp), allocatable :: EE(:)
    real(dp) :: aa, deltax

    deltax = (xmax - xmin) / npoints
    aa = 1 / (mass * deltax**2)
    allocate(DD(npoints+1))
    DD = potvec + aa
    allocate(EE(npoints+1))
    EE(1:npoints) = (aa / 2) * (-1)
    
    call solvetridiag(DD, EE, eigvec)

  end subroutine eigenvalue
  
end module calculations
