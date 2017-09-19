module io

  use formatting
  implicit none

contains

  !> Liest die Benutzerangaben aus der Input-Datei "schrodinger.inp" ein.
  !!
  !! \details Es werden alle Angaben des Benutzers zur Lösung der Schrödinger-Gleichung eingelesen.
  !! Die Masse des Quantenobjektes, der örtliche Rahmen, die zu berechnenden Eigenwerte, der
  !! Interpolationstyp des Potentials und schließlich die Anzahl und die Position den
  !! Interpolationsstützpunkte.
  !!
  !! \param xmin  unterer X-Wert
  !! \param xmax  oberer X-Wert
  !! \param npoints  Genauigkeit der örtlichen Auflösung
  !! \param base  X und Y-Werte der Stützstellen des Potentials
  !! \param inttype  Art der Interpolation (linear oder polynom)
  !! \param mass Masse des Teilchens im Potential (output)
    
  subroutine reading(xmin, xmax, npoints, base, inttype, mass)
    integer, intent(out) :: npoints
    real(dp), intent(out) :: xmin, xmax, mass
    real(dp), allocatable, intent(out) :: base(:,:)
    character(len=7), intent(out) :: inttype
    integer :: intpoints

    open(11, file="schrodinger.inp", status="old", form="formatted", action="read")

    read(11,*) mass
    read(11,*) xmin, xmax, npoints
    read(11,*) inttype
    read(11,*) intpoints
    allocate(base(2,intpoints))
    read(11,*) base
  end subroutine reading
  
  !>  Schreibt die Ergebnisse in die Output-Textdateien
  !!
  !! \details Berechnung der X-Werte für die Ausgabe und Ausgabe des Potentials, der Energiewerte
  !! und der Wellenfunktionen
  !!
  !! \param xmin  unterer X-Wert
  !! \param xmax  oberer X-Wert
  !! \param npoints  Genauigkeit der örtlichen Auflösung
  !! \param potvec  Y-Werte der Interpolation des Potentials, entweder durch lineare oder
  !! Polynominterpolation berechnet
  !!
  subroutine writingpot(potvec, xmin, xmax, npoints)

    real(dp), intent(in) :: potvec(:)
    real(dp), intent(in) :: xmin, xmax
    integer, intent(in) :: npoints
    real(dp), allocatable :: xdata(:), output(:,:)
    real(dp) :: deltax
    integer :: ii

    allocate(xdata(npoints+1))
    deltax = (xmax-xmin)/npoints
    do ii = 1 , npoints+1
      xdata(ii) = ((ii-1)*deltax+xmin)
    end do

    allocate(output(2,npoints+1))
    
    output(1,:) = xdata
    output(2,:) = potvec
    
    open(21, file="discrpot.dat", status="replace", form="formatted", action="write")
    

    write(21,"(2F8.2)") output
  
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
