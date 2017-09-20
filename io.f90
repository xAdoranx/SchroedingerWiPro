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
    
  subroutine reading(xmin, xmax, npoints, base, inttype, mass, valrange)
    integer, intent(out) :: npoints
    integer, allocatable, intent(out) :: valrange(:)
    real(dp), intent(out) :: xmin, xmax, mass
    real(dp), allocatable, intent(out) :: base(:,:)
    character(len=10), intent(out) :: inttype
    integer :: intpoints

    open(11, file="schrodinger.inp", status="old", form="formatted", action="read")

    read(11,*) mass
    read(11,*) xmin, xmax, npoints
    
    allocate(valrange(2))
    read(11,*) valrange
    
    read(11,*) inttype
    read(11,*) intpoints
    allocate(base(2,intpoints))
    read(11,*) base

    close(11)
    
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
      xdata(ii) = ((ii-1)*deltax)+xmin
    end do
    

    allocate(output(2,npoints+1))
    
    output(1,:) = xdata
    output(2,:) = potvec
    
    open(21, file="discrpot.dat", status="replace", form="formatted", action="write")
    

    write(21,"(2F14.7)") output

    close(21)
  
  end subroutine writingpot

  subroutine writingew(npoints, xmin, xmax, DD, eigvec, valrange)

    real(dp), intent(in) :: DD(:), eigvec(:,:)
    real(dp), intent(in) :: xmin, xmax
    integer, intent(in) :: npoints
    integer, intent(in) :: valrange(:)
    real(dp) :: deltax
    real(dp), allocatable :: xdata(:), output(:,:)
    integer :: ii, range

    open(22, file="energies.dat", status="replace", form="formatted", action="write")
    open(23, file="wfuncs.dat", status="replace", form="formatted", action="write")
    open(24, file="ewfuncs.dat", status="replace", form="formatted", action="write")

    write(22,"(F14.7)") DD

    deltax = (xmax - xmin) / npoints
    allocate(xdata(npoints+1))
    xdata(1) = xmin
    do ii=2,npoints+1
      xdata(ii) = xdata(ii-1) + deltax
    end do

    range = valrange(2) - valrange(1)
    allocate(output(npoints+1,range+1))

    output(:,1) = xdata
    output(:,2:range+1) = eigvec(:,valrange(1):valrange(2))
    !output = transpose(output)

    do ii=1,size(output(:,1),1)
      write(23,"(10000000000000F14.7)") output(ii,:)
    end do
    

    do ii=1,range
      output(:,ii+1) = output(:,ii+1) + DD(valrange(1)+ii-1)
    end do
    
    !write(*,*) output
    do ii=1,size(output(:,1),1)
      write(24,"(10000000000000F14.7)") output(ii,:)
    end do

    close(22)
    close(23)
    close(24)
    
  end subroutine writingew

  subroutine autotestread(compare)

    integer :: npoints
    integer, allocatable :: valrange(:)
    real(dp) :: xmin, xmax, mass
    real(dp), allocatable :: base(:,:)
    character(len=10) :: inttype
    integer :: intpoints, range
    real(dp), allocatable :: energiesori1(:), energiescalc1(:)
    real(dp), allocatable :: discrpotori1(:,:), discrpotcalc1(:,:)
    real(dp), allocatable :: wfuncsori1(:,:), wfuncscalc1(:,:)
    real(dp), allocatable :: ewfuncsori1(:,:), ewfuncscalc1(:,:)
    real(dp), allocatable :: energiesori2(:), energiescalc2(:)
    real(dp), allocatable :: discrpotori2(:,:), discrpotcalc2(:,:)
    real(dp), allocatable :: wfuncsori2(:,:), wfuncscalc2(:,:)
    real(dp), allocatable :: ewfuncsori2(:,:), ewfuncscalc2(:,:)
    real(dp), allocatable :: energiesori3(:), energiescalc3(:)
    real(dp), allocatable :: discrpotori3(:,:), discrpotcalc3(:,:)
    real(dp), allocatable :: wfuncsori3(:,:), wfuncscalc3(:,:)
    real(dp), allocatable :: ewfuncsori3(:,:), ewfuncscalc3(:,:)
    logical, allocatable, intent(out) :: compare(:,:)

    open(31, file="test/schrodingertest1.inp", status="old", form="formatted", action="read")

    read(31,*) mass
    read(31,*) xmin, xmax, npoints
    
    allocate(valrange(2))
    read(31,*) valrange
    range = valrange(2) - valrange(1)
    
    read(31,*) inttype
    read(31,*) intpoints
    allocate(base(2,intpoints))
    read(31,*) base

    close(31)

    allocate(compare(4,3))

    open(41, file="test/energies1.dat.ori", status="old", form="formatted", action="read")
    open(42, file="test/discrpot1.dat.ori", status="old", form="formatted", action="read")
    open(43, file="test/wfuncs1.dat.ori", status="old", form="formatted", action="read")
    open(44, file="test/ewfuncs1.dat.ori", status="old", form="formatted", action="read")
    
    open(45, file="test/energies1.dat", status="old", form="formatted", action="read")
    open(46, file="test/discrpot1.dat", status="old", form="formatted", action="read")
    open(47, file="test/wfuncs1.dat", status="old", form="formatted", action="read")
    open(48, file="test/ewfuncs1.dat", status="old", form="formatted", action="read")


    allocate(energiesori1(range))
    allocate(energiescalc1(range))
    allocate(discrpotori1(npoints,2))
    allocate(discrpotcalc1(npoints,2))
    allocate(wfuncsori1(npoints,range+1))
    allocate(wfuncscalc1(npoints,range+1))
    allocate(ewfuncsori1(npoints,range+1))
    allocate(ewfuncscalc1(npoints,range+1))
    read(41,*) energiesori1
    read(45,*) energiescalc1
    read(42,*) discrpotori1
    read(46,*) discrpotcalc1
    read(43,*) wfuncsori1
    read(47,*) wfuncscalc1
    read(44,*) ewfuncsori1
    read(48,*) ewfuncscalc1

    compare(1,1) = maxval(abs((energiesori1-energiescalc1))) < tol
    compare(2,1) = maxval(abs((discrpotori1-discrpotcalc1))) < tol
    compare(3,1) = maxval(abs((wfuncsori1-wfuncscalc1))) < tol
    compare(4,1) = maxval(abs((ewfuncsori1-ewfuncscalc1))) < tol

    deallocate(energiesori1)
    deallocate(energiescalc1)
    deallocate(discrpotori1)
    deallocate(discrpotcalc1)
    deallocate(wfuncsori1)
    deallocate(wfuncscalc1)
    deallocate(ewfuncsori1)
    deallocate(ewfuncscalc1)

    close(41)
    close(42)
    close(43)
    close(44)
    close(45)
    close(46)
    close(47)
    close(48)

    

    open(32, file="test/schrodingertest2.inp", status="old", form="formatted", action="read")

    read(32,*) mass
    read(32,*) xmin, xmax, npoints
    
    allocate(valrange(2))
    read(32,*) valrange
    range = valrange(2) - valrange(1)
    
    read(32,*) inttype
    read(32,*) intpoints
    allocate(base(2,intpoints))
    read(32,*) base

    close(32)

    open(51, file="test/energies2.dat.ori", status="old", form="formatted", action="read")
    open(52, file="test/discrpot2.dat.ori", status="old", form="formatted", action="read")
    open(53, file="test/wfuncs2.dat.ori", status="old", form="formatted", action="read")
    open(54, file="test/ewfuncs2.dat.ori", status="old", form="formatted", action="read")
    
    open(55, file="test/energies2.dat", status="old", form="formatted", action="read")
    open(56, file="test/discrpot2.dat", status="old", form="formatted", action="read")
    open(57, file="test/wfuncs2.dat", status="old", form="formatted", action="read")
    open(58, file="test/ewfuncs2.dat", status="old", form="formatted", action="read")
    
    allocate(energiesori2(range))
    allocate(energiescalc2(range))
    allocate(discrpotori2(npoints,2))
    allocate(discrpotcalc2(npoints,2))
    allocate(wfuncsori2(npoints,range+1))
    allocate(wfuncscalc2(npoints,range+1))
    allocate(ewfuncsori2(npoints,range+1))
    allocate(ewfuncscalc2(npoints,range+1))
    read(51,*) energiesori2
    read(55,*) energiescalc2
    read(52,*) discrpotori2
    read(56,*) discrpotcalc2
    read(53,*) wfuncsori2
    read(57,*) wfuncscalc2
    read(54,*) ewfuncsori2
    read(58,*) ewfuncscalc2

    compare(1,2) = maxval(abs((energiesori2-energiescalc2))) < tol
    compare(2,2) = maxval(abs((discrpotori2-discrpotcalc2))) < tol
    compare(3,2) = maxval(abs((wfuncsori2-wfuncscalc2))) < tol
    compare(4,2) = maxval(abs((ewfuncsori2-ewfuncscalc2))) < tol

    deallocate(energiesori2)
    deallocate(energiescalc2)
    deallocate(discrpotori2)
    deallocate(discrpotcalc2)
    deallocate(wfuncsori2)
    deallocate(wfuncscalc2)
    deallocate(ewfuncsori2)
    deallocate(ewfuncscalc2)

    close(51)
    close(52)
    close(53)
    close(54)
    close(55)
    close(56)
    close(57)
    close(58)


    open(61, file="test/energies3.dat.ori", status="old", form="formatted", action="read")
    open(62, file="test/discrpot3.dat.ori", status="old", form="formatted", action="read")
    open(63, file="test/wfuncs3.dat.ori", status="old", form="formatted", action="read")
    open(64, file="test/ewfuncs3.dat.ori", status="old", form="formatted", action="read")
    
    open(65, file="test/energies3.dat", status="old", form="formatted", action="read")
    open(66, file="test/discrpot3.dat", status="old", form="formatted", action="read")
    open(67, file="test/wfuncs3.dat", status="old", form="formatted", action="read")
    open(68, file="test/ewfuncs3.dat", status="old", form="formatted", action="read")


    open(33, file="test/schrodingertest3.inp", status="old", form="formatted", action="read")

    read(33,*) mass
    read(33,*) xmin, xmax, npoints
    
    allocate(valrange(2))
    read(33,*) valrange
    range = valrange(2) - valrange(1)
    
    read(33,*) inttype
    read(33,*) intpoints
    allocate(base(2,intpoints))
    read(33,*) base

    close(33)

    allocate(energiesori3(range))
    allocate(energiescalc3(range))
    allocate(discrpotori3(npoints,2))
    allocate(discrpotcalc3(npoints,2))
    allocate(wfuncsori3(npoints,range+1))
    allocate(wfuncscalc3(npoints,range+1))
    allocate(ewfuncsori3(npoints,range+1))
    allocate(ewfuncscalc3(npoints,range+1))
    read(61,*) energiesori1
    read(65,*) energiescalc1
    read(62,*) discrpotori1
    read(66,*) discrpotcalc1
    read(63,*) wfuncsori1
    read(67,*) wfuncscalc1
    read(64,*) ewfuncsori1
    read(68,*) ewfuncscalc1

    compare(1,3) = maxval(abs((energiesori3-energiescalc3))) < tol
    compare(2,3) = maxval(abs((discrpotori3-discrpotcalc3))) < tol
    compare(3,3) = maxval(abs((wfuncsori3-wfuncscalc3))) < tol
    compare(4,3) = maxval(abs((ewfuncsori3-ewfuncscalc3))) < tol

    deallocate(energiesori3)
    deallocate(energiescalc3)
    deallocate(discrpotori3)
    deallocate(discrpotcalc3)
    deallocate(wfuncsori3)
    deallocate(wfuncscalc3)
    deallocate(ewfuncsori3)
    deallocate(ewfuncscalc3)

    close(61)
    close(62)
    close(63)
    close(64)
    close(65)
    close(66)
    close(67)
    close(68)
  end subroutine autotestread
    
end module io
