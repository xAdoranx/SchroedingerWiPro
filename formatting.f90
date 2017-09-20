!> Gibt die Genauigkeit der Fließkommazahlen an
!!
!! \param dp Genauigkeit von Fließkommazahlen
module formatting

  implicit none

  integer,parameter :: dp = selected_real_kind(12,99)
  real(dp), parameter :: tol = 10**(-6)

end module formatting
