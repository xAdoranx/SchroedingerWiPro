!> specifies accuracy
module formatting

  implicit none

  !> accuracy for floating point numbers
  integer,parameter :: dp = selected_real_kind(12,99)
  real(dp), parameter :: tol = 10**(-6)

end module formatting
