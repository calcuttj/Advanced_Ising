module calculations

implicit none

public calc_mag

contains
  
  subroutine calc_mag(lattice, N, mag)
    integer,intent(in) :: N
    integer, intent(in) :: lattice(N,N)
    real(8), intent(out) :: mag

    mag = 0
    mag = sum(lattice(:,:))
    mag = ABS(mag/(N*N))

  end subroutine calc_mag

  subroutine calc_suscept(T,mag,prev_mag,suscept)

    real(8), intent(in) :: T
    real(8), intent(in) :: mag
    real(8), intent(inout) :: prev_mag
    real(8), intent(out) :: suscept

    suscept = (mag-prev_mag)/T
    prev_mag = mag

  end subroutine calc_suscept

end module calculations
