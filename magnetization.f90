module magnetization

implicit none

public calc_mag

contains
  
  subroutine calc_mag(lattice, N, mag)
    integer,intent(in) :: N
    integer, intent(in) :: lattice(N,N)
    real(8), intent(out) :: mag
    mag = 0
    mag = sum(lattice(:,:))
    mag = mag/(N*N)
  end subroutine calc_mag


end module magnetization
