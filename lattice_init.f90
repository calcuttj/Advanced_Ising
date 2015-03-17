module lattice_init


  Implicit None

  public init_lattice
  public randy

contains
  subroutine init_lattice(lattice,N,spin)
    integer, intent(in) :: N,spin
    integer, dimension(N,N), intent(out) :: lattice
    
    lattice = spin 
  
  end subroutine init_lattice

  subroutine randy(lattice,N)
    integer, intent(in) :: N
    integer, dimension(N,N), intent(out) :: lattice
    integer :: i,j
    real(8) :: rand

    do i =1, N
       do j=1,N
          call random_number(rand)
          if (rand > 0.5) then
             lattice(i,j) = -1
          else 
             lattice(i,j) = 1
          end if
       end do
    end do


  end subroutine randy

  subroutine find_friends(N,init_lattice,friends,indx)
    integer, intent(in) :: N, indx
    integer, intent(out) :: friends(4,N)



  end subroutine find_friends

end module lattice_init
