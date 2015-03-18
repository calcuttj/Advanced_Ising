module lattice_init


  Implicit None

  public init_lattice
  public randy
  public find_friends
  
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
    
    integer :: i,j

    call indx_to_coord(indx,N,i,j)


  end subroutine find_friends
  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
!Moved from myprog.f90 ---> Easier to use in building neighbors lattice
subroutine indx_to_coord(indx,N,x,y)
  integer, intent(in) :: indx, N
  integer, intent(out) :: x,y
  x = indx/N !!! I think this should be (indx/N + 1)
             !!! Example: the first row (x = 1) contains sites with indx {0:N-1}
             !!!          indx/N would make x = 0 for all in first row. 

  y = mod(indx/N) !!! Should this be mod((indx+1),N) ?
                  !!! Similar reasoning as above


                   !!! Note: these would be fine if we changed how our lattice was defined: each dimension ranging from 0 to N-1 rather than 1 to N
end subroutine indx_to_coord

subroutine coord_to_indx(x,y,N,indx)
  integer, intent(in) :: x,y,N
  integer, intent(out) :: indx
  indx = x*N+y !!I think this should be (x-1)*N + (y-1)
               !!Example (N =3): 
               !!        x = 1, y = 1 results in indx = 0
               !!        x = 1, y = 2 results in indx = 1
               !!        x = 1, y = 3 results in indx = 2
               !!        x = 2, y = 1 results in indx = 3
               !!        And so on
               !!       
               !!        Resulting in our array corresponding to: 
               !!        0, 1, 2
               !!        3, 4, 5
               !!        6, 7, 8
 
end subroutine coord_to_indx
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
end module lattice_init
