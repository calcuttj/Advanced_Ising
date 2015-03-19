program advanced_ising

  use lattice_init
  use cluster
  use calculations

  
  implicit none

  integer, parameter :: N=10
  integer :: ising_lattice(N,N), FRIENDS(4,0:(N*N-1))
  integer :: iter, iterations = 100
  real(8) :: initial_temp = 0.05
  real(8) :: final_temp = 5d0
  real(8) :: T, temp_step_size = 0.05
  real(8) :: prev_mag, mag, suscept
  integer :: flag

  write(*,*) "Hit 0 for random lattice, 1 for all up, -1 for all down"
  read(*,*) flag

  call init_random_seed()
  T = initial_temp
  open(unit=20,file="MagVsT.txt")
  open(unit=21,file="SuscVsT.txt")

  do while (T <= final_temp)
     if (flag == 0) then
        call randy(ising_lattice,N)
     else
        call init_lattice(ising_lattice,N,flag)
     end if
     print*, T
     call find_friends(N,friends)
     do iter = 1, iterations
        call grow_cluster(ising_lattice,N,T)    
     end do

     call calc_mag(ising_lattice,N,mag) 
     write(20,*) T,mag
 !    call calc_suscept(T,mag,prev_mag,suscept)
 !    write(21,*) T,suscept
     T = T + temp_step_size
  end do
end program advanced_ising


subroutine write_lattice(lattice,N)
  integer, intent(in)::N
  integer, intent(in)::lattice(N,N)

  integer :: i,j
  do i = 1, N
     do j =1,N
        write(*,'(I2,X)', advance='no') lattice(i,j)
     end do
     write(*,*) ''
  end do
  write(*,*) ''
  
end subroutine write_lattice

