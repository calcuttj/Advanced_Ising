module cluster
  use lattice_init
  implicit none
 

  public grow_cluster
  public increase_queue
  public decrease_queue
  public getSpin
  public addtoCluster
  public init_random_seed
contains
  subroutine grow_cluster(lattice,friends,N,T) !!before writing this, we need the individual subroutines called within to be complete
!!if we don't, we run the risk of not knowing/anticipating what they need to do/how they need to work. i.e. increasing/decreasing the stack 
    integer, intent(in) :: N, friends(4,0:N*N-1)
    integer, intent(inout) :: lattice(N,N)
    integer, allocatable :: queue(:)
    real(8), intent(in) :: T
    
    real(8) :: rand1, test
    integer :: rand_indx, init_spin, spin


    !!Get the random point in the lattice, along with it's spin
    call random_number(rand1)
    rand_indx = ceiling(rand1*(N*N))-1
    call getSpin(rand_indx,N,lattice,init_spin)
    allocate(queue(0))
    ! add the random point neighbours to the queue  
    call addtoCluster(N,rand_indx,friends,lattice,queue)
    
    do while (size(queue) > 0)
       call getSpin(queue(1),N,lattice,spin) !!take first indx in queue
       if (spin == init_spin) then !!check if it has same spin
          write(*,*) "SAME SPIN"
          call random_number(test) !! Apply the MC test
          if ((1-exp(-2d0/T)) > test) then
             write(*,*) "MC successful"
             call addtoCluster(N,queue(1),friends,lattice,queue) !!flips spins and adds neighbours to queue
          end if
       end if
       write(*,*) "Queue size: ", size(queue)
       call decrease_queue(queue) !! rid of first indx in queue
       write(*,*) "New size: ", size(queue)
    end do

  end subroutine grow_cluster

  subroutine getSpin(indx,N,lattice,spin)
    integer, intent(in) :: indx, N
    integer, intent(inout) :: lattice(N,N)
    integer, intent(out) :: spin
    integer :: i,j

    call indx_to_coord(indx,N,i,j)
    spin = lattice(i,j)

  end subroutine getSpin

  subroutine addtoCluster(N,indx,friends,lattice,queue)
    integer, intent(in) :: N, indx
    integer, intent(in) :: friends(4,0:N*N-1)
    integer, intent(inout),allocatable :: queue(:)
    integer, intent(inout) :: lattice(N,N)
    integer :: i,j,k
    

    !flip spin
    call indx_to_coord(indx,N,i,j)
    lattice(i,j) = -1*lattice(i,j)

    !add friends to queue
    do k = 1, 4
       write(*,*) "size of queue: ",  size(queue)
       write(*,*) friends(k,indx)
       call increase_queue(queue,friends(k,indx))
       write(*,*) "got here", k
    end do
  end subroutine addtoCluster

  SUBROUTINE increase_queue(queue,indx)
    INTEGER,DIMENSION(:),ALLOCATABLE :: tmp_arr
    integer,allocatable,intent(inout)::queue(:)
    integer, intent(in)::indx
    integer new_dim
    integer :: i
    write(*,*) indx
    i = indx
    new_dim = size(queue) + 1
    ALLOCATE(tmp_arr(new_dim))

    tmp_arr(1:SIZE(queue))=queue

    DEALLOCATE(queue)
    ALLOCATE(queue(new_dim))
    queue=tmp_arr
    queue(size(queue)) = i
    write (*,*) queue(:)
  ENDSUBROUTINE increase_queue

  SUBROUTINE decrease_queue(queue)
    INTEGER,DIMENSION(:),ALLOCATABLE :: tmp_arr
    integer,allocatable,intent(inout)::queue(:)
    integer :: new_dim

    new_dim = size(queue) - 1
    ALLOCATE(tmp_arr(new_dim))
    tmp_arr=queue(2:size(queue))
    DEALLOCATE(queue)
    ALLOCATE(queue(new_dim))
    queue=tmp_arr

  ENDSUBROUTINE decrease_queue

  subroutine init_random_seed()
    implicit none

    integer, allocatable :: seed(:)
    integer :: i, n, un, istat, dt(8), pid, t(2), s
    integer(8) :: count, tms
    call random_seed(size = n)
    allocate(seed(n))
    open(newunit=un, file="/dev/urandom", access="stream",&
         form="unformatted", action="read", status="old", &
         iostat=istat)
    if (istat == 0) then
       read(un) seed
       close(un)
    else
       call system_clock(count)
       if (count /= 0) then
          t = transfer(count, t)
       else
          call date_and_time(values=dt)
          tms = (dt(1) - 1970)*365_8 * 24 * 60 * 60 * 1000 &
               + dt(2) * 31_8 * 24 * 60 * 60 * 1000 &
               + dt(3) * 24 * 60 * 60 * 60 * 1000 &
               + dt(5) * 60 * 60 * 1000 &
               + dt(6) * 60 * 1000 + dt(7) * 1000 &
               + dt(8)
          t = transfer(tms, t)
       end if
       s = ieor(t(1), t(2))
       pid = getpid() + 1099279 ! Add a prime
       s = ieor(s, pid)
       if (n >= 3) then
          seed(1) = t(1) + 36269
          seed(2) = t(2) + 72551
          seed(3) = pid
          if (n > 3) then
             seed(4:) = s + 37 * (/ (i, i = 0, n - 4) /)
          end if
       else
          seed = s + 37 * (/ (i, i = 0, n - 1 ) /)
       end if
    end if
    call random_seed(put=seed)
  end subroutine init_random_seed


end module cluster


