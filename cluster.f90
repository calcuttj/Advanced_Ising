module cluster
  implicit none
  
  public grow_cluster
  public increase_stack
  public decrease_stack
contains
  subroutine grow_cluster(lattice,N,T)
    integer, intent(in) :: N
    integer, dimension(N,N), intent(inout) :: lattice
    integer, allocatable :: stack(:,:)
    real(8), intent(in) :: T

    real(8) :: rand1, rand2
    real(8) :: test1,test2,test3,test4
    integer :: random1,random2, init_site

    call init_random_seed

    call random_number(rand1)
    call random_number(rand2)

    random1 = ceiling(rand1*N)
    random2 = ceiling(rand2*N)

    init_site = lattice(random1,random2)
    allocate(stack(2,1))
    stack(1,1) = random1
    stack(2,1) = random2
    lattice(random1,random2) = -1*lattice(random1,random2)

    do while (size(stack) > 0)
!NEED TO FIX WITH FRIENDS ARRAY
       !Left Boundary Condition Check
       if (stack(1,1) == 1) then
          if (lattice(N,stack(2,1)) == init_site) then
             call random_number(test1)
             if ((1-exp(-2d0/T)) > test1) then
                lattice(N,stack(2,1)) = -1*lattice(N,stack(2,1))
                call increase_stack(stack,N,stack(2,1))
             end if
          end if
       else
          if (lattice(stack(1,1)-1,stack(2,1)) == init_site) then

             call random_number(test1)
             if ((1-exp(-2d0/T)) > test1) then
                lattice(stack(1,1)-1,stack(2,1)) = -1*lattice(stack(1,1)-1,stack(2,1))
                call increase_stack(stack,stack(1,1)-1,stack(2,1))
             end if
          end if
       end if

       !Right Boundary Condition Check
       if (stack(1,1) == N) then
          if (lattice(1,stack(2,1)) == init_site) then
             call random_number(test2)
             if ((1-exp(-2d0/T)) > test2) then
                lattice(1,stack(2,1)) = -1*lattice(1,stack(2,1))
                call increase_stack(stack,1,stack(2,1))
             end if
          end if
       else
          if (lattice(stack(1,1)+1,stack(2,1)) == init_site) then
             call random_number(test2)
             if ((1-exp(-2d0/T)) > test2) then
                lattice(stack(1,1)+1,stack(2,1)) = -1*lattice(stack(1,1)+1,stack(2,1))
                call increase_stack(stack,stack(1,1)+1,stack(2,1))
             end if
          end if
       end if

       !Top Boundary Condition Check
       if (stack(2,1) == 1) then
          if (lattice(stack(1,1),N) == init_site) then
             call random_number(test3)
             if ((1-exp(-2d0/T)) > test3) then
                lattice(stack(1,1),N) = -1*lattice(stack(1,1),N)
                call increase_stack(stack,stack(1,1),N)
             end if
          end if
       else
          if (lattice(stack(1,1),stack(2,1)-1) == init_site) then

             call random_number(test3)
             if ((1-exp(-2d0/T))>test3) then
                lattice(stack(1,1),stack(2,1)-1) = -1*lattice(stack(1,1),stack(2,1)-1)
                call increase_stack(stack,stack(1,1),stack(2,1)-1)
             end if
          end if
       end if

       !Bottom Boundary Condition Check
       if (stack(2,1) == N) then
          if (lattice(stack(1,1),1) == init_site) then
             call random_number(test4)
             if ((1-exp(-2d0/T)) > test4) then
                lattice(stack(1,1),1) = -1*lattice(stack(1,1),1)
                call increase_stack(stack,stack(1,1),1)
             end if
          end if
       else
          if (lattice(stack(1,1),stack(2,1)+1) == init_site) then
             call random_number(test4)
             if ((1-exp(-2d0/T))>test4) then
                lattice(stack(1,1),stack(2,1)+1) = -1*lattice(stack(1,1),stack(2,1)+1)
                call increase_stack(stack,stack(1,1),stack(2,1)+1)
             end if
          end if
       end if
       call decrease_stack(stack)
       !end checking cluster growth
    end do
  end subroutine grow_cluster


SUBROUTINE increase_stack(stack,i,j)
  INTEGER,DIMENSION(:,:),ALLOCATABLE :: tmp_arr
  integer,allocatable,intent(inout)::stack(:,:)
  integer, intent(in)::i,j
  integer new_dim
  integer :: a,b
  a = i
  b = j
  new_dim = size(stack)/2 + 1
  ALLOCATE(tmp_arr(2,new_dim))
  tmp_arr(:,1:SIZE(stack)/2)=stack
  DEALLOCATE(stack)
  ALLOCATE(stack(2,new_dim))
  stack=tmp_arr
  stack(1,size(stack)/2) = a
  stack(2,size(stack)/2) = b
ENDSUBROUTINE increase_stack

SUBROUTINE decrease_stack(stack)
  INTEGER,DIMENSION(:,:),ALLOCATABLE :: tmp_arr
  integer,allocatable,intent(inout)::stack(:,:)
  integer new_dim
  new_dim = size(stack)/2 - 1
  ALLOCATE(tmp_arr(2,new_dim))
  tmp_arr(:,1:SIZE(stack)/2-1)=stack(:,2:size(stack)/2)
  DEALLOCATE(stack)
  ALLOCATE(stack(2,new_dim))
  stack=tmp_arr

ENDSUBROUTINE decrease_stack

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


