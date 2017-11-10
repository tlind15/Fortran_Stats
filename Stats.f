       Program Stats

       character(len=25) :: f
       real, dimension(:), allocatable :: array
       integer :: size
       print *, "Input size of the array: "
       read *, size
       allocate(array(size))
       call READAR(array, size)
       print *, array
       deallocate(array)
       End Program Stats

       Subroutine READAR(array, size)
       integer, intent(in) :: size
       real, dimension(1:size) :: array
       do i = 1, size
        array(i) = i + 1
       end do
       return
       end
