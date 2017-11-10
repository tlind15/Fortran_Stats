       Program Stats

       ! --- Need this interface to pass allocatable argument into the subroutine ---
       interface
       subroutine READAR(grades)
       real, allocatable, intent(out) :: grades(:)
       end subroutine
       end interface 

       ! --- Declaration ---
       real, allocatable :: grades(:)
       ! --- End declaration ---

       call READAR(grades)
       End Program Stats

       Subroutine READAR(array, size)
       integer, intent(in) :: size
       real, dimension(1:size) :: array
       do i = 1, size
        array(i) = i + 1
       end do
       return
       end
 