       Program Stats

       ! ======================= Program Interface ===================================
       ! Need this interface to pass allocatable argument into the subroutine 

       interface
       subroutine READAR(grades, count)
       real, allocatable, intent(out) :: grades(:)
       integer, intent(out) :: count
       end subroutine
       end interface

       ! ======================== End Interface =====================================




       ! =============== Begin Main program ==============================

       ! --------------------- Declaration -------------------------------
       real, allocatable :: grades(:) !grades array to be allocated later
       integer :: count !the number of grades in the file
       ! -----------------------------------------------------------------

       call READAR(grades, count)
       call SHELL(grades, count)
       print *, grades
       deallocate(grades)
       End Program Stats

       ! ================ End Main Program ===============================




       ! =================== READAR Subroutine ====================

       !  -opens a user inputted file
       !  -counts the number of entries in the file
       !  -reads the file entries into an array
       ! ***Subroutine assumes that each grade entry is on a new line***
    
      
       Subroutine READAR(grades, count)
       ! ------------------- Declaration -----------------------
       integer, intent(out) :: count !the number of grades in the file
       real, allocatable, intent(out) :: grades(:)
       character (len=100) :: ftext !the path of the grades file
       ! -------------------------------------------------------
       
       ! ------ Determine number of grade entries -----
       print *, "Input name of grades file: "
       read *, ftext
       count = 0
       open (unit = 1, file = ftext)

       ! --- loop while not EOF ---
 3     read(1,*,end=5) !if EOF, go to 5, otherwise read
       count = count + 1
       go to 3
       ! ----- finish looping -----

 5     close(1)
       ! --------- finish number of entries -----------
       
       
       ! ----------- Read in grades to array ------------- 
       allocate(grades(1:count))
       open (unit = 2, file = ftext)
       do i = 1, count
       	read (2,*) grades(i) !put grades values into array
       end do
       close(2)
       ! ---------- finish reading in grades -------------

       return
       end

       ! ================ End READAR Subroutine ==================




       ! ================ SHELL Subroutine =======================

       ! sorts the array in ascending order


       Subroutine SHELL (array, size)
       ! -----------------------Declaration-----------------------
       integer, intent(in) :: size
       real, intent(inout) :: array(1:size)
       integer :: sequence(1:5)
       integer :: gap
       integer :: temp
       ! ----------------------------------------------------------

       sequence = (/57, 23, 10, 4, 1/) !Marcin Ciura optimal shell sort gap sequence

       do i = 1, 5 !iterate through all the gap sizes in the above sequence
       	gap = sequence(i)
 1       if (gap < size) then
       		do j = 1, size 
       			k = 1
 2       		if (j + k*gap <= size) then
       				l = j + k*gap
       				m = 0
 3       			if (k - m >= 1 .AND. array(l) <= array(j+(k-m-1)*gap)) then
       					m = m+1
       					go to 3
       				else if (m > 0) then
       					do n = 1, m
       					 temp = array(l - gap)
       					 array(l - gap) = array(l)
       					 array(l) = temp
       					 l = l - gap
       					end do	
       				end if
       				k = k + 1
       				go to 2
       			end if
       		if (gap == 1) then
       			go to 5
   			end if
       		end do 
 5      end if
       end do
       return 
       end

       ! =============== End SHELL Subroutine ==================== 