       Program Stats
       implicit none

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
       real :: Mean !this is a delcaration of the function Mean
       real :: Median !this is the delcaration of the function Median
       real :: Variance !this is the delcaration of the function Variance
       real :: value_variance 
       ! -----------------------------------------------------------------

       call READAR(grades, count)
       print *, "" !add a new line for aesthetics

       print *, "Original Array"
       call WRITEAR(grades, count)

       call SHELL(grades, count)

       print *, "Sorted Array"
       call WRITEAR(grades, count)

     
       print *, "The minimum is", grades(1)
       print *, "The maximum is", grades(count)
       print *, "The range is", grades(count) - grades(1)
       print *, "The mean is", Mean(grades, count)
       print *, "The median is", Median(grades, count)

       value_variance = Variance(grades, count)
       print *, "The variance is", value_variance
       print *, "The standard deviation is", value_variance ** 0.5

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




       ! ====================================== SHELL Subroutine ============================================

       ! sorts the array in ascending order


       Subroutine SHELL (array, size)
       ! -----------------------Declaration-----------------------
       integer, intent(in) :: size
       real, intent(inout) :: array(1:size)
       integer :: sequence(1:5)
       integer :: gap
       integer :: temp
       ! ----------------------------------------------------------

       
       !---Error checking/check for small sized array---
       if(size == 0 .or. size == 1) then
       		go to 5

       else if (size == 2) then
       		if (array(2) < array(1)) then
       			temp = array(1)
       			array(1) = array(2)
       			array(2) = temp
       		end if
       		go to 5
       end if
       !-----------------------------------------------


       sequence = (/57, 23, 10, 4, 1/) !Marcin Ciura optimal shell sort gap sequence

       !i --> count of iteratation through all the gap sizes in the above sequence
       !j --> the jth element in array. It also represents the 1st element in each gapped sub-array
       !k --> the number of gaps away we are from element j
       !l --> the index of the current element of interest
       !m --> the number of gaps back from element array(l)

       !-------------------------------Iterate through all gap sizes---------------------------------------
       do i = 1, 5
       	gap = sequence(i)

       	 !-----------------------Only use gaps that are smaller than array size----------------------------
 1       if (gap < size) then
       		do j = 1, size

       		!--------Iterate through gaps starting at jth element---------------
       			k = 1
 2       		if (j + k*gap <= size) then
       				l = j + k*gap

       				m = 1
       				!----compare array(l) to element that is m gaps back------

       				!we can only go back at most k gaps since that will bring
       				!us to element j which is the first element of the sub-array

 3       			if (k - m >= 0 .AND. array(l) <= array(l - m*gap)) then
       					m = m+1
       					go to 3
       				!----------------------------------------------------------	


       				!------shift the previous m-1 elements in sub-array to the right by one gap------

       				!we accomplish this shift by performing m-1 swaps 

       				else if (m > 1) then
       					do n = 2, m
       					 !--------Swap-------------
       					 temp = array(l - gap)
       					 array(l - gap) = array(l)
       					 array(l) = temp
       					 !-------------------------

       					 l = l - gap
       					end do	
       				end if
       				!-----------------------finish shifting elements----------------------------------

       				!---Increment k and repeat---
       				k = k + 1
       				go to 2
       				!----------------------------

       			end if
       			!----------------finish iteration through gaps ---------------------
       		

       		!---optimize when gap = 1 and skip rest of j loop ---	
       		if (gap == 1) then
       			go to 4
   			end if
   			!----------------finish optimization-----------------

       		end do  !end do j loop

 4      end if
 		!--------------------------------finish sort for one gap size------------------------------------

       end do
       !-------------------------------finish iterating through gap sizes--------------------------------

 5     return 
       end

       ! ====================================End SHELL Subroutine ============================================ 



       !================================= WRITEAR Subroutine =====================================

       !prints the contents of an array in 6 columns per line

       Subroutine WRITEAR(array, size)

       !------------Declarations--------------------
       integer, intent(in) :: size
       real, intent(in) :: array(1:size)
       character(len=100) :: format  !will hold string for output format with write function
       !--------------------------------------------

       !---Error checking/check for small array---
       if (size < 0) then
       		go to 1

       else if (size == 0) then
       		print *, ""
       		go to 1
       end if
       !------------------------------------------

       !---Print each array element---

 	   format = "(F10.2, T2, T2)" !real number with two spots after decimal, followed by tab
       do i = 1, size
        write (*, format, advance='no') array(i)
       	
        if (mod(i,6) == 0) then  !after the 6 entry, add a newline
       		print *, ""
        end if

       end do
       !------------------------------

       print *, "" !add a new line for aesthetics
       print *, "" !add a new line for aesthetics

1      return
       end
       !====================================== End WRITEAR =======================================



       !================== Function - Mean ======================

       !this function calculates the mean of a set of numbers

       Real function Mean(array, size)
       !-------Declarations---------------
       integer, intent(in) :: size
       real, intent(in) :: array(1:size)
       real :: sum
       !---------------------------------

       sum = 0
       do i = 1, size
       		sum = sum + array(i)
       end do

       Mean = sum/size

       return
       end
       !================ End Function - Mean ===================



       !=============== Function - Median ======================

       !this function computes the median of a set of numbers

       Real function Median(array, size)
       !------------Declarations-----------------
       integer, intent(in) :: size
       real, intent(in) :: array(1:size)
       !-----------------------------------------

       if(mod(size,2) == 0) then
       		Median = (array(size/2) + array(size/2 + 1))/2
       else
       		Median = array(size/2 + 1)
       end if

       return 
       end
       !=====================================================



       !======== Function - Variance ============

       !computes the variance of a set of numbers

       Real function Variance (array, size)
       !------------Declarations----------
       integer, intent(in) :: size
       real, intent(in) :: array(1:size)
       real :: Mean
       real :: avg
       real :: sum
       !----------------------------------

       avg = Mean(array, size)
       sum = 0

       do i= 1, size
       		sum = (array(i) - avg) ** 2
       end do

       Variance = sum/size

       return
       end
       !=========================================