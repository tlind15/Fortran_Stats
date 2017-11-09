Program Stats

    character(len=25) :: f
    print *,"Input name of file to read: "
    read  (*, *) f 
    print *,f
    call READAR
End Program Stats

Subroutine READAR
    integer, dimension(1:10) :: array
    do i = 1, 10
        array(i) = i + 1
    end do
    print *, array
    return
    end
