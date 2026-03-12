program spline_1
    implicit none

    integer, parameter :: n = 4
    real :: x(n), y(n), m(n - 1), var
    integer :: i

    write(*,*) "var = "
    read(*,*) var

    do i = 1, n
        write(*,*) "x = "
        read(*,*) x(i)
        
        write(*,*) "y = "
        read(*,*) y(i)
    end do

    do i = 1, n - 1
        m(i) = (y(i + 1) - y(i)) / (x(i + 1) - x(i)) 
    end do

    i = 0
    do
        if(var <= x(i + 1) .or. i .eq. n - 1) exit

        i = i + 1
    end do

    write(*,*) y(i) + m(i) * (var - x(i))
end program