program spline_2
    implicit none

    integer, parameter :: n = 4
    real :: x(n), y(n)
    real :: h(n-1), m(n-1)
    real :: c(n-1)
    real :: var, result
    integer :: i

    write(*,*) "var = "
    read(*,*) var

    do i = 1, n
        write(*,*) "x = "
        read(*,*) x(i)

        write(*,*) "y = "
        read(*,*) y(i)
    end do

    do i = 1, n-1
        h(i) = x(i+1) - x(i)
        m(i) = (y(i+1) - y(i)) / h(i)
    end do

    c(1) = 0.0

    do i = 2, n-1
        c(i) = (m(i) - m(i-1)) / h(i)
    end do

    i = 1
    do
        if (var <= x(i+1) .or. i == n-1) exit
        i = i + 1
    end do

    result = y(i) + (m(i) - c(i)*h(i))*(var - x(i)) + c(i)*(var - x(i))**2

    print *, "Spline value = ", result

end program spline_2