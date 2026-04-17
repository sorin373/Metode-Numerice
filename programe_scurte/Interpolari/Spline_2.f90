program Spline_2
    implicit none

    integer :: n, i
    real, allocatable :: x(:), y(:), m(:), h(:), a(:)
    real :: k

    read(*,*) n

    allocate(x(n), y(n), m(n), h(n), a(n))

    read(*,*) k
    
    do i = 1, n
        read(*,*) x(i), y(i)
    end do

    do i = 1, n - 1
        if (x(i + 1) <= x(i)) then
            deallocate(x, y, m, h, a)
            stop
        end if
    end do

    do i = 1, n - 1
        h(i) = x(i + 1) - x(i)
    end do

    m(1) = (y(2) - y(1)) / (x(2) - x(1))

    do i = 2, n - 1
        m(i) = 2 * (y(i) - y(i - 1)) / h(i - 1) - m(i - 1)
    end do

    do i = 1, n - 1
        a(i) = (y(i + 1) - y(i)) / ((x(i + 1) - x(i)) ** 2) - m(i) / (x(i + 1) - x(i))
    end do

    i = 0
    do
        if (k <= x(i + 1) .and. k >= x(i)) exit
        i = i + 1
    end do

    write(*,*) y(i) + m(i) * (k - x(i)) + a(i) * ((k - x(i)) ** 2)
end program