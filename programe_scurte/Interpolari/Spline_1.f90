program Spline_1
    implicit none

    integer :: n, i
    real, allocatable :: x(:), y(:), m(:)
    real :: k

    read(*,*) n

    allocate(x(n), y(n), m(n))
    
    read(*,*) k

    do i = 1, n
        read(*,*) x(i), y(i)
    end do

    do i = 1, n - 1
        if (x(i + 1) <= x(i)) then
            deallocate(x, y, m)
            stop
        endif
    end do

    do i = 1, n - 1
        m(i) = (y(i + 1) - y(i)) / (x(i + 1) - x(i))
    end do

    i = 1
    do
        if ((k <= x(i + 1) .and. k >= x(i)) .or. i == n - 1) exit
        i = i + 1
    end do

    write(*,*) y(i) + m(i) * (k - x(i))
end program