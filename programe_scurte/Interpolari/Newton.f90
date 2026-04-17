program Newton
    implicit none

    integer :: n, i, j
    real :: x(100), y(100), m(100,100), k, rez, p

    read(*,*) n, k

    do i = 1, n
        read(*,*) x(i), y(i)
    end do

    do i = 1, n - 1
        if (x(i+1) <= x(i)) stop
    end do

    do i = 1, n
        m(i, 1) = y(i)
    end do

    do i = 2, n
        do j = 1, n - i + 1
            m(j, i) = (m(j + 1, i - 1) - m(j, i - 1)) / (x(i + j - 1) - x(j))
        end do
    end do

    rez = m(1, 1)

    do i = 2, n
        p = 1

        do j = 1, i - 1
            p = p * (k - x(j))
        end do

        rez = rez + p * m(1, i)
    end do

    write(*,*) rez
end program