program integrare
    implicit none

    integer :: i, n
    real :: a, b, h, integrala, x(50), y(50)
    
    read(*,*) a, b
    read(*,*) n

    h = (b - a) / n

    do i = 1, n + 1
        x(i) = a + (i - 1) * h
        y(i) = x(i) ** 4 / sqrt(1 + x(i))
    end do

    integrala = y(1) + y(n + 1)

    do i = 2, n
        integrala = integrala + 2 * y(i)
    end do

    integrala = integrala * h / 2

    write(*,*) integrala
end program