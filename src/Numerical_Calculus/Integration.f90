program integrare
    implicit none

    integer, parameter :: nmax = 50
    integer :: i, n
    real :: a, b, h, integrala, x(nmax), y(nmax)

    print *, "lim inf: "
    read *, a

    print *, "lim sup:"
    read *, b

    print *, "n = "
    read *, n

    h = (b - a) / n ! calcul pas

    do i = 1, n + 1
        x(i) = a + (i - 1) * h
        y(i) = x(i) ** 4 / sqrt(1 + x(i))
    end do

    integrala = y(1) + y(n + 1)

    do i = 2, n
        integrala = integrala + 2 * y(i)
    end do

    integrala = (h / 2) * integrala

    print *, "val aprox = ", integrala

    print *, "_________________"

    do i = 1, n + 1
        print *, "x(",i,")=", x(i), "y(",i,")=", y(i)
    end do
end program