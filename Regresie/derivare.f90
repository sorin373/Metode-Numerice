program derivate
    implicit none

    integer, parameter :: nmax = 50
    integer :: i
    real :: h, x(-nmax:nmax), y(-nmax:nmax)
    real :: DD, DS, DC1, DC2, fp, fs

    print *, "h = "
    read *, h

    if (h <= 0) then
        print *, "Err"
        stop
    end if

    print *, "x(0)="
    read *, x(0)

    do i = -nmax, nmax
        x(i) = x(0) + i * h
        y(i) = exp(x(i) ** 2) - x(i) - 1
    end do

    DD = (y(1) - y(0)) / h
    DS = (y(0) - y(1)) / h
    DC1 = (y(1) - y(-1)) / (2 * h)

    DC2 = (y(1) - 2 * y(0) + y(-1)) / (h ** 2)

    fp = 2 * x(0) * exp(x(0)**2) - 1
    fs = 2 * exp(x(0) ** 2) + 4 * x(0) * exp(x(0) ** 2)

    print *, "DD = ", DD
    print *, "DS = ", DS
    print *, "DC1 = ", DC1
    print *, "DC2 = ", DC2
    print *, "fp = ", fp
    print *, "fs = ", fs

    print *, abs((DD - fp) / fp) * 100
end program