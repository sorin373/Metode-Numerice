program derivare
    implicit none

    integer :: i
    real :: h, x(-50:50), y(-50:50)
    real :: DD, DS, DC1, DC2, fp, fs

    read(*,*) h
    read(*,*) x(0)

    do i = -50, 50
        x(i) = x(0) + i * h
        y(i) = exp(x(i) ** 2) - x(i) - 1
    end do

    DD = (y(1) - y(0)) / h
    DS = (y(0) - y(-1)) / h
    DC1 = (y(1) - y(-1)) / (2 * h)

    DC2 = (y(1) - 2 * y(0) + y(-1)) / (h ** 2)

    fp = 2 * x(0) * exp(x(0) ** 2) - 1
    fs = 2 * exp(x(0) ** 2) + 4 * x(0) ** 2 * exp(x(0) ** 2)

    write(*,*) abs((DD - fp) / fp) * 100
end program 