program spline_1
    implicit none

    real, allocatable :: x(:), y(:), m(:)
    real :: var
    integer :: i, n, ios

    open(unit = 1, file = "data.in", iostat = ios, action="read")

    if (ios /= 0) then
        write(*,*) "Eroare la deschiderea fisierului!"
        stop
    end if

    read(1, *) n

    if (n < 2) then
        write(*,*) "n trebuie sa fie mai mare ca 2."
        stop
    end if

    allocate(x(n), y(n), m(n - 1))

    write(*,*) "var = "
    read(*,*) var

    do i = 1, n
        read(1,*) x(i), y(i)
    end do

    do i = 1, n-1
        if (x(i+1) <= x(i)) then
            write(*,*) "Val. pentru x trebuie sa fie in ord. strict crescatoare!"
            stop
        end if
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

    deallocate(x, y, m)

    close(1)
end program