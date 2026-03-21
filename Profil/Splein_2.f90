program spline_ord_2
    implicit none

    integer, parameter :: N_GEN = 10000
    integer :: n, i, ios, j
    real, allocatable :: m(:), x(:), y(:)

    real(kind=8) :: h(100), a(100)
    real(kind=8) :: x_gen(N_GEN), y_gen(N_GEN), pas

    open(unit = 1, file = "intrados.in", iostat = ios, action="read")

    if (ios /= 0) then
        write(*,*) "Eroare la deschiderea fisierului .in!"
        stop
    end if

    read(1,*) n

    allocate(m(n), x(n), y(n))

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
         h(i) = x(i + 1) - x(i)
    end do

    m(1) = (y(2) - y(1)) / (x(2) - x(1))

    do i = 1, n - 1
         a(i) = (y(i + 1)- y(i)) / ((x(i + 1) - x(i)) * (x(i + 1) - x(i))) - m(i) / (x(i + 1) - x(i))
         m(i + 1) = 2.00 * (y(i + 1) - y(i)) / h(i) - m(i)
    end do

    pas = (x(n) - x(1)) / real(N_GEN - 1)

    do i = 1, N_GEN
         x_gen(i) = x(1) + real(i - 1) * pas
    end do

    do i = 1, N_GEN
         y_gen(i) = 0.0
    end do

    do i = 1, N_GEN
         do j = 1, n - 1
	        if (x_gen(i) <= x(j+1) .and. x_gen(i) >= x(j)) then
               y_gen(i) = y(j) + m(j) * (x_gen(i) - x(j)) + a(j) * (x_gen(i) - x(j)) ** 2
               exit
            end if
         end do
    end do

    open(unit=2, file="intrados.out", iostat=ios, status="replace")

    if (ios /= 0) then
        write(*,*) "Eroare la deschiderea fisierului .out!"
        stop
    end if

    do i = 1, N_GEN
         write(2, '(F10.6,1X,F12.6)') x_gen(i), y_gen(i)
    end do

    deallocate(m, x, y)  

    close(1)
    close(2)
end program