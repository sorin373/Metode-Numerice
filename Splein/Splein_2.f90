program spline_ord_2
    implicit none

    integer :: n, i, ord, ios
    real, allocatable :: m(:), x(:), y(:)
    real :: var, rez

    real :: h(100), a(100)

    open(unit = 1, file = "data.in", iostat = ios, action="read")

    if (ios /= 0) then
        write(*,*) "Eroare la deschiderea fisierului!"
        stop
    end if

    read(1,*) n

    allocate(m(n), x(n), y(n))

    write(*,*) "var: "
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

   do i = 1, n
        h(i) = x(i + 1) - x(i)
   end do

   m(1) = (y(2) - y(1)) / (x(2) - x(1))

   do i = 2, n
        m(i) = 2 * (y(i) - y(i - 1)) / h(i - 1) - m(i - 1)
   end do

   do i = 1, n
        a(i) = (y(i + 1)- y(i)) / ((x(i + 1) - x(i)) * (x(i + 1) - x(i))) - m(i) / (x(i + 1) - x(i))
   end do

    do i = 1, n
        if (var < x(i+1) .and. var > x(i)) then
            rez = y(i) + m(i) * (var - x(i)) + a(i) * (var - x(i)) * (var - x(i))
        end if
    end do

   write(*,*) "val. interpolata: ", rez

   deallocate(m, x, y)  

   close(1)
end program