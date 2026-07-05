program Lagrange
    implicit none

    integer :: n, i, j, ios
    real, allocatable :: x(:), y(:), L(:), f(:)
    real :: suma, k

    open(unit = 1, file = "data.in", iostat = ios, action="read")

    if (ios /= 0) then
        write(*,*) "Eroare la deschiderea fisierului!"
        stop
    end if

    read(1,*) n

    allocate(x(n), y(n), L(n), f(n))

    write(*,*) "k: "
    read(*,*) k

    do i = 1, n
        read(1,*) x(i), y(i)
    end do

    suma = 0
    do i = 1, n
        f(i) = 0
    end do

    do i = 1, n
        L(i) = 1
        do j = 1, n
            if (i /= j) then
                L(i) = L(i) * (k - x(j)) / (x(i) - x(j))
            end if
        end do

        f(i) = L(i) * y(i)
        suma = suma + f(i)
    end do

    print *, "var. interpolata: ", suma

    deallocate(x, y, L, f)

    close(1)
end program