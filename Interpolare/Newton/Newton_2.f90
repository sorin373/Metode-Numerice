!=====================================================================================
! Interpolare Polinomiala Newton
! Calculeaza valoarea aproximativa a unei functii intr-un punct (var)
! folosind metoda diferentelor divizate a lui Newton pe un set de n puncte.
!
! var: 3.5
! n: 5
!
! 1 7
! 2 5
! 3 9
! 4 11
! 5 13
!
! val. interpolata: 10.3593750
!=====================================================================================

program interpolare_newton
    implicit none
    
    real :: var, rez, c
    real, allocatable :: m(:,:), x(:), y(:)
    integer :: i, j, n, ios

    c = 1

    open(unit = 1, file = "data.in", iostat = ios, action="read")

    if (ios /= 0) then
        write(*,*) "Eroare la deschiderea fisierului!"
        stop
    end if

    write(*,*) "var: "
    read(*,*) var

    read(1,*) n

    if (n < 2) then
        write(*,*) "n trebuie sa fie mai mare ca 2."
        stop
    end if

    allocate(m(n, n))
    allocate(x(n), y(n))

    do i = 1, n
        read(1, *) x(i), y(i)
    end do
    
    do i = 1, n
        do j = i + 1, n
            if (x(i) .eq. x(j)) then
                write(*,*) "Exista val. x egale -> impartire la 0"
                deallocate(x, y, m)
                stop
            end if
        end do
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
        c = 1
        
        do j = 1, i - 1
            c = c * (var - x(j))
        end do

        rez = rez + m(1, i) * c
    end do

    write(*, *) "val. interpolata: ", rez

    deallocate(m, x, y)

    close(1)
end program 