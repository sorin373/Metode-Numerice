program spline_ord_1
    implicit none

    integer, parameter :: N_GEN = 10000
    integer :: i, j, n, ios
    real(kind=8) :: pas, x_gen(N_GEN), y_gen(N_GEN)
    real(kind=8), allocatable :: x(:), y(:), m(:)

    open(unit = 1, file = "./data/input/intrados.in", iostat = ios, action="read")

    if (ios /= 0) then
        write(*,*) "Eroare la deschiderea fisierului .in!"
        stop
    end if

    read(1, *) n

    if (n < 2) then
        write(*,*) "n trebuie sa fie mai mare ca 2."
        stop
    end if

    allocate(x(n), y(n), m(n - 1))

    do i = 1, n
        read(1,*) x(i), y(i)
    end do

    close(1)

    do i = 1, n - 1
        if (x(i + 1) <= x(i)) then
            write(*,*) "Val. pentru x trebuie sa fie in ord. strict crescatoare!"
            stop
        end if
    end do

    do i = 1, n - 1
        m(i) = (y(i + 1) - y(i)) / (x(i + 1) - x(i)) 
    end do

    pas = (x(n) - x(1)) / real(N_GEN - 1)

    ! Generare a 'N_GEN' x din intervalul [x(1), x(n)]
    do i = 2, N_GEN
        x_gen(i) = x(1) + real(i - 1) * pas
    end do
    
    ! Interpolez valorile y pentru punctele x generate
    do i = 1, N_GEN
        do j = 1, n - 1
            if (x_gen(i) <= x(j+1) .and. x_gen(i) >= x(j)) then
                y_gen(i) = y(j) + m(j) * (x_gen(i) - x(j))
                exit
            end if
        end do
    end do

    open(unit=2, file="./data/output/intrados.out", iostat=ios ,status="replace")

    if (ios /= 0) then
        write(*,*) "Eroare la deschiderea fisierului .out!"
        stop
    end if

    do i = 1, N_GEN
        write(2, '(F10.6,1X,F12.6)') x_gen(i), y_gen(i)
    end do

    close(2)

    deallocate(x, y, m)
end program