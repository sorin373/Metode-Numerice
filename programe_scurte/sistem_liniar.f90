program sisteme_liniare
    implicit none

    integer, parameter :: nmax = 100
    integer :: n, i, j, max_iter, iter
    real(8) :: A(nmax, nmax), b(nmax), x(nmax), x_old(nmax)
    real(8) :: suma, toler, eroare

    read(*,*) n

    do i = 1, n
        do j = 1, n
            read(*,*) A(i, j)
        end do
    end do

    do i = 1, n
        read(*,*) b(i)
    end do

    read(*,*) toler
    read(*,*) max_iter

    do i = 1, n
        if (A(i,i) == 0.d0) stop
    end do

    do i = 1, n
        suma = 0

        do j = 1, n
            if (i /= j) suma = suma + abs(A(i, j))
        end do

        if (abs(A(i, i)) <= suma) stop
    end do

    x = 0.d0
    x_old = 0.d0

    do iter = 1, max_iter
        x_old = x

        do i = 1, n
            suma = 0.d0

            do j = 1, n
                if (i /= j) suma = suma + A(i, j) * x_old(j)
            end do

            x(i) = (b(i) - suma) / A(i, i)
        end do

        eroare = maxval(abs(x - x_old))
        if (eroare < toler) exit
    end do

    if (eroare < toler) print *, "Metoda a convers"

    do i = 1, n
        print *, x(i)
    end do
end program