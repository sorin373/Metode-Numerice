program Lagrange
    implicit none

    integer :: n, i, j
    real :: x(100), y(100), L(100)
    real :: k, suma

    read(*,*) n, k

    do i = 1, n
        read(*,*) x(i), y(i)
    end do

    suma = 0

    do i = 1, n
        L(i) = 1

        do j = 1, n
            if (i /= j) L(i) = (k - x(j)) / (x(i) - x(j))
        end do

        suma = suma + L(i) * y(i)
    end do

    write(*,*) suma
end program