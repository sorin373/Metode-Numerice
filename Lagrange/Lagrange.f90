!!!Program interpolare lagrange

program lab1lagr
    implicit none

    !declararea

    integer :: n, i, j

    real :: x(20), y(20), L(20), f(20)
    real :: suma, k

    !citire

    print *, "Numarul de puncte n=" !write(*,*) item1, item2, ...

    read *, n!read(*,*) item 1, item2, ...

    print *, "introduceti valoarea necunoscutei "

    read *, k

    !citire vectori

    do i = 1, n
        print *, "x = "
        read *, x(i)
        print *, "y = "
        read *, y(i)
    end do

    !! cond initiale

    suma = 0
    do i = 1, n
        f(i) = 0
    end do

    ! bucla repetitiva

    do i = 1, n
        L(i) = 1
        do j = 1, n
            if (i /= j) then
                L(i) = L(i) * (k-x(j))/(x(i)- x(j))
            end if
        end do

        f(i) = L(i)*y(i)
        suma = suma + f(i)

    end do

    ! afisare

    do i = 1,n
        print *, "polinomul k = ", f(i)
    end do

    print *, "raspunsul pentrul variabila de interpolare este: ", suma

end program