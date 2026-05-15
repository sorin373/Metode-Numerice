program odemid
    implicit none

    real *8 :: f, yex, y0 = 2.d0/3.d0, a = 0.d0, b = 1.d0, y_interm, h
    integer, parameter :: nmax = 50
    real *8, dimension(0:nmax) :: x, yM,yexact
    integer :: i, ios

    open(unit = 1, file = "edo.out", iostat = ios)

    !generare grila

    h = (b-a)/nmax

    ! initializare vectori

    do i = 0, nmax
        x(i) = a + h*i
        yexact(i) = yex(x(i)) !pt ca am declarat functia
    end do

    !!metoda midpoint

    yM(0) = y0

    do i = 0, nmax-1
        !pasul de predictie
        y_interm = yM(i) + h/2.d0 * f(x(i), yM(i))
        !corectie
        yM(i+1) = yM(i) + h * f(x(i)+h/2.d0, y_interm)
    end do

    !afisare

    do i = 0, nmax
        write(1,*) x(i), yM(i), yexact(i)
    end do
    
    close(1)
end program

function f(x, y)
    implicit none
    real *8 :: x, y, f
    f = y - x
end function

function yex(x)
    implicit none
    real *8 :: x, yex
    yex = 1.d0 + x - 1.d0/3.d0 * dexp(x)
end function