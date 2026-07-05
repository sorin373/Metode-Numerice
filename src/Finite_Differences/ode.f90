program ode
    implicit none

    real*8 :: a = 0.d0, b = 1.d0, y = 2.d0 / 3.d0
    real*8 :: y_mid, h, x
    real*8 :: f, y_exact
    integer, parameter :: n = 50
    integer :: i

    open(unit = 1, file="file.out")

    h = (b - a) / n

    do i = 0, n
        x = a + i * h

        write(1,*) x, y, y_exact(x)

        if (i < n) then
            y_mid = y + h / 2.d0 * f(x, y)
            y = y + h * f(x + h / 2.d0, y_mid)
        endif
    end do

    close(1)
end program

function f(x, y)
    implicit none
    real*8 :: x, y, f
    f = y - x
end function

function y_exact(x)
    implicit none
    real*8 :: x, y_exact
    y_exact = 1.d0 + x - 1.d0 / 3.d0 * dexp(x)
end function