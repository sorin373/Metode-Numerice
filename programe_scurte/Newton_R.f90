program Newton_Rhapson
    implicit none

    real :: x, x_old, toler, f, fp
    integer :: iter
    integer, parameter :: iter_max = 100

    iter = 1
    
    read(*,*) toler
    read(*,*) x_old

    do while(iter < iter_max)
        f = log(x_old) - 1 + sin(x_old - 2.71)
        fp = 1 / x_old + cos(x_old - 2.71)

        if (fp < toler) exit

        x = x_old - f / fp

        if (abs(x - x_old) < toler) then
            write(*,*) "Rez: ", x
            exit
        endif

        x_old = x
        iter = iter + 1
    end do
end program