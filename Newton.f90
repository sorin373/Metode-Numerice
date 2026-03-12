module varr
    implicit none
    real :: x(100), y(100)

contains

recursive function dif(start, finish) result(ret)

    integer, intent(in) :: start, finish
    real :: ret

    if (start - finish .eq. 1) then
        ret = (y(start) - y(finish)) / (x(start) - x(finish))
    else
        ret = (dif(start, finish + 1) - dif(start - 1, finish)) / (x(start) - x(finish));
    endif
end function

end module

program interpolare_Newton
    use varr
    implicit none
    
    real :: var, sum, a
    integer :: n, i, j

    sum = 0

    write(*,*) "var: "
    read(*,*) var

    write(*,*) "n: "
    read(*,*) n

    do i = 1, n
        write(*,*) "x: "
        read(*,*) x(i)
 
        write(*,*) "y: "
        read(*,*) y(i)
    end do 

    sum = y(1)
    do i = 1, n - 1
        a = dif(i + 1, 1)

        do j = 1, i
            a = a * (var - x(j))
        end do

        sum = sum + a
    end do

    write(*,*) sum
end program