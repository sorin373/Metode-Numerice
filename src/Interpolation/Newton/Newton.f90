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

module newton_module
    implicit none

    real, allocatable :: x(:), y(:)
    real, allocatable :: memo(:,:)  ! folosesc matricea 'memo' sa stochez diferentele pentru a reduce timpul petrecut in recursivitate
    logical, allocatable :: computed(:,:)   

contains
    recursive function DD(i, j) result(ret)
        integer, intent(in) :: i, j
        real :: ret

        if (computed(i, j)) then
            ret = memo(i, j)
            return
        end if

        if (i == j) then
            ret = y(i)
        else
            ret = (DD(i, j + 1) - DD(i - 1, j)) / (x(i) - x(j))
        end if

        memo(i, j) = ret
        computed(i, j) = .true.
    end function DD

end module newton_module

program interpolare_newton
    use newton_module
    implicit none

    integer :: n, i, j
    real :: xp, rez, term

    write(*,*) "Introduceti necunoscuta:"
    read(*,*) xp

    write(*,*) "Numar puncte :"
    read(*,*) n

    if (n < 2) then
        write(*,*) "n trebuie sa fie mai mare ca 2."
        stop
    end if

    allocate(x(n), y(n))
    allocate(memo(n, n), computed(n, n))
    computed = .false.

    do i = 1, n
        write(*,*) "x(", i, ") = "
        read(*,*) x(i)

        write(*,*) "y(", i, ") = "
        read(*,*) y(i)
    end do

    do i = 1, n
        do j = i + 1, n
            if (x(i) == x(j)) then
                write(*,*) "Exista val. x egale -> impartire la 0"
                deallocate(x, y, memo, computed)
                stop
            end if
        end do
    end do

    rez = 0.0

    do i = 1, n
        term = DD(i, 1)
        do j = 1, i - 1
            term = term * (xp - x(j))
        end do
        rez = rez + term
    end do

    write(*,*) "Val. interpolata =", rez

    deallocate(x, y, memo, computed)

end program interpolare_newton