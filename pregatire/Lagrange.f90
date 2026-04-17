! f(x) = y_i * L_i(x)
! L_i(x) = (x - x_j) / (x_i - x_j) | i != j

! program Lagrange
!     implicit none

!     integer :: n, i, j
!     real, allocatable :: x(:), y(:), L(:), f(:)
!     real :: k, S

!     read(*,*) n

!     allocate(x(n), y(n), L(n), f(n))

!     read(*,*) k

!     do i = 1, n
!         read(*,*) x(i), y(i)
!     end do

!     S = 0

!     do i = 1, n
!         L(i) = 1
        
!         do j = 1, n
!             if (i /= j) then 
!                 L(i) = L(i) * (k - x(j)) / (x(i) - x(j))
!             endif
!         end do

!         f(i) = L(i) * y(i)
!         S = S + f(i)
!     end do

!     write(*,*) S

!     deallocate(x, y, L, f)
! end program

! program Newton
!     implicit none

!     integer :: n, i, j
!     real, allocatable :: x(:), y(:), m(:,:)
!     real :: k, p, rez
    
!     read(*,*) n

!     allocate(x(n), y(n))
!     allocate(m(n,n))

!     read(*,*) k

!     do i = 1, n
!         read(*,*) x(i), y(i)
!     end do

!     do i = 1, n
!         do j = i + 1, n
!             if (x(i) == x(j)) then
!                 deallocate(x, y, m)
!                 stop 
!             end if
!         end do
!     end do

!     do i = 1, n
!         m(i, 1) = y(i)
!     end do

!     do i = 2, n
!         do j = 1, n - i + 1
!             m(j, i) = (m(j + 1, i - 1) - m(j, i - 1)) / (x(i + j - 1) - x(j))            
!         end do
!     end do

!     rez = m(1, 1)

!     do i = 2, n
!         p = 1

!         do j = 1, i - 1
!             p = p * (k - x(j))
!         end do

!         rez = rez + m(1, i) * p
!     end do

!     write(*,*) rez

!     deallocate(x, y, m)
! end program

! program Spline_1
!     implicit none

!     integer :: n, i, j
!     real, allocatable :: x(:), y(:), m(:)
!     real :: k

!     read(*,*) n

!     allocate(x(n), y(n), m(n))
    
!     read(*,*) k

!     do i = 1, n
!         read(*,*) x(i), y(i)
!     end do

!     do i = 1, n - 1
!         do j = i + 1, n
!             if (x(j) <= x(i)) then
!                 deallocate(x, y, m)
!                 stop
!             endif
!         end do
!     end do

!     do i = 1, n - 1
!         m(i) = (y(i + 1) - y(i)) / (x(i + 1) - x(i))
!     end do

!     i = 1
!     do
!         if ((k <= x(i + 1) .and. k >= x(i)) .or. i == n - 1) exit
!         i = i + 1
!     end do

!     write(*,*) y(i) + m(i) * (k - x(i))
! end program

! program Spline_2
!     implicit none

!     integer :: n, i, j
!     real, allocatable :: x(:), y(:), m(:), h(:), a(:)
!     real :: k

!     read(*,*) n

!     allocate(x(n), y(n), m(n), h(n), a(n))

!     read(*,*) k
    
!     do i = 1, n
!         read(*,*) x(i), y(i)
!     end do

!     do i = 1, n - 1
!         do j = i + 1, n
!             if (x(j) <= x(i)) then
!                 deallocate(x, y, m, h, a)
!                 stop
!             end if
!         end do
!     end do

!     do i = 1, n - 1
!         h(i) = x(i + 1) - x(i)
!     end do

!     m(1) = (y(2) - y(1)) / (x(2) - x(1))

!     do i = 2, n - 1
!         m(i) = 2 * (y(i) - y(i - 1)) / h(i - 1) - m(i - 1)
!     end do

!     do i = 1, n - 1
!         a(i) = (y(i + 1) - y(i)) / ((x(i + 1) - x(i)) ** 2) - m(i) / (x(i + 1) - x(i))
!     end do

!     i = 0
!     do
!         if (k <= x(i + 1) .and. k >= x(i)) exit
!         i = i + 1
!     end do

!     write(*,*) y(i) + m(i) * (k - x(i)) + a(i) * ((k - x(i)) ** 2)
! end program

! program derivare
!     implicit none

!     integer :: i
!     real :: h, x(-50:50), y(-50:50)
!     real :: DD, DS, DC1, DC2, fp, fs

!     read(*,*) h
!     read(*,*) x(0)

!     do i = -50, 50
!         x(i) = x(0) + i * h
!         y(i) = exp(x(i) ** 2) - x(i) - 1
!     end do

!     DD = (y(1) - y(0)) / h
!     DS = (y(0) - y(-1)) / h
!     DC1 = (y(1) - y(-1)) / (2 * h)

!     DC2 = (y(1) - 2 * y(0) + y(-1)) / (h ** 2)

!     fp = 2 * x(0) * exp(x(0) ** 2) - 1
!     fs = 2 * exp(x(0)**2) + 4 * x(0)**2 * exp(x(0)**2)

!     print *, abs((DD - fp) / fp) * 100
! end program 

! program integrare
!     implicit none

!     integer :: i, n
!     real :: a, b, h, integrala, x(50), y(50)
    
!     read(*,*) a, b
!     read(*,*) n

!     h = (b - a) / n

!     do i = 1, n + 1
!         x(i) = a + (i - 1) * h
!         y(i) = x(i) ** 4 / sqrt(1 + x(i))
!     end do

!     integrala = y(1) + y(n + 1)

!     do i = 2, n
!         integrala = integrala + 2 * y(i)
!     end do

!     integrala = integrala * h / 2

!     write(*,*) integrala
! end program

! program sisteme_liniare
!     implicit none

!     integer, parameter :: nmax = 100
!     integer :: n, i, j, max_iter, iter
!     real(8) :: A(nmax, nmax), b(nmax), x(nmax), x_old(nmax)
!     real(8) :: suma, toler, eroare

!     read(*,*) n

!     do i = 1, n
!         do j = 1, n
!             read(*,*) A(i, j)
!         end do
!     end do

!     do i = 1, n
!         read(*,*) b(i)
!     end do

!     read(*,*) toler
!     read(*,*) max_iter

!     do i = 1, n
!         if (A(i,i) == 0.d0) stop
!     end do

!     do i = 1, n
!         suma = 0

!         do j = 1, n
!             if (i /= j) suma = suma + abs(A(i, j))
!         end do

!         if (abs(A(i, i)) <= suma) stop
!     end do

!     x = 0.d0
!     x_old = 0.d0

!     do iter = 1, max_iter
!         x_old = x

!         do i = 1, n
!             suma = 0.d0

!             do j = 1, n
!                 if (i /= j) suma = suma + A(i, j) * x_old(j)
!             end do

!             x(i) = (b(i) - suma) / A(i, i)
!         end do

!         eroare = maxval(abs(x - x_old))
!         if (eroare < toler) exit
!     end do

!     if (eroare < toler) print *, "Metoda a convers"

!     ! solutia
!     do i = 1, n
!         print *, x(i)
!     end do
! end program

! program Newton_Rhapson
!     implicit none

!     real :: x, x_old, toler, f, fp
!     integer :: iter
!     integer, parameter :: iter_max = 100

!     iter = 1
    
!     read(*,*) toler
!     read(*,*) x_old

!     do while(iter < iter_max)
!         f = log(x_old) - 1 + sin(x_old - 2.71)
!         fp = 1 / x_old + cos(x_old - 2.71)

!         if (fp < toler) exit

!         x = x_old - f / fp

!         if (abs(x - x_old) < toler) then
!             write(*,*) "Rez: ", x
!             exit
!         endif

!         x_old = x
!         iter = iter + 1
!     end do
! end program