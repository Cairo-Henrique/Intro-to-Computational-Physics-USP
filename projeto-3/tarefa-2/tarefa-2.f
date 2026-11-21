      program tarefa2
      ! Quadratura numérica

      implicit none

      integer*4 i
      real*8 h, pi
      real*8 valor_exato, a, b
      real*8 trapezio, simpson, boole ! funções
      real*8 valor_trapezio, valor_simpson, valor_boole ! valores das funções

      parameter(pi = 3.14159265358979323d0)

      ! Intervalo de integração
      a = 0d0
      b = 2d0 * pi

      valor_exato = 0.5d0 -  0.5d0 * exp(-2d0*pi) ! valor exato da integral

      open(unit=10, file='tarefa-2-saida-1.dat')

      do i = 2,13

        h = 2d0 * pi * ((2d0)**(-i)) ! h = dx

        valor_trapezio = trapezio(h,a,b)
        valor_simpson = simpson(h,a,b)
        valor_boole = boole(h,a,b)

        ! printa em uma linha os valores
        print *, valor_trapezio, valor_simpson, valor_boole

        ! coloca em uma linha os valores no arquivo
        write(10, *) valor_trapezio, valor_simpson, valor_boole
      end do

      ! printa os valores exatos na linha final
      print '(6(f20.11))', valor_exato, valor_exato, valor_exato

      ! coloca os valores exatos na linha final do arquivo
      write(10, '(6(f20.11))') valor_exato, valor_exato, valor_exato

      close(10)

      end program

      ! FUNÇÕES

      function f(x)
      real*8 x, f
      f = exp(-x) * cos(x)
      return
      end function

      function trapezio(h,a,b)
      ! Regra do trapezio para aproximar a integral
      ! de f(x) no intervalo [a,b], dividindo-o em subintervalos
      ! de largura h.
      !
      ! Entrada:
      !   h = largura dos subintervalos
      !   a = limite inferior de integracao
      !   b = limite superior de integracao
      !
      ! Saida:
      !   trapezio = aproximacao numerica da integral de f(x)
      !              no intervalo [a,b].
      integer*4 i, N
      real*8 f, h, a, b, trapezio
      N = int((b-a)/h + 1d0)
      trapezio = 0d0
      do i = 0, N-1
        trapezio = trapezio + (h/2d0) * (f(a + i*h) + f(a + (i+1)*h))
      end do
      return
      end function

      function simpson(h,a,b)
      ! Regra de Simpson para aproximar a integral
      ! de f(x) no intervalo [a,b], utilizando blocos de dois
      ! subintervalos de largura h.
      !
      ! Entrada:
      !   h = largura dos subintervalos
      !   a = limite inferior de integracao
      !   b = limite superior de integracao
      !
      ! Saida:
      !   simpson = aproximacao numerica da integral de f(x)
      !              no intervalo [a,b].
      !
      ! Observacao:
      !   O numero de subintervalos N deve ser multiplo de 2.
      integer*4 i, N
      real*8 f, h, a, b, simpson
      N = int((b-a)/h)
      simpson = 0d0
      do i = 0, ((N/2)-1) ! somente intervalos múltiplos de 2
        simpson = simpson + (h/3d0) *
     &  (f(a + 2*i*h)
     &  + 4d0*f(a + (2*i+1)*h)
     &  + f(a + (2*i+2)*h))
      end do
      return
      end function

      function boole(h,a,b)
      ! Regra de Boole para aproximar a integral
      ! de f(x) no intervalo [a,b], utilizando blocos de quatro
      ! subintervalos de largura h.
      !
      ! Entrada:
      !   h = largura dos subintervalos
      !   a = limite inferior de integracao
      !   b = limite superior de integracao
      !
      ! Saida:
      !   boole = aproximacao numerica da integral de f(x)
      !           no intervalo [a,b].
      !
      ! Observacao:
      !   O numero de subintervalos N deve ser multiplo de 4.
      integer*4 i, N
      real*8 f, h, a, b, boole
      N = int((b-a)/h)
      boole = 0d0
      do i = 0, N/4-1 ! somente intervalos múltiplos de 4
        boole = boole + (2d0*h/45d0) *
     &  (7d0*f(a + 4*i*h)
     &  + 32d0*f(a + (4*i+1)*h)
     &  + 12d0*f(a + (4*i+2)*h)
     &  + 32d0*f(a + (4*i+3)*h)
     &  + 7d0*f(a + (4*i+4)*h))
      end do
      return
      end function