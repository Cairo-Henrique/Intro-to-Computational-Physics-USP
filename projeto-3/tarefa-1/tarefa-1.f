      program tarefa1
      ! Derivação numérica

      implicit none

      integer*4 i
      real*8 x, h
      real*8 D_f, D2_f, f1_2f, f1_2t, f1_3s, 
     & f1_5s, f2_3s, f2_5s ! funções
      real*8 valor_D_f, valor_D2_f, 
     & valor_f1_2f, valor_f1_2t, valor_f1_3s, 
     & valor_f1_5s, valor_f2_3s, valor_f2_5s ! valores das funções

      parameter(x = 0.1d0)

      open(unit=10, file='tarefa-1-saida-1.dat')

      do i = 1,12

        h = (5d0)**(-i) ! h = dx

        valor_f1_2f = f1_2f(x,h)
        valor_f1_2t = f1_2t(x,h)
        valor_f1_3s = f1_3s(x,h)
        valor_f1_5s = f1_5s(x,h)
        valor_f2_3s = f2_3s(x,h)
        valor_f2_5s = f2_5s(x,h)

        ! printa em uma linha os valores
        print *, valor_f1_2f, valor_f1_2t, valor_f1_3s, valor_f1_5s, 
     &  valor_f2_3s, valor_f2_5s

        ! coloca em uma linha os valores no arquivo
        write(10, *) valor_f1_2f, valor_f1_2t, valor_f1_3s,
     &  valor_f1_5s, valor_f2_3s, valor_f2_5s
      end do

      valor_D_f = D_f(x)
      valor_D2_f = D2_f(x)

      ! printa os valores exatos na linha final
      ! '(6(f20.11))' significa que serão impressos 6 valores de ponto flutuante com 20 dígitos no total e 11 dígitos após o ponto decimal
      print '(6(f20.11))', valor_D_f, valor_D_f,
     &  valor_D_f, valor_D_f,
     &  valor_D2_f, valor_D2_f

      ! coloca os valores exatos na linha final do arquivo
      write(10, '(6(f20.11))') valor_D_f, valor_D_f,
     &  valor_D_f, valor_D_f,
     &  valor_D2_f, valor_D2_f

      close(10)

      end program

      ! FUNÇÕES

      function f(x)
      real*8 x, f
      f = exp(x**2) / tan(2*x)
      return
      end function

      function D_f(x)
      real*8 x, D_f
      D_f = exp(x**2) * (x * sin(4*x) - 2) / (sin(2*x)**2)
      return
      end function

      function D2_f(x)
      real*8 x, D2_f
      D2_f =
     & ((4.0d0*x**2+2.0d0)*exp(x**2)
     & *cos(2.0d0*x)*sin(2.0d0*x)**2
     & -8.0d0*exp(x**2)*x*sin(2.0d0*x)
     & +8.0d0*exp(x**2)*cos(2.0d0*x))
     & /(sin(2.0d0*x)**3)
      return
      end

      function f1_2f(x,h)
      real*8 x,h,f1_2f,f
      f1_2f = (f(x+h)-f(x))/h
      return
      end

      function f1_2t(x,h)
      real*8 x,h,f1_2t,f
      f1_2t = (f(x)-f(x-h))/h
      return
      end

      function f1_3s(x,h)
      real*8 x,h,f1_3s,f
      f1_3s = (f(x+h)-f(x-h))/(2.0d0*h)
      return
      end

      function f1_5s(x,h)
      real*8 x,h,f1_5s,f
      f1_5s = (-f(x+2.0d0*h)+8.0d0*f(x+h)
     &       -8.0d0*f(x-h)+f(x-2.0d0*h))
     &       /(12.0d0*h)
      return
      end

      function f2_3s(x,h)
      real*8 x,h,f2_3s,f
      f2_3s = (f(x+h)-2.0d0*f(x)+f(x-h))
     &        /(h**2)
      return
      end

      function f2_5s(x,h)
      real*8 x,h,f2_5s,f
      f2_5s = (-f(x+2.0d0*h)+16.0d0*f(x+h)
     &         -30.0d0*f(x)+16.0d0*f(x-h)
     &         -f(x-2.0d0*h))
     &         /(12.0d0*h**2)
      return
      end