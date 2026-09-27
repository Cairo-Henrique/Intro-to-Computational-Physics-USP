      program tarefa1d

      integer*4 N
      real*8 x, somatorio_fx, epsilon, integral_i, integral_ii
      parameter(epsilon = (10d0)**(-3)) ! precisão

      real*8 numero_aleatorio_entre, funcao_i, funcao_ii ! funções
      real*8 a_i, b_i, a_ii, b_ii ! Intervalos de integração
      real*8 integral_i_real, integral_ii_real ! Valores reais das integrais

      integer*4 iseed
      parameter(iseed = 42)
      call srand(iseed)

      ! Integral (i)
      print *, 'Resultados para a integral (i):'
      a_i = 0d0
      b_i = 1d0
      integral_i_real = 3.14159265358979323 ! pi
      somatorio_fx = 0
      do N = 1, 10**8
        x = numero_aleatorio_entre(a_i, b_i)
        somatorio_fx = somatorio_fx + funcao_i(x) ! Para calcular a média
        integral_i = (b_i - a_i) * somatorio_fx / N ! Teorema do Valor Médio
        ! Quando o erro é menor que epsilon, mostra os resultados finais
        if (abs(integral_i - integral_i_real) .LT. epsilon) then
            print *, 'O numero de pontos necessario eh', N
            print *, 'A integral estimada eh', integral_i
            goto 10
        end if
      end do

10    continue

      ! Integral (ii)
      print *, 'Resultados para a integral (ii):'
      a_ii = 1d0
      b_ii = 10d0
      integral_ii_real = log(10d0) ! ln(10)
      somatorio_fx = 0
      do N = 1, 10**8
        x = numero_aleatorio_entre(a_ii, b_ii)
        somatorio_fx = somatorio_fx + funcao_ii(x)
        integral_ii = (b_ii - a_ii) * somatorio_fx / N
        if (abs(integral_ii - integral_ii_real) .LT. epsilon) then
            print *, 'O numero de pontos necessario eh', N
            print *, 'A integral estimada eh', integral_ii
            goto 20
        end if
      end do

20    continue

      end

      function numero_aleatorio_entre(inicio, fim)
      ! Gera um numero aleatorio uniformemente distribuido entre inicio e fim.
      ! Entrada: inicio e fim - limites inferior e superior do intervalo.
      ! Saida: numero_aleatorio_entre - numero aleatorio gerado no intervalo.
      real*8 inicio, fim, numero_aleatorio_entre

      numero_aleatorio_entre = (fim - inicio) * rand() + inicio ! bijecao linear entre o intervalo [0,1) e o intervalo [inicio, fim]
      
      return
      end

      function funcao_i(x)
      ! f(x) = 4 / (x^2 + 1)
      real*8 x, funcao_i
      funcao_i = 4d0 / (x**2 + 1d0)
      return
      end

      function funcao_ii(x)
      ! f(x) = 1 / x
      real*8 x, funcao_ii
      funcao_ii = 1d0 / x
      return
      end