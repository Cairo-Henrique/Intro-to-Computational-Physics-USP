      program tarefa1c
      ! Calcular pi utilizando o método de Monte Carlo em um círculo unitário.
      ! Investigamos o numero N_total de pontos necessário para obter precisão de 4 casas decimais.

      integer*4 N_dentro, N_total
      real*8 x, y, numero_aleatorio_entre, epsilon
      parameter(epsilon = (10d0)**(-4)) ! precisão

      real*8 pi_mc, pi_real
      parameter(pi_real = 3.14159265358979323) ! valor real para calcular a precisão

      integer*4 iseed
      parameter(iseed = 42)
      call srand(iseed)

      ! loop para sortear N_total pontos até achar o valor bom de pi
      N_dentro = 0
      do N_total = 1, 10**8

        x = numero_aleatorio_entre(-1d0, 1d0)
        y = numero_aleatorio_entre(-1d0, 1d0)

        if (x**2 + y**2 .LT. 1d0) then ! (x,y) pertencente ao círculo de raio 1
            N_dentro = N_dentro + 1
        end if
        
        pi_mc = 4d0 * N_dentro / N_total ! A = pi r^2 = proporcao em relacao ao quadrado. Temos r = 1 e area do quadrado = 4
        
        ! Se o erro for menor que epsilon, mostra os resultados finais
        if (abs(pi_mc - pi_real) .LT. epsilon) then
            print *, 'O numero de pontos necessario eh', N_total
            print *, 'O valor de pi estimado eh', pi_mc
            stop
        end if

      end do

      end

      function numero_aleatorio_entre(inicio, fim)
      ! Gera um numero aleatorio uniformemente distribuido entre inicio e fim.
      ! Entrada: inicio e fim - limites inferior e superior do intervalo.
      ! Saida: numero_aleatorio_entre - numero aleatorio gerado no intervalo.
      real*8 inicio, fim, numero_aleatorio_entre
      
      numero_aleatorio_entre = (fim - inicio) * rand() + inicio ! bijecao linear entre o intervalo [0,1) e o intervalo [inicio, fim]
      
      return
      end