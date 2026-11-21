      program tarefa1b
      ! Efeito resistivo do ar em movimentos unidimensionais

      implicit none

      integer*4 i
      real*8 m, P_c, rho, A, t_max, dt
      real*8 v0, v, t, x, x0, K
      real*8 t_terminal
      real*8 valores_v(0:100000) ! vetor para armazenar velocidades

      ! Constantes
      m = 70.0d0 ! Massa do corpo em kg
      P_c = 400.0d0 ! Potência do ciclista em Watts
      rho = 1.3d0 ! densidade do meio em kg/m^3
      A = 0.3d0 ! área frontal em m^2
      t_max = 5d0 * 60.0d0 ! 5 minutos
      dt = 0.01d0 ! passo de tempo

      ! Condições iniciais
      v0 = 0.0d0 ! velocidade inicial
      x0 = 0.0d0 ! posição inicial
      v = v0
      K = 0.5d0 * m * v0**2
      x = x0
      valores_v(0) = v0

      ! Abre arquivo de saída
      open(unit=10, file='tarefa-1b-saida-2.dat')

      ! Instante de tempo em que a velocidade é terminal
      t_terminal = -1d0

      ! Loop
      do i = 0, int(t_max/dt)
            t = i * dt
            ! Printa e escreve no arquivo de saída
            print *, 't =', t, 'v =', v
            write(10,*) t, v
            if (i == int(t_max/dt)) exit

            ! Atualiza a energia cinética usando o método de Euler
            K = K + dt * (P_c -
     &      sqrt(2d0 / m**3) * rho * A * K**(3d0/2d0))

            ! Calcula a velocidade a partir da energia cinética
            v = sqrt(2d0 * K / m)

            valores_v(i+1) = v

            ! Verifica se a velocidade é terminal em t + dt
            if (t_terminal .lt. 0d0 .and.
     &      abs(valores_v(i+1) - valores_v(i)) .lt. 1d-6) then
                t_terminal = t + dt
            end if

            ! Atualiza a posição usando a Regra do Trapézio
            x = x + ((valores_v(i) + valores_v(i+1)) * dt / 2d0)
      end do

      print *, 'Posicao final: ', x

      print *, 'Velocidade terminal: ', v

      print *, 'Instante da velocidade terminal: ', t_terminal

      close(10)

      end program