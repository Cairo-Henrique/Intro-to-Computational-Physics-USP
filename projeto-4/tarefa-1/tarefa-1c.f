      program tarefa1c
      ! Efeito resistivo do ar em movimentos unidimensionais

      implicit none

      integer*4 i, j, n
      real*8 m, P_c, rho, A, t_max, dt
      real*8 v0, v, t, x, x0, K
      real*8 t_terminal
      real*8 areas(3)
      real*8 valores_v(0:10000)
      character*24 arquivos(3)

      ! Constantes
      m = 70.0d0 ! Massa do corpo em kg
      P_c = 400.0d0 ! Potencia do ciclista em Watts
      rho = 1.3d0 ! densidade do meio em kg/m^3
      t_max = 5d0 * 60.0d0 ! 5 minutos
      dt = 0.1d0 ! passo de tempo

      ! Areas frontais em m^2
      areas(1) = 0.2d0
      areas(2) = 0.3d0
      areas(3) = 0.4d0

      ! Arquivos de saida
      arquivos(1) = 'tarefa-1c-saida-1.dat'
      arquivos(2) = 'tarefa-1c-saida-2.dat'
      arquivos(3) = 'tarefa-1c-saida-3.dat'

      ! Condicoes iniciais
      v0 = 0.0d0
      x0 = 0.0d0
      n = int(t_max/dt)

      ! Loop sobre os valores da area frontal
      do j = 1, 3

            A = areas(j)
            v = v0
            K = 0.5d0 * m * v0**2
            x = x0
            valores_v(0) = v0
            t_terminal = -1d0

            ! Abre o arquivo de saida
            open(unit=10+j, file=trim(arquivos(j)))

            print *, 'Area frontal A = ', A

            ! Loop
            do i = 0, n
                  t = i * dt

                  ! Escreve os dados de velocidade
                  write(10+j,*) t, v

                  if (i == n) exit

                  ! Atualiza a energia cinética usando o método de Euler
                  K = K + dt * (P_c -
     &                sqrt(2d0 / m**3) * rho * A *
     &                K**(3d0/2d0))

                  ! Calcula a velocidade a partir da energia cinética
                  v = sqrt(2d0 * K / m)

                  valores_v(i+1) = v

                  ! Verifica se a velocidade é terminal em t + dt
                  if (t_terminal .lt. 0d0 .and.
     &                abs(valores_v(i+1) -
     &                valores_v(i)) .lt. 1d-6) then
                        t_terminal = t + dt
                  end if

                  ! Atualiza a posição usando a Regra do Trapézio
                  x = x + (valores_v(i) +
     &                      valores_v(i+1)) * dt / 2d0
            end do

            print *, 'Posicao final: ', x
            print *, 'Velocidade final: ', v
            print *, 'Tempo terminal aproximado: ',
     &               t_terminal

            close(10+j)

      end do

      end program