      program tarefa2c
      ! Efeito resistivo do ar em movimentos bidimensionais
      ! (i) C_d = 0.295 para velocidades menores que 343 m/s e C_d = 0.5 caso contrário
      ! (ii) rho depende da altura

      implicit none

      integer*4 unidade, i
      real*8 m, A, dt
      real*8 pi, g
      real*8 theta, v0, x0, y0

      real*8 theta_max, alcance_max
      real*8 Delta_x, alcance_horizontal

      real*8 Delta_x_minus, Delta_x_plus

      parameter(pi = 3.14159265358979323d0) ! valor de pi
      parameter(g = 9.8d0) ! aceleracao da gravidade

      ! Constantes
      m = 42.0d0 ! Massa do projétil em kg
      A = pi * (0.1491d0/2d0)**2 ! área frontal em m^2 de um calibre 149.1 mm
      dt = 0.01d0 ! passo de tempo

      ! Condições iniciais
      v0 = 377.0d0 ! velocidade inicial
      x0 = 0.0d0 ! x inicial
      y0 = 0.0d0 ! y inicial

      ! ! Análogo do item (a) ! !

      ! Primeiro ângulo: pi/6, sem resistência do ar
      open(unit=10, file='tarefa-2c-saida-1.dat')
      unidade = 10
      theta = pi / 6d0
      call trajetoria(.false., m, A, dt, g, v0, x0, y0,
     &                theta, unidade)
      close(10)

      ! Primeiro ângulo: pi/6, com resistência do ar
      open(unit=11, file='tarefa-2c-saida-2.dat')
      unidade = 11
      call trajetoria(.true., m, A, dt, g, v0, x0, y0,
     &                theta, unidade)
      close(11)

      ! Segundo ângulo: pi/4, sem resistência do ar
      open(unit=20, file='tarefa-2c-saida-3.dat')
      unidade = 20
      theta = pi / 4d0
      call trajetoria(.false., m, A, dt, g, v0, x0, y0,
     &                theta, unidade)
      close(20)

      ! Segundo ângulo: pi/4, com resistência do ar
      open(unit=21, file='tarefa-2c-saida-4.dat')
      unidade = 21
      call trajetoria(.true., m, A, dt, g, v0, x0, y0,
     &                theta, unidade)
      close(21)

      ! Terceiro ângulo: pi/3, sem resistência do ar
      open(unit=30, file='tarefa-2c-saida-5.dat')
      unidade = 30
      theta = pi / 3d0
      call trajetoria(.false., m, A, dt, g, v0, x0, y0,
     &                theta, unidade)
      close(30)

      ! Terceiro ângulo: pi/3, com resistência do ar
      open(unit=31, file='tarefa-2c-saida-6.dat')
      unidade = 31
      call trajetoria(.true., m, A, dt, g, v0, x0, y0,
     &                theta, unidade)
      close(31)

      ! ! Análogo do item (b) ! !

      theta_max = 0.0d0
      alcance_max = 0.0d0

      open(unit=70, file='tarefa-2c-saida-7.dat')

      do i = 1, 900
        theta = i * pi / 1800d0 ! ângulo em radianos

        Delta_x = alcance_horizontal(
     &   theta, m, A, dt, g, v0, x0, y0) ! Calcula o alcance horizontal do projétil

        if (Delta_x .gt. alcance_max) then
          alcance_max = Delta_x
          theta_max = theta
        end if
            
        ! Escreve os valores no arquivo de saída
        write(70,*) theta, Delta_x

      end do

      close(70)

      print *, 'Angulo do alcance maximo:', theta_max*180d0/pi, 'graus'
      print *, 'Alcance maximo:', alcance_max

      ! ! Sensibilidade do alcance horizontal frente a pequenas mudanças em v0 ! !

      Delta_x_minus = alcance_horizontal(
     &   theta_max, m, A, dt, g, 0.99d0*v0, x0, y0)

      Delta_x_plus = alcance_horizontal(
     &   theta_max, m, A, dt, g, 1.01d0*v0, x0, y0)

      print *, '0.99v0 => Delta_x =', Delta_x_minus
      print *, '1.01v0 => Delta_x =', Delta_x_plus

      ! ! Sensibilidade da velocidade em função do tempo frente a pequenas mudanças em v0 ! !

      ! Arquivos de saída
      open(unit=80, file='tarefa-2c-saida-8.dat')
      open(unit=90, file='tarefa-2c-saida-9.dat')

      ! Subrotinas que calculam velocidade em função do tempo e colocam nos arquivos de saída
      call velocidades(m, A, dt, g, 0.99d0*v0, x0,
     &                      y0, theta_max, 80)
      call velocidades(m, A, dt, g, 1.01d0*v0, x0,
     &                      y0, theta_max, 90)

      close(80)
      close(90)

      end program

      function get_C_d(velocidade_x, velocidade_y)
      real*8 velocidade_x, velocidade_y
      real*8 velocidade
      real*8 get_C_d
      velocidade = 
     & (velocidade_x**(2d0) + velocidade_y**(2d0))**(0.5d0)
      if (velocidade .lt. 343d0) then
        get_C_d = 0.295d0
      else
        get_C_d = 0.5d0
      end if
      return
      end function

      function get_rho(y)
      real*8 y
      real*8 rho0, beta, T0, alpha
      real*8 get_rho
      rho0 = 1.3d0
      beta = 6.49d0 * (10d0)**(-3d0)
      T0 = 300d0
      alpha = 4.256d0
      get_rho = rho0 * (1 - (beta/T0)*y)**alpha
      return
      end function

      subroutine trajetoria(is_resistencia, m, A, dt, g, v0, x0,
     &                      y0, theta, unidade)
      ! Calcula a trajetória (x,y) de um projétil com resistência do ar

      implicit none

      integer*4 i, unidade
      logical is_resistencia
      real*8 m, C_d, rho, A, dt, g
      real*8 theta, v0, x, y, x0, y0
      real*8 vx(100000), vy(100000)
      real*8 get_C_d, get_rho

      ! Condições iniciais
      x = x0
      y = y0
      vx(1) = v0 * cos(theta)
      vy(1) = v0 * sin(theta)

      ! Loop
      i = 1
      do while (y .ge. 0.0d0)
            ! Escreve os valores no arquivo de saída
            write(unidade,*) x, y

            ! Atualiza C_d e rho
            C_d = get_C_d(vx(i), vy(i))

            if (is_resistencia) then
            rho = get_rho(y)
            else
            rho = 0.0d0
            end if

            ! Atualiza as velocidades via método de Euler
            vx(i+1) = vx(i) - rho*A*C_d/(2d0*m)*
     &      sqrt(vx(i)**2 + vy(i)**2)*vx(i)*dt
            vy(i+1) = vy(i) - (g + rho*A*C_d/(2d0*m)*
     &      sqrt(vx(i)**2 + vy(i)**2)*vy(i))*dt

            ! Atualiza as posições via regra do retângulo
            x = x + dt * (vx(i))
            y = y + dt * (vy(i))

            ! Atualiza a iteração
            i = i + 1
      end do

      end subroutine

      function alcance_horizontal(
     &   theta, m, A, dt, g, v0, x0, y0)
      ! Calcula o alcance horizontal de um projétil com resistência do ar

      integer*4 i
      real*8 m, C_d, rho, A, dt, g
      real*8 theta, v0, x, y, x0, y0
      real*8 vx(100000), vy(100000)
      real*8 get_C_d, get_rho

      real*8 alcance_horizontal

      ! Condições iniciais
      x = x0
      y = y0
      vx(1) = v0 * cos(theta)
      vy(1) = v0 * sin(theta)

      ! Loop
      i = 1
      do while (y .ge. 0.0d0)

            ! Atualiza C_d e rho
            C_d = get_C_d(vx(i), vy(i))
            rho = get_rho(y)

            ! Atualiza as velocidades via método de Euler
            vx(i+1) = vx(i) - rho*A*C_d/(2d0*m)*
     &      sqrt(vx(i)**2 + vy(i)**2)*vx(i)*dt
            vy(i+1) = vy(i) - (g + rho*A*C_d/(2d0*m)*
     &      sqrt(vx(i)**2 + vy(i)**2)*vy(i))*dt

            ! Atualiza as posições via regra do retângulo
            x = x + dt * (vx(i))
            y = y + dt * (vy(i))

            ! Atualiza a iteração
            i = i + 1
      end do

      alcance_horizontal = x

      return
      end function

      subroutine velocidades(m, A, dt, g, v0, x0,
     &                      y0, theta, unidade)
      ! Calcula a velocidade em função do tempo de um projétil com resistência do ar

      implicit none

      integer*4 i, unidade
      real*8 m, C_d, rho, A, dt, g
      real*8 theta, t, v, v0, x, y, x0, y0
      real*8 vx(100000), vy(100000)
      real*8 get_C_d, get_rho

      ! Condições iniciais
      x = x0
      y = y0
      vx(1) = v0 * cos(theta)
      vy(1) = v0 * sin(theta)

      ! Loop
      i = 1
      do while (y .ge. 0.0d0)
            ! Tempo e velocidade atuais
            t = dble(i-1) * dt
            v = (vx(i)**(2d0) + vy(i)**(2d0))**(0.5d0)

            ! Escreve os valores no arquivo de saída
            write(unidade,*) t, v

            ! Atualiza C_d e rho
            C_d = get_C_d(vx(i), vy(i))
            rho = get_rho(y)

            ! Atualiza as velocidades via método de Euler
            vx(i+1) = vx(i) - rho*A*C_d/(2d0*m)*
     &      sqrt(vx(i)**2 + vy(i)**2)*vx(i)*dt
            vy(i+1) = vy(i) - (g + rho*A*C_d/(2d0*m)*
     &      sqrt(vx(i)**2 + vy(i)**2)*vy(i))*dt

            ! Atualiza as posições via regra do retângulo
            x = x + dt * (vx(i))
            y = y + dt * (vy(i))

            ! Atualiza a iteração
            i = i + 1
      end do

      end subroutine