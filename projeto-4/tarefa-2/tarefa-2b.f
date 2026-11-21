      program tarefa2b
      ! Efeito resistivo do ar em movimentos bidimensionais

      implicit none

      integer*4 i
      real*8 m, C_d, rho, A, dt
      real*8 pi, g
      real*8 theta, v0, x0, y0
      real*8 Delta_x, alcance_max, theta_max
      real*8 alcance_horizontal

      parameter(pi = 3.14159265358979323d0) ! valor de pi
      parameter(g = 9.8d0) ! aceleracao da gravidade

      ! Constantes
      m = 42.0d0 ! Massa do projétil em kg
      C_d = 0.295d0 ! Coeficiente de arrasto
      rho = 1.3d0 ! densidade do meio em kg/m^3
      A = pi * (0.1491d0/2d0)**2 ! área frontal em m^2 de um calibre 149.1 mm
      dt = 0.01d0 ! passo de tempo

      ! Condições iniciais
      v0 = 377.0d0 ! velocidade inicial
      x0 = 0.0d0 ! x inicial
      y0 = 0.0d0 ! y inicial

      theta_max = 0.0d0
      alcance_max = 0.0d0

      open(unit=10, file='tarefa-2b-saida-1.dat')

      do i = 1, 900
        theta = i * pi / 1800d0 ! ângulo em radianos

        Delta_x = alcance_horizontal(
     &   theta, m, C_d, rho, A, dt, g, v0, x0, y0) ! Calcula o alcance horizontal do projétil

        if (Delta_x .gt. alcance_max) then
          alcance_max = Delta_x
          theta_max = theta
        end if
            
        ! Escreve os valores no arquivo de saída
        print *, theta, Delta_x
        write(10,*) theta, Delta_x

      end do

      close(10)

      print *, 'Angulo do alcance maximo:', theta_max*180d0/pi, 'graus'
      print *, 'Alcance maximo:', alcance_max

      end program

      function alcance_horizontal(
     &   theta, m, C_d, rho, A, dt, g, v0, x0, y0)
      ! Calcula o alcance horizontal de um projétil com resistência do ar

      integer*4 i
      real*8 m, C_d, rho, A, dt, g
      real*8 theta, v0, x, y, x0, y0
      real*8 vx(100000), vy(100000)

      real*8 alcance_horizontal

      ! Condições iniciais
      x = x0
      y = y0
      vx(1) = v0 * cos(theta)
      vy(1) = v0 * sin(theta)

      ! Loop
      i = 1
      do while (y .ge. 0.0d0)

            ! Atualiza as velocidades via método de Euler
            vx(i+1) = vx(i) - rho*A*C_d/(2d0*m)*
     &      sqrt(vx(i)**2 + vy(i)**2)*vx(i)*dt
            vy(i+1) = vy(i) - (g + rho*A*C_d/(2d0*m)*
     &      sqrt(vx(i)**2 + vy(i)**2)*vy(i))*dt

            ! Atualiza as posições via regra do trapézio
            x = x + dt * (vx(i) + vx(i+1))/2d0
            y = y + dt * (vy(i) + vy(i+1))/2d0

            ! Atualiza a iteração
            i = i + 1
      end do

      alcance_horizontal = x

      return
      end function