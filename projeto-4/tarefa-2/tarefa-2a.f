      program tarefa2a
      ! Efeito resistivo do ar em movimentos bidimensionais

      implicit none

      integer*4 unidade
      real*8 m, C_d, rho, A, dt
      real*8 pi, g
      real*8 theta, v0, x0, y0

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

      ! Primeiro ângulo: pi/6, sem resistência do ar
      open(unit=10, file='tarefa-2a-saida-1.dat')
      unidade = 10
      theta = pi / 6d0
      call trajetoria(m, C_d, 0d0, A, dt, g, v0, x0, y0,
     &                theta, unidade)
      close(10)

      ! Primeiro ângulo: pi/6, com resistência do ar
      open(unit=11, file='tarefa-2a-saida-2.dat')
      unidade = 11
      call trajetoria(m, C_d, rho, A, dt, g, v0, x0, y0,
     &                theta, unidade)
      close(11)

      ! Segundo ângulo: pi/4, sem resistência do ar
      open(unit=20, file='tarefa-2a-saida-3.dat')
      unidade = 20
      theta = pi / 4d0
      call trajetoria(m, C_d, 0d0, A, dt, g, v0, x0, y0,
     &                theta, unidade)
      close(20)

      ! Segundo ângulo: pi/4, com resistência do ar
      open(unit=21, file='tarefa-2a-saida-4.dat')
      unidade = 21
      call trajetoria(m, C_d, rho, A, dt, g, v0, x0, y0,
     &                theta, unidade)
      close(21)

      ! Terceiro ângulo: pi/3, sem resistência do ar
      open(unit=30, file='tarefa-2a-saida-5.dat')
      unidade = 30
      theta = pi / 3d0
      call trajetoria(m, C_d, 0d0, A, dt, g, v0, x0, y0,
     &                theta, unidade)
      close(30)

      ! Terceiro ângulo: pi/3, com resistência do ar
      open(unit=31, file='tarefa-2a-saida-6.dat')
      unidade = 31
      call trajetoria(m, C_d, rho, A, dt, g, v0, x0, y0,
     &                theta, unidade)
      close(31)

      end program


      subroutine trajetoria(m, C_d, rho, A, dt, g, v0, x0,
     &                      y0, theta, unidade)
      ! Calcula a trajetória (x,y) de um projétil com resistência do ar

      implicit none

      integer*4 i, unidade
      real*8 m, C_d, rho, A, dt, g
      real*8 theta, v0, x, y, x0, y0
      real*8 vx(100000), vy(100000)

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
            print *, x, y

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

      end subroutine