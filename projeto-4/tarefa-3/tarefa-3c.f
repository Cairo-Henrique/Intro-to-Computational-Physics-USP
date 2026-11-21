      program tarefa3c
      ! Pêndulo simples
      ! As grandezas físicas são adimensionalizadas
      ! Utiliza o método de Euler-Cromer
      ! Retorna theta(tau) e energia(tau)
      ! Retorna o espaço de fase (theta, omega)

      implicit none

      integer*4 i
      real*8 m, l, T0, dtau
      real*8 theta0, omega0
      real*8 tau, tau_max
      real*8 pi, g
      real*8 theta(10000), omega(10000), E(10000)
      real*8 energia

      parameter(pi = 3.14159265358979323d0) ! valor de pi
      parameter(g = 9.8d0) ! aceleracao da gravidade

      ! Constantes
      m = 1.0d0 ! Massa do pêndulo em kg
      l = 1.0d0 ! Comprimento do fio em m
      T0 = 2.00709d0 ! Período do pêndulo
      dtau = 0.005d0 ! passo de tempo

      ! Condições iniciais
      theta0 = pi/2d0 ! Ângulo inicial em rad
      omega0 = 0.0d0 ! Velocidade angular inicial adimensional

      ! Tempo máximo de simulação
      tau_max = 10.0d0 * pi

      ! ! Angulo e energia ! !

      ! Abre o arquivo de saída
      open(unit=10, file='tarefa-3c-saida-1.dat')

      ! Inicializa variáveis
      theta(1) = theta0
      omega(1) = omega0
      tau = 0.0d0
      i = 1
      do while (tau .le. tau_max)
            ! Calcula a energia total adimensional do pêndulo simples
            E(i) = energia(theta(i), omega(i))

            ! Escreve os valores no arquivo de saída
            print *, tau, theta(i), E(i)
            write(10,*) tau, theta(i), E(i)

            ! Atualiza as variáveis via método de Euler-Cromer
            omega(i+1) = omega(i) - sin(theta(i)) * dtau
            theta(i+1) = theta(i) + omega(i+1) * dtau

            ! Atualiza o tempo
            tau = tau + dtau
            i = i + 1
      end do

      close(10)

      ! ! Espaço de fase ! !

      ! Abre o arquivo de saída
      open(unit=20, file='tarefa-3c-saida-2.dat')

      ! Inicializa variáveis
      tau = 0.0d0
      i = 1
      do while (tau .le. tau_max)
            ! Escreve os valores no arquivo de saída
            print *, theta(i), omega(i)
            write(20,*) theta(i), omega(i)

            ! Atualiza o tempo
            tau = tau + dtau
            i = i + 1
      end do

      close(20)

      end program 

      function energia(theta, omega)
      ! Calcula a energia total adimensional do pêndulo simples
      implicit none
      real*8 theta, omega, energia
      energia = 0.5d0 * omega**2 + 1d0 - cos(theta)
      return
      end function