      program tarefa6c
      ! Pêndulo preso a um ponto oscilante e imerso em fluido viscoso
      ! As grandezas físicas são adimensionalizadas
      ! Utiliza o método de Euler-Cromer
      ! Retorna theta(tau)

      implicit none

      character(30) filename
      real*8 gamma, alpha, nu, dtau
      real*8 theta0, omega0
      real*8 tau_max
      real*8 pi

      parameter(pi = 3.14159265358979323d0) ! valor de pi

      ! Constantes
      gamma = 0.05d0 ! Coeficiente de amortecimento
      alpha = 0.1d0 ! amplitude adimensional do movimento do ponto de apoio
      nu = 40 ! Frequência de excitação adimensionalizada
      dtau = 0.001d0 ! passo de tempo

      ! Condições iniciais
      theta0 = 0.9999d0 * pi ! Ângulo inicial em rad
      omega0 = 0.0d0 ! Velocidade angular inicial em rad/s

      ! Tempo máximo de simulação
      tau_max = 30.0d0 * 2.0d0 * pi ! 30 períodos do pêndulo simples

      ! Abre o arquivo de saída
      filename = 'tarefa-6c-saida-1.dat'

      call simula_pendulo(filename, 
     &   theta0, omega0, gamma, nu, alpha, dtau, tau_max)

      close(10)
      end program 

      subroutine simula_pendulo(filename, 
     &   theta0, omega0, gamma, nu, alpha, dtau, tau_max)
      ! Simula o pêndulo preso a um ponto oscilante e imerso em fluido viscoso usando o método de Euler-Cromer
      ! Retorna theta(tau) em um arquivo de saída filename
      implicit none
      character(30) filename
      integer*4 i
      real*8 theta0, omega0, gamma, nu, alpha, dtau, tau_max
      real*8 tau, theta, omega
      real*8 pi
      parameter(pi = 3.14159265358979323d0)

      ! Abre o arquivo de saída
      open(unit=10, file=filename)

      ! Inicializa variáveis
      theta = theta0
      omega = omega0
      tau = 0.0d0
      i = 1
      do while (tau .le. tau_max)

            ! Escreve os valores no arquivo de saída
            write(10,*) tau, theta
            
            ! Atualiza as variáveis via método de Euler-Cromer
            omega = omega + 
     &       dtau * (- sin(theta) - gamma * omega
     &       + alpha * (nu**2) * sin(nu*tau) * sin(theta))
            theta = theta + omega * dtau

            ! Mantem o angulo no intervalo [-pi, pi)
            do while (theta .ge. pi)
              theta = theta - 2.0d0*pi
            end do
            do while (theta .lt. -pi)
              theta = theta + 2.0d0*pi
            end do

            ! Atualiza o tempo
            tau = tau + dtau
            i = i + 1
      end do

      close(10)

      end subroutine