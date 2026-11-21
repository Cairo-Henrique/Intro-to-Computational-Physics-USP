      program tarefa5a
      ! Pêndulo amortecido e forçado
      ! As grandezas físicas são adimensionalizadas
      ! Utiliza o método de Euler-Cromer
      ! Retorna theta(tau), omega(tau) e energia(tau)

      implicit none

      character(30) filename
      integer*4 i
      real*8 m, l, T0, gamma, nu, dtau
      real*8 theta0, omega0
      real*8 tau_max
      real*8 pi
      real*8 alpha(6)

      parameter(pi = 3.14159265358979323d0) ! valor de pi

      ! Constantes
      m = 1.0d0 ! Massa do pêndulo em kg
      l = 1.0d0 ! Comprimento do fio em m
      T0 = 2.00709d0 ! Período do pêndulo simples em s
      gamma = 0.5d0 ! Coeficiente de amortecimento
      nu = (2d0/3d0) ! Frequência de excitação adimensionalizada
      dtau = 0.001d0 ! passo de tempo

      ! constante adimensional que parametriza a intensidade do termo forçante
      alpha = (/ 0.5d0, 0.75d0, 1.0d0, 1.25d0, 1.4d0, 1.5d0 /)

      ! Condições iniciais
      theta0 = 0.0d0 ! Ângulo inicial em rad
      omega0 = 0.0d0 ! Velocidade angular inicial adimensional

      ! Tempo máximo de simulação
      tau_max = 50.0d0 * 2.0d0 * pi ! 50 períodos do pêndulo simples

      ! Executa simulação para diferentes valores de alpha
      do i = 1, 6
          print *, 'Simulação com alpha =', alpha(i)
          write(filename,'(A,I1,A)') 'tarefa-5a-saida-', i, '.dat'
          call simula_pendulo(filename,
     &     theta0, omega0, gamma, nu, alpha(i), dtau, tau_max)
      end do

      end program 

      function energia(theta, omega)
      ! Calcula a energia total adimensional do pêndulo simples
      implicit none
      real*8 theta, omega, energia
      energia = 0.5d0 * omega**2 + 1d0 - cos(theta)
      return
      end function

      subroutine simula_pendulo(filename, 
     &   theta0, omega0, gamma, nu, alpha, dtau, tau_max)
      ! Simula o pêndulo amortecido e forçado usando o método de Euler-Cromer
      ! Retorna theta(tau), omega(tau) e energia(tau) em um arquivo de saída filename
      implicit none
      character(30) filename
      integer*4 i
      real*8 theta0, omega0, gamma, nu, alpha, dtau, tau_max
      real*8 tau, theta, omega, E
      real*8 energia
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
            ! Calcula a energia total adimensional do pêndulo
            E = energia(theta, omega)

            ! Escreve os valores no arquivo de saída
            write(10,*) tau, theta, omega, E
            
            ! Atualiza as variáveis via método de Euler-Cromer
            omega = omega + 
     &       dtau * (- sin(theta) - gamma * omega
     &       + alpha * sin(nu*tau))
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