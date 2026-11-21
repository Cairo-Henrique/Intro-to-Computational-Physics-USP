      program tarefa6a
      ! Pêndulo preso a um ponto oscilante e imerso em fluido viscoso
      ! As grandezas físicas são adimensionalizadas
      ! Utiliza o método de Euler-Cromer
      ! Retorna theta_min em função de nu

      implicit none

      integer*4 i
      real*8 gamma, alpha, nu, dtau
      real*8 theta0, omega0
      real*8 tau_max
      real*8 pi
      real*8 abs_theta_min
      real*8 get_theta_min

      parameter(pi = 3.14159265358979323d0) ! valor de pi

      ! Constantes
      gamma = 0.05d0 ! Coeficiente de amortecimento
      alpha = 0.1d0 ! amplitude adimensional do movimento do ponto de apoio
      dtau = 0.001d0 ! passo de tempo

      ! Condições iniciais
      theta0 = 0.9999d0 * pi ! Ângulo inicial em rad
      omega0 = 0.0d0 ! Velocidade angular inicial em rad/s

      ! Tempo máximo de simulação
      tau_max = 30.0d0 * 2.0d0 * pi ! 30 períodos do pêndulo simples

      ! Abre o arquivo de saída
      open(unit=10, file='tarefa-6a-saida-1.dat')

      ! Executa simulação para diferentes valores de ni
      do i = 1, 21
          nu = dble(i-1) ! Frequência de excitação adimensionalizada
          abs_theta_min = get_theta_min(
     &       theta0, omega0, gamma, nu, alpha, dtau, tau_max)
          write(10, *) nu, abs_theta_min
          
          ! Checa se ocorreu estabilização dinâmica
          if (abs_theta_min .gt. 1.0d-3) then
              print *, 'Estabilizacao dinamica ocorreu para nu =', nu
          end if
      end do

      close(10)
      end program 

      function get_theta_min(
     &       theta0, omega0, gamma, nu, alpha, dtau, tau_max)
      ! Simula o pêndulo preso a um ponto oscilante e imerso em fluido viscoso usando o método de Euler-Cromer
      ! Retorna o valor mínimo de |theta| durante a simulação
      implicit none
      integer*4 i
      real*8 theta0, omega0, gamma, nu, alpha, dtau, tau_max
      real*8 tau, theta, omega
      real*8 pi
      real*8 get_theta_min
      parameter(pi = 3.14159265358979323d0)

      ! Inicializa variáveis
      theta = theta0
      omega = omega0
      tau = 0.0d0
      i = 1

      ! Inicializa o valor mínimo de |theta| com o valor inicial
      get_theta_min = abs(theta0)
      
      do while (tau .le. tau_max)
            
            ! Atualiza as variáveis via método de Euler-Cromer
            omega = omega + 
     &       dtau * (- sin(theta) - gamma * omega
     &       + alpha * (nu**2) * sin(nu*tau) * sin(theta))
            theta = theta + omega * dtau

            ! Mantem o angulo no intervalo (-pi, pi]
            do while (theta .gt. pi)
              theta = theta - 2.0d0*pi
            end do
            do while (theta .le. -pi)
              theta = theta + 2.0d0*pi
            end do

            ! Atualiza o valor mínimo de |theta| se necessário
            if (abs(theta) < get_theta_min) then
                get_theta_min = abs(theta)
            end if

            ! Atualiza o tempo
            tau = tau + dtau
            i = i + 1
      end do

      return
      end function 