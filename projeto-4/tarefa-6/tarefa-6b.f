      program tarefa6b
      ! Pêndulo preso a um ponto oscilante e imerso em fluido viscoso
      ! As grandezas físicas são adimensionalizadas
      ! Utiliza o método de Euler-Cromer
      ! Retorna nu* em função de theta0

      implicit none

      real*8 gamma, alpha, dtau
      real*8 theta0, omega0
      real*8 tau_max
      real*8 pi
      real*8 nu_star
      real*8 get_nu_star

      parameter(pi = 3.14159265358979323d0) ! valor de pi

      ! Constantes
      gamma = 0.0d0 ! Coeficiente de amortecimento
      alpha = 0.1d0 ! amplitude adimensional do movimento do ponto de apoio
      dtau = 0.001d0 ! passo de tempo

      ! Condições iniciais
      omega0 = 0.0d0 ! Velocidade angular inicial em rad/s

      ! Tempo máximo de simulação
      tau_max = 30.0d0 * 2.0d0 * pi ! 30 períodos do pêndulo simples

      ! Abre o arquivo de saída
      open(unit=10, file='tarefa-6b-saida-1.dat')

      ! Executa simulação para diferentes valores de theta0
      theta0 = 0.81d0 * pi
      do while (theta0 .lt. pi)
          nu_star = get_nu_star(
     &         theta0, omega0, gamma, alpha, dtau, tau_max)
          write(10,*) theta0, nu_star
          print *, 'theta0 =', theta0,
     &             'nu_star =', nu_star
      theta0 = theta0 + 0.01d0 * pi
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

      function get_nu_star(
     &       theta0, omega0, gamma, alpha, dtau, tau_max)
      ! Variando nu, simula vários pêndulos presos a um ponto oscilante e imersos em fluido viscoso usando o método de Euler-Cromer
      ! Retorna o valor de nu* que corresponde ao limiar no qual o pêndulo se estabiliza dinamicamente (|theta|_min > 1e-3)
      implicit none
      real*8 theta0, omega0, gamma, nu, alpha, dtau, tau_max
      real*8 pi
      real*8 abs_theta_min
      real*8 get_theta_min
      real*8 get_nu_star
      parameter(pi = 3.14159265358979323d0)

      nu = 0.0d0

      get_nu_star = -1.0d0 ! Valor padrão caso não encontre nu*

      do while (nu .le. 500.0d0)
          nu = nu + 0.5d0 ! Incrementa nu
          abs_theta_min = get_theta_min(
     &       theta0, omega0, gamma, nu, alpha, dtau, tau_max)
          if (abs_theta_min .gt. 1.0d-3) then
              get_nu_star = nu
              return
          end if
      end do

      return
      end function 