      program tarefa5d
      ! Pendulo amortecido e forcado
      ! As grandezas fisicas sao adimensionalizadas
      ! Utiliza o metodo de Euler-Cromer
      ! Retorna theta(tau), omega(tau) e E(tau) para o calculo
      ! dos expoentes de Lyapunov, com condicoes iniciais
      ! perturbadas em relacao ao programa tarefa5a

      implicit none

      character(30) filename
      integer*4 i
      real*8 T0, gamma, nu, dtau
      real*8 theta0, omega0
      real*8 tau_max
      real*8 pi
      real*8 alpha(6)
      logical periodico(6)

      parameter(pi = 3.14159265358979323d0)

      ! Constantes
      T0 = 2.00709d0
      gamma = 0.5d0
      nu = (2d0/3d0)
      dtau = 0.001d0

      ! constante adimensional que parametriza a intensidade do termo forçante
      alpha = (/ 0.5d0, 0.75d0, 1.0d0, 1.25d0, 1.4d0, 1.5d0 /)

      ! Classificacao preliminar dos regimes:
      ! .true.  = periodico
      ! .false. = caotico
      ! Ajuste conforme os resultados do item 5a.
      periodico = (/ .true., .true., .true.,
     &               .false., .true., .false. /)

      ! Condicao inicial do angulo
      theta0 = 0.0d0

      ! Tempo maximo de simulacao
      tau_max = 50.0d0 * 2.0d0 * pi

      ! Executa simulacao para diferentes valores de alpha
      do i = 1, 6

          ! Escolhe a velocidade angular inicial conforme o regime
          if (periodico(i)) then
              omega0 = 1.0d-2
          else
              omega0 = 1.0d-5
          end if

          print *, 'Simulacao com alpha =', alpha(i)
          print *, 'omega0 perturbado =', omega0

          ! Arquivo de saida da nova trajetoria
          write(filename,'(A,I1,A)')
     &         'tarefa-5d-saida-', i, '.dat'

          call simula_pendulo(filename, theta0, omega0,
     &         gamma, nu, alpha(i), dtau, tau_max)

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