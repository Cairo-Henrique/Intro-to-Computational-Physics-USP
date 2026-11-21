      program tarefa4
      ! Pêndulo amortecido
      ! As grandezas físicas são adimensionalizadas
      ! Utiliza o método de Euler-Cromer
      ! Retorna theta(tau)

      implicit none

      integer*4 i
      real*8 gamma, dtau
      real*8 theta0, omega0
      real*8 tau, tau_max
      real*8 pi, g
      real*8 theta(10000), omega(10000)

      parameter(pi = 3.14159265358979323d0) ! valor de pi
      parameter(g = 9.8d0) ! aceleracao da gravidade

      ! Constantes
      gamma = 0.5d0 ! Coeficiente de amortecimento
      dtau = 0.005d0 ! passo de tempo

      ! Condições iniciais
      theta0 = pi/2d0 ! Ângulo inicial em rad
      omega0 = 0.0d0 ! Velocidade angular inicial adimensional

      ! Tempo máximo de simulação
      tau_max = 10.0d0 * pi

      ! Abre o arquivo de saída
      open(unit=10, file='tarefa-4-saida-1.dat')

      ! Inicializa variáveis
      theta(1) = theta0
      omega(1) = omega0
      tau = 0.0d0
      i = 1
      do while (tau .le. tau_max)
            ! Escreve os valores no arquivo de saída
            print *, tau, theta(i)
            write(10,*) tau, theta(i)

            ! Atualiza as variáveis via método de Euler-Cromer
            omega(i+1) = omega(i) - 
     &       dtau * (sin(theta(i)) + gamma * omega(i))
            theta(i+1) = theta(i) + omega(i+1) * dtau

            ! Atualiza o tempo
            tau = tau + dtau
            i = i + 1
      end do

      close(10)

      end program 