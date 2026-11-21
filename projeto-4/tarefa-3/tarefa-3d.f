      program tarefa3d
      ! Pêndulo simples
      ! As grandezas físicas são adimensionalizadas
      ! Utiliza o método de Euler-Cromer
      ! Retorna período em função do ângulo inicial

      implicit none

      integer*4 i
      real*8 dtau
      real*8 theta0
      real*8 pi, g
      real*8 periodo_euler_cromer, periodo_integral
      real*8 T_ec(90), T_integral, T_aproximado

      parameter(pi = 3.14159265358979323d0) ! valor de pi
      parameter(g = 9.8d0) ! aceleracao da gravidade

      ! Passo de tempo
      dtau = 0.001d0 

      ! Abre o arquivo de saída
      open(unit=10, file='tarefa-3d-saida-1.dat')

      do i = 1, 90
            theta0 = dble(i) * pi / 180d0 ! Converte para radianos
            ! Calcula o período do pêndulo simples
            T_ec(i) = periodo_euler_cromer(theta0, dtau)
            T_integral = periodo_integral(theta0, dtau)
            T_aproximado = 2d0 * pi * (1d0 + (1d0/16d0) * theta0**2)
            ! Escreve os valores no arquivo de saída
            print *, theta0, T_ec(i), T_integral, T_aproximado
            write(10,*) theta0, T_ec(i), T_integral, T_aproximado
      end do

      close(10)

      end program 

      function periodo_euler_cromer(theta0, dtau)
      ! Calcula o período do pêndulo simples em função do ângulo inicial
      ! Utiliza o método de Euler-Cromer
      implicit none
      real*8 theta0, omega0
      real*8 dtau, tau, tau_max
      real*8 theta(2), omega(2)
      real*8 periodo_euler_cromer
      periodo_euler_cromer = 0.0d0
      ! Inicializa variáveis
      omega0 = 0.0d0
      tau_max = 10.0d0 * 3.14159265358979323d0
      tau = 0.0d0
      theta(1) = theta0
      omega(1) = omega0
      ! Loop para calcular o período
      do while (tau .le. tau_max)
            ! Atualiza as variáveis via método de Euler-Cromer
            omega(2) = omega(1) - sin(theta(1)) * dtau
            theta(2) = theta(1) + omega(2) * dtau

            ! Atualiza o tempo
            tau = tau + dtau

            ! Verifica se o pêndulo passou pelo ponto de retorno
            if (theta(2) .lt. 0.0d0 .and. theta(1) .ge. 0.0d0) then
                  periodo_euler_cromer = 4 * tau
                  return
            end if

            ! Atualiza as variáveis para a próxima iteração
            theta(1) = theta(2)
            omega(1) = omega(2)
      end do
      return
      end function

      function periodo_integral(theta0, h)
      ! Calcula o período do pêndulo simples em função do ângulo inicial
      ! Com base na integral elíptica completa de primeira espécie
      ! Utiliza o método de Boole para calcular a integral
      implicit none
      real*8 theta0, periodo_integral
      real*8 h, a, b, boole, pi
      parameter(pi = 3.14159265358979323d0)
      a = 0.0d0
      b = pi/2d0
      periodo_integral = 4d0 * boole(h, a, b, theta0)
      return
      end function

      function f(u, theta0)
      ! Integrando da integral elíptica transformada
      ! A substituição é u = arcsin(sin(theta/2)/sin(theta0/2))
      ! Por simetria, integra-se de u = 0 a u = pi/2
      ! e multiplica-se o resultado por 4
      implicit none
      real*8 u, theta0, f
      f = 1d0 / sqrt(1d0 -
     &     (sin(theta0/2d0))**2 * sin(u)**2)
      return
      end function

      function boole(h,a,b,theta0)
      ! Regra de Boole para aproximar a integral
      ! de f(x) no intervalo [a,b], utilizando blocos de quatro
      ! subintervalos de largura h.
      !
      ! Entrada:
      !   h = largura dos subintervalos
      !   a = limite inferior de integracao
      !   b = limite superior de integracao
      !
      ! Saida:
      !   boole = aproximacao numerica da integral de f(x)
      !           no intervalo [a,b].
      !
      ! Observacao:
      !   O numero de subintervalos N deve ser multiplo de 4.
      implicit none
      integer*4 i, N
      real*8 f, h, a, b, boole, theta0, h_eff
      N = int((b-a)/h)
      N = 4 * (N/4) ! Ajusta N para que seja múltiplo de 4
      h_eff = (b-a)/dble(N) ! Ajusta h para que seja compatível com N
      boole = 0d0
      do i = 0, N/4-1
        boole = boole + (2d0*h_eff/45d0) *
     &  (7d0*f(a + 4*i*h_eff, theta0)
     &  + 32d0*f(a + (4*i+1)*h_eff, theta0)
     &  + 12d0*f(a + (4*i+2)*h_eff, theta0)
     &  + 32d0*f(a + (4*i+3)*h_eff, theta0)
     &  + 7d0*f(a + (4*i+4)*h_eff, theta0))
      end do
      return
      end function