      program tarefa1b
      ! Órbitas circulares: comparação entre os métodos de EC e Verlet.
      ! Utiliza-se a 3a Lei de Kepler GM⊙ = 4pi^2 UA^3 ano^(−2).
      ! Grandezas adimensionais rho = r/UA, tau = 2pi*t/ano, v = v/(2pi*UA/ano).
      ! Toma-se o Sol como origem estática e a Terra como corpo em órbita.

      implicit none

      integer*4 i, nsteps
      real*8 a, e, tau, periodo, taumax, dtau
      real*8 rx(10000), ry(10000), vx(10000), vy(10000), r(10000)
      real*8 pi
      parameter (pi = 3.14159265358979323d0)

      ! Condições iniciais adimensionais
      a = 1.0d0 ! semieixo maior da órbita elíptica
      e = 0.0d0 ! excentricidade da órbita elíptica
      rx(1) = a ! posição inicial no eixo x
      ry(1) = 0.0d0 ! posição inicial no eixo y
      vx(1) = - e / sqrt(a) ! velocidade inicial no eixo x
      vy(1) = sqrt((1.0d0-e**2)/a) ! velocidade inicial no eixo y para a órbita elíptica especificada acima

      ! Parâmetros da simulação
      periodo = 2.0d0 * pi * a**(3d0/2d0) ! período adimensional (3a Lei de Kepler)
      taumax = 10.0d0 * periodo ! 10 períodos adimensionais
      dtau = 0.01d0 ! passo de tempo adimensional

      ! Número de passos da simulação
      nsteps = int(taumax/dtau)

      ! Simulação via método de Euler-Cromer
      open(10, file='tarefa-1b-saida-1.dat')

      do i = 1, nsteps

        tau = (i-1)*dtau
        write(10,*) tau, rx(i), ry(i), vx(i), vy(i)

        ! Atualização da distância
        r(i) = sqrt(rx(i)**2 + ry(i)**2)

        ! Atualização das posições e velocidades
        vx(i+1) = vx(i) - (rx(i)/r(i)**3)*dtau
        rx(i+1) = rx(i) + vx(i+1)*dtau
        vy(i+1) = vy(i) - (ry(i)/r(i)**3)*dtau
        ry(i+1) = ry(i) + vy(i+1)*dtau

      end do

      close(10)

      ! Simulação via método de Verlet
      ! Para o método de Verlet, precisamos de uma segunda posição inicial
      ! Calculamos os valores de rx(1), ry(1), vx(1), vy(1)
      ! por meio do método de Euler para calcular a posição do passo i=2
      open(20, file='tarefa-1b-saida-2.dat')

      write(20,*) 0.0d0, rx(1), ry(1), vx(1), vy(1)

      rx(2) = rx(1) + vx(1)*dtau
      ry(2) = ry(1) + vy(1)*dtau

      do i = 2, nsteps

        ! Atualização da distância
        r(i) = sqrt(rx(i)**2 + ry(i)**2)

        ! Atualização das posições
        rx(i+1) = 2.0d0*rx(i) - rx(i-1) - (rx(i)/r(i)**3)*dtau**2
        ry(i+1) = 2.0d0*ry(i) - ry(i-1) - (ry(i)/r(i)**3)*dtau**2

        ! Atualização das velocidades
        vx(i) = (rx(i+1) - rx(i-1))/(2.0d0*dtau)
        vy(i) = (ry(i+1) - ry(i-1))/(2.0d0*dtau)

        tau = (i-1)*dtau
        write(20,*) tau, rx(i), ry(i), vx(i), vy(i)

      end do

      close(20)

      end program