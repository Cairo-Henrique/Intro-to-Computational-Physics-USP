      program tarefa2c
      ! O problema de dois corpos: órbitas com método de Verlet.
      ! Gera trajetórias sob forças centrais da forma F = - rho^phi.
      ! Mostra exemplos com phi > 0 e phi <= -3.
      ! Utiliza-se a 3a Lei de Kepler GM⊙ = 4pi^2 UA^3 ano^(−2).
      ! Grandezas adimensionais rho = r/UA, tau = 2pi*t/ano, v = v/(2pi*UA/ano).
      ! Toma-se o Sol como origem estática e a Terra como corpo em órbita.

      implicit none

      character(30) filename
      integer*4 i, j
      real*8 phi
      real*8 taumax, dtau
      real*8 rx(50000), ry(50000), vx, vy, r(50000)
      real*8 velocidades(5)
      real*8 expoentes(7)
      real*8 pi
      parameter (pi = 3.14159265358979323d0)

      ! Condições iniciais adimensionais
      rx(1) = 1.0d0 ! posição inicial no eixo x
      ry(1) = 0.0d0 ! posição inicial no eixo y
      vx = 0.0d0 ! velocidade inicial no eixo x

      ! Parâmetros da simulação
      taumax = 12.0d0 * pi ! Tempo adimensional máximo
      dtau = 0.001d0 ! passo de tempo adimensional

      ! ! Varia a vy(1) para gerar trajetórias diferentes com diferentes forças centrais ! !

      ! Velocidades iniciais para diferentes órbitas
      velocidades(1) = 0.80d0
      velocidades(2) = 1.00d0
      velocidades(3) = 1.20d0

      ! Expoentes das forças centrais
      expoentes(1) = 0.5d0
      expoentes(2) = 1.0d0
      expoentes(3) = 2.0d0
      expoentes(4) = 4.0d0
      expoentes(5) = -3.0d0
      expoentes(6) = -4.0d0
      expoentes(7) = -5.0d0

      ! Simulações para phi > 0
      do i = 1,4
        phi = expoentes(i)

        do j = 1,2
          if (j .eq. 1) then
            vy = 1.0d0
          else
            vy = 2.0d0
          end if

          write(filename, '(A,I1,A,I1,A)')
     &       'tarefa-2c-saida-', i, '-', j, '.dat'

          call simulacao_orbita(
     &       filename, phi, rx, ry, vx, vy, r, dtau, taumax)
        end do
      end do

      ! Simulações para phi <= -3
      do i = 5,7
        phi = expoentes(i)

        do j = 1,3
          vy = velocidades(j)

          write(filename, '(A,I1,A,I1,A)')
     &       'tarefa-2c-saida-', i, '-', j, '.dat'

          call simulacao_orbita(
     &       filename, phi, rx, ry, vx, vy, r, dtau, taumax)
        end do
      end do

      end program

      subroutine simulacao_orbita(
     &   filename, phi, rx, ry, vx, vy, r, dtau, taumax)
      ! Simulação de um corpo sofrendo força central da forma 
      ! F = - rho^phi pelo Sol via método de Verlet
      ! Retorna tau, rx, ry
      implicit none
      character(30) filename
      real*8 phi
      real*8 rx(50000), ry(50000), vx, vy, r(50000)
      real*8 periodo
      real*8 tau, dtau, taumax
      real*8 aceleracao
      integer*4 i, nsteps

      ! Número de passos da simulação
      nsteps = int(taumax/dtau)

      ! Para o método de Verlet, precisamos de uma segunda posição inicial
      ! Calculamos os valores de rx(1), ry(1), vx(1), vy(1)
      ! por meio do método de Euler para calcular a posição do passo i=2
      open(10, file=filename) ! Posições e velocidades

      periodo = -1.0d0 ! Inicializa o período como -1 para indicar que ainda não foi calculado

      write(10,*) 0.0d0, rx(1), ry(1)

      rx(2) = rx(1) + vx*dtau
      ry(2) = ry(1) + vy*dtau
      do i = 2, nsteps

        ! Atualização da distância
        r(i) = sqrt(rx(i)**2 + ry(i)**2)

        ! Interrompe a simulacao quando ocorre aproximacao
        ! excessiva do Sol central
        if (r(i) .lt. 0.2d0) then
          print *, 'Aproximacao excessiva do Sol central.'
          print *, 'phi = ', phi, ' vy = ', vy
          exit
        end if

        ! Atualização das posições
        rx(i+1) = 2.0d0*rx(i) - rx(i-1) +
     &   aceleracao(phi, rx(i), r(i))*dtau**2
        ry(i+1) = 2.0d0*ry(i) - ry(i-1) +
     &   aceleracao(phi, ry(i), r(i))*dtau**2

        tau = (i-1)*dtau
        write(10,*) tau, rx(i), ry(i)

        ! Calcula o período da órbita quando a posição cruza o eixo x positivo
        if (ry(i-1) < 0.0d0 .and.
     &      ry(i) >= 0.0d0 .and.
     &      rx(i) > 0.0d0 .and.
     &      periodo .lt. 0.0d0) then
            periodo = tau
        end if

      end do

      if (periodo .gt. 0.0d0) then
        print *, 'Periodo da orbita: ', periodo
      else
        print *, 'A orbita nao eh fechada => periodo nao definido.'
      end if

      close(10)
      end subroutine

      function aceleracao(phi, r_componente, r)
      ! aceleração na direção de r_componente oriunda de uma força central da forma F = - rho^phi 
      ! onde rho = r/UA, phi é o expoente da força central, r_componente é a componente da posição (rx ou ry)
      ! e r é a distância total (raiz quadrada da soma dos quadrados das componentes)
        implicit none
        real*8 phi, r_componente, r
        real*8 aceleracao
        aceleracao = - (r_componente / r**(-phi + 1.0d0))
      return
      end function

      function energia(rx, ry, vx, vy)
      ! Calcula a energia total (cinética + potencial) de um corpo em órbita
      ! em torno de um corpo central, dado a posição (rx, ry) e velocidade (vx, vy)
      ! Unidade: GM⊙m/UA, onde m é a massa do planeta
      
        implicit none
        real*8 rx, ry, vx, vy
        real*8 energia

        ! Energia cinética
        energia = 0.5d0 * (vx**2 + vy**2)

        ! Energia potencial gravitacional
        energia = energia - 1.0d0 / sqrt(rx**2 + ry**2)

      return
      end function