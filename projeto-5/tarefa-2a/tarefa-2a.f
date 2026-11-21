      program tarefa2a
      ! O problema de dois corpos: órbitas limitadas com método de Verlet.
      ! Verifica as condições que geram órbitas fechadas
      ! Verifica as 3 leis de Kepler para órbitas elípticas.
      ! Utiliza-se a 3a Lei de Kepler GM⊙ = 4pi^2 UA^3 ano^(−2).
      ! Grandezas adimensionais rho = r/UA, tau = 2pi*t/ano, v = v/(2pi*UA/ano).
      ! Toma-se o Sol como origem estática e a Terra como corpo em órbita.

      implicit none

      character(30) filename
      integer*4 i
      real*8 taumax, dtau
      real*8 rx(50000), ry(50000), vx(50000), vy(50000), r(50000)
      real*8 pi
      parameter (pi = 3.14159265358979323d0)

      ! Condições iniciais adimensionais
      rx(1) = 1.0d0 ! posição inicial no eixo x
      ry(1) = 0.0d0 ! posição inicial no eixo y
      vx(1) = 0.0d0 ! velocidade inicial no eixo x

      ! Parâmetros da simulação
      taumax = 12.0d0 * pi ! Tempo adimensional máximo
      dtau = 0.001d0 ! passo de tempo adimensional

      ! Varia a vy(1) para gerar 3 elipses, 1 parábola e 1 hipérbole
      do i = 1,5
        if (i .le. 3) then ! 3 elipses
          vy(1) = 1.0d0 + 0.1d0*dble(i)
        else if (i .eq. 4) then ! parábola
          vy(1) = sqrt(2.0d0)
        else ! hipérbole
          vy(1) = 1.5d0
        end if

        write(filename, '(A,I1,A)') 'tarefa-2a-saida-', i, '.dat'

        call simulacao_orbita(
     &       filename, rx, ry, vx, vy, r, dtau, taumax)
      end do

      end program

      subroutine simulacao_orbita(
     &   filename, rx, ry, vx, vy, r, dtau, taumax)
      ! Simulação via método de Verlet
      ! Retorna tau, rx, ry, vx, vy, area
      implicit none
      character(30) filename
      real*8 rx(50000), ry(50000), vx(50000), vy(50000), r(50000)
      real*8 area, periodo
      real*8 tau, dtau, taumax
      real*8 energia_inicial, a
      real*8 energia
      integer*4 i, nsteps

      energia_inicial = energia(rx(1), ry(1), vx(1), vy(1))
      a = - 1.0d0 / (2.0d0 * energia_inicial) ! semieixo maior da órbita elíptica

      ! Número de passos da simulação
      nsteps = int(taumax/dtau)

      ! Para o método de Verlet, precisamos de uma segunda posição inicial
      ! Calculamos os valores de rx(1), ry(1), vx(1), vy(1)
      ! por meio do método de Euler para calcular a posição do passo i=2
      open(10, file=filename) ! Posições e velocidades

      area = 0.0d0
      periodo = -1.0d0 ! Inicializa o período como -1 para indicar que ainda não foi calculado

      write(10,*) 0.0d0, rx(1), ry(1), vx(1), vy(1), area

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

        ! Cálculo da área varrida pelo vetor posição
        area = area + dtau * 0.5d0 * (rx(i)*vy(i) - ry(i)*vx(i))

        tau = (i-1)*dtau
        write(10,*) tau, rx(i), ry(i), vx(i), vy(i), area

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
        print *, 'Semieixo maior da orbita: ', a
        print *, 'T^2/a^3: ', periodo**2 / a**3
      else
        print *, 'A orbita nao eh fechada => periodo nao definido.'
      end if

      close(10)
      end subroutine

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