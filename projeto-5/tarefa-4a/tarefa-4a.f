      program tarefa4a
      ! Coreografia celeste: problema de Lagrange com método Runge-Kutta de 4a ordem.
      ! Retorna ρ(τ) de três partículas gravitando em um triângulo equilátero
      ! Utiliza-se a 3a Lei de Kepler GM⊙ = 4pi^2 UA^3 ano^(−2).
      ! Grandezas adimensionais rho = r/UA, tau = 2pi*t/ano, v = v/(2pi*UA/ano).

      implicit none

      character(30) filename
      real*8 taumax, dtau
      real*8 mu_i, mu_ii, mu_iii
      real*8 v0
      real*8 rx_i(200000), ry_i(200000), vx_i, vy_i
      real*8 rx_ii(200000), ry_ii(200000), vx_ii, vy_ii
      real*8 rx_iii(200000), ry_iii(200000), vx_iii, vy_iii
      real*8 pi
      parameter (pi = 3.14159265358979323d0)

      ! Massas das partículas (adimensionais com o Sol)
      mu_i = 1.0d0
      mu_ii = 1.0d0
      mu_iii = 1.0d0

      ! Condições iniciais adimensionais
      ! Posições das partículas
      rx_i(1) = 1.0d0 ! posição inicial no eixo x
      rx_ii(1) = - 0.5d0
      rx_iii(1) = - 0.5d0
      ry_i(1) = 0.0d0 ! posição inicial no eixo y
      ry_ii(1) = sqrt(3.0d0)/2.0d0
      ry_iii(1) = - sqrt(3.0d0)/2.0d0

      ! Velocidades das partículas
      v0 = 3d0**(-0.25d0) ! constante de normalização das velocidades iniciais
      vx_i = 0.0d0 ! velocidade inicial no eixo x
      vx_ii = - sqrt(3.0d0)/2.0d0 * v0
      vx_iii = sqrt(3.0d0)/2.0d0 * v0
      vy_i = v0 ! velocidade inicial no eixo y
      vy_ii = - 0.5d0 * v0
      vy_iii = - 0.5d0 * v0

      ! Parâmetros da simulação
      taumax = 5.0d0 * 2.0d0 * pi ! 5 anos de simulação
      dtau = 0.001d0 ! passo de tempo adimensional

      ! Nome do arquivo de saída
      filename = 'tarefa-4a-saida-1.dat'

      ! Chamada da subrotina de simulação
      call simulacao_orbita(
     &   filename,
     &   rx_i, ry_i, vx_i, vy_i,
     &   rx_ii, ry_ii, vx_ii, vy_ii,
     &   rx_iii, ry_iii, vx_iii, vy_iii,
     &   mu_i, mu_ii, mu_iii, dtau, taumax)

      end program


      subroutine simulacao_orbita(
     &   filename,
     &   rx_i, ry_i, vx_i, vy_i,
     &   rx_ii, ry_ii, vx_ii, vy_ii,
     &   rx_iii, ry_iii, vx_iii, vy_iii,
     &   mu_i, mu_ii, mu_iii, dtau, taumax)
      ! Simulacao via metodo de Runge-Kutta de quarta ordem
      ! Considera apenas a interacao gravitacional entre as tres particulas
      ! Retorna tau, rx, ry das particulas em um arquivo de saida
      implicit none
      character(30) filename

      real*8 rx_i(200000), ry_i(200000), vx_i, vy_i
      real*8 rx_ii(200000), ry_ii(200000), vx_ii, vy_ii
      real*8 rx_iii(200000), ry_iii(200000), vx_iii, vy_iii

      real*8 mu_i, mu_ii, mu_iii
      real*8 tau, dtau, taumax
      real*8 y(12), yt(12)
      real*8 k1(12), k2(12), k3(12), k4(12)
      integer*4 i, j, nsteps

      ! Numero de passos da simulacao
      nsteps = nint(taumax/dtau)

      ! Vetor de estado inicial
      y(1) = rx_i(1)
      y(2) = ry_i(1)
      y(3) = vx_i
      y(4) = vy_i

      y(5) = rx_ii(1)
      y(6) = ry_ii(1)
      y(7) = vx_ii
      y(8) = vy_ii

      y(9) = rx_iii(1)
      y(10) = ry_iii(1)
      y(11) = vx_iii
      y(12) = vy_iii

      ! Arquivo de saida
      open(10, file=filename, status='replace')

      write(10,*) 0.0d0,
     &   y(1), y(2), y(5), y(6), y(9), y(10)

      ! Integracao pelo metodo de Runge-Kutta de quarta ordem
      do i = 1, nsteps

        ! Primeiro estagio
        call derivadas(y, mu_i, mu_ii, mu_iii, k1)

        ! Segundo estagio
        do j = 1, 12
          yt(j) = y(j) + 0.5d0*dtau*k1(j)
        end do

        call derivadas(yt, mu_i, mu_ii, mu_iii, k2)

        ! Terceiro estagio
        do j = 1, 12
          yt(j) = y(j) + 0.5d0*dtau*k2(j)
        end do

        call derivadas(yt, mu_i, mu_ii, mu_iii, k3)

        ! Quarto estagio
        do j = 1, 12
          yt(j) = y(j) + dtau*k3(j)
        end do

        call derivadas(yt, mu_i, mu_ii, mu_iii, k4)

        ! Atualizacao do vetor de estado
        do j = 1, 12
          y(j) = y(j) + (dtau/6.0d0)*
     &       (k1(j) + 2.0d0*k2(j)
     &       + 2.0d0*k3(j) + k4(j))
        end do

        ! Armazenamento das posicoes
        rx_i(i+1) = y(1)
        ry_i(i+1) = y(2)

        rx_ii(i+1) = y(5)
        ry_ii(i+1) = y(6)

        rx_iii(i+1) = y(9)
        ry_iii(i+1) = y(10)

        ! Escrita no arquivo de saida
        tau = dble(i)*dtau

        write(10,*) tau,
     &       rx_i(i+1), ry_i(i+1),
     &       rx_ii(i+1), ry_ii(i+1),
     &       rx_iii(i+1), ry_iii(i+1)

      end do

      close(10)
      end subroutine

      
      subroutine derivadas(y, mu_i, mu_ii, mu_iii, dydtau)
      ! Calcula as derivadas do vetor de estado
      ! Sistema gravitacional de tres particulas
      implicit none

      real*8 y(12), dydtau(12)
      real*8 mu_i, mu_ii, mu_iii
      real*8 aceleracao

      ! Particula I
      dydtau(1) = y(3)
      dydtau(2) = y(4)

      dydtau(3) = mu_ii*aceleracao('x',
     &   y(1), y(2), y(5), y(6))
     &   + mu_iii*aceleracao('x',
     &   y(1), y(2), y(9), y(10))

      dydtau(4) = mu_ii*aceleracao('y',
     &   y(1), y(2), y(5), y(6))
     &   + mu_iii*aceleracao('y',
     &   y(1), y(2), y(9), y(10))

      ! Particula II
      dydtau(5) = y(7)
      dydtau(6) = y(8)

      dydtau(7) = mu_i*aceleracao('x',
     &   y(5), y(6), y(1), y(2))
     &   + mu_iii*aceleracao('x',
     &   y(5), y(6), y(9), y(10))

      dydtau(8) = mu_i*aceleracao('y',
     &   y(5), y(6), y(1), y(2))
     &   + mu_iii*aceleracao('y',
     &   y(5), y(6), y(9), y(10))

      ! Particula III
      dydtau(9) = y(11)
      dydtau(10) = y(12)

      dydtau(11) = mu_i*aceleracao('x',
     &   y(9), y(10), y(1), y(2))
     &   + mu_ii*aceleracao('x',
     &   y(9), y(10), y(5), y(6))

      dydtau(12) = mu_i*aceleracao('y',
     &   y(9), y(10), y(1), y(2))
     &   + mu_ii*aceleracao('y',
     &   y(9), y(10), y(5), y(6))

      return
      end subroutine


      function aceleracao(
     &   componente, rx_corpo_1, ry_corpo_1, rx_corpo_2, ry_corpo_2)
      ! Calcula a aceleração de um corpo devido à gravidade de outro corpo
      ! componente: 'x' para x, 'y' para y
      ! rx_corpo_1, ry_corpo_1: posição do corpo que sofre a aceleração
      ! rx_corpo_2, ry_corpo_2: posição do corpo que exerce a força gravitacional
      implicit none
      character*1 componente
      real*8 rx_corpo_1, ry_corpo_1, rx_corpo_2, ry_corpo_2
      real*8 aceleracao, r
      r = sqrt((rx_corpo_2 - rx_corpo_1)**2 + 
     & (ry_corpo_2 - ry_corpo_1)**2)
      if (componente .eq. 'x') then
        aceleracao = - (rx_corpo_1 - rx_corpo_2) / r**3
      else if (componente .eq. 'y') then
        aceleracao = - (ry_corpo_1 - ry_corpo_2) / r**3
      end if
      return
      end function