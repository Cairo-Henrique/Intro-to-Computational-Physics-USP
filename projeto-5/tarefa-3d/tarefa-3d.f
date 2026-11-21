      program tarefa3d
      ! O problema de vários corpos: Sol, Júpiter e asteroides com método de Verlet.
      ! Retorna ρ(τ) de três asteroides no cinturão entre Marte e Júpiter
      ! Utiliza-se a 3a Lei de Kepler GM⊙ = 4pi^2 UA^3 ano^(−2).
      ! Grandezas adimensionais rho = r/UA, tau = 2pi*t/ano, v = v/(2pi*UA/ano).
      ! Toma-se o Sol como origem estática e outros corpos em órbita.

      implicit none

      character(30) filename
      real*8 taumax, dtau
      real*8 mu_jupiter
      real*8 rx_i(200000), ry_i(200000), vx_i, vy_i
      real*8 rx_ii(200000), ry_ii(200000), vx_ii, vy_ii
      real*8 rx_iii(200000), ry_iii(200000), vx_iii, vy_iii
      real*8 rx_jupiter(200000), ry_jupiter(200000), 
     & vx_jupiter, vy_jupiter
      real*8 pi
      parameter (pi = 3.14159265358979323d0)

      ! Massas adimensionais dos corpos
      mu_jupiter = 9.545d-4 ! massa de Júpiter

      ! Condições iniciais adimensionais
      ! Asteroides
      rx_i(1) = 3.000d0 ! posição inicial no eixo x
      rx_ii(1) = 3.276d0
      rx_iii(1) = 3.700d0
      ry_i(1) = 0.0d0 ! posição inicial no eixo y
      ry_ii(1) = 0.0d0
      ry_iii(1) = 0.0d0
      vx_i = 0.0d0 ! velocidade inicial no eixo x
      vx_ii = 0.0d0
      vx_iii = 0.0d0
      vy_i = 3.628d0 / (2.0d0 * pi) ! velocidade inicial no eixo y
      vy_ii = 3.471d0 / (2.0d0 * pi)
      vy_iii = 3.267d0 / (2.0d0 * pi)
      ! Júpiter
      rx_jupiter(1) = 5.2d0 ! posição inicial no eixo x
      ry_jupiter(1) = 0.0d0 ! posição inicial no eixo y
      vx_jupiter = 0.0d0 ! velocidade inicial no eixo x
      vy_jupiter = 2.755 / (2.0d0 * pi) ! velocidade inicial no eixo y (órbita circular)

      ! Parâmetros da simulação
      taumax = 30.0d0 * 2.0d0 * pi ! 30 anos de simulação
      dtau = 0.001 ! passo de tempo adimensional

      ! Nome do arquivo de saída
      filename = 'tarefa-3d-saida-1.dat'

      ! Chamada da subrotina de simulação
      call simulacao_orbita(
     &   filename,
     &   rx_i, ry_i, vx_i, vy_i,
     &   rx_ii, ry_ii, vx_ii, vy_ii,
     &   rx_iii, ry_iii, vx_iii, vy_iii,
     &   rx_jupiter, ry_jupiter, vx_jupiter, vy_jupiter,
     &   mu_jupiter, dtau, taumax)

      end program

      subroutine simulacao_orbita(
     &   filename,
     &   rx_i, ry_i, vx_i, vy_i,
     &   rx_ii, ry_ii, vx_ii, vy_ii,
     &   rx_iii, ry_iii, vx_iii, vy_iii,
     &   rx_jupiter, ry_jupiter, vx_jupiter, vy_jupiter,
     &   mu_jupiter, dtau, taumax)
      ! Simulação via método de Verlet
      ! Considera apenas a interação gravitacional do Sol e de Júpiter sobre os asteroides
      ! Retorna tau, rx, ry dos asteroides e de Júpiter
      implicit none
      character(30) filename
      real*8 rx_sol, ry_sol
      parameter (rx_sol = 0.0d0, ry_sol = 0.0d0)
      real*8 rx_i(200000), ry_i(200000), vx_i, vy_i
      real*8 rx_ii(200000), ry_ii(200000), vx_ii, vy_ii
      real*8 rx_iii(200000), ry_iii(200000), vx_iii, vy_iii
      real*8 rx_jupiter(200000), ry_jupiter(200000)
      real*8 vx_jupiter, vy_jupiter
      real*8 tau, dtau, taumax
      real*8 mu_jupiter
      real*8 aceleracao
      real*8 pi
      parameter (pi = 3.14159265358979323d0)
      integer*4 i, nsteps, nperyear

      ! Número de dtau's no ano
      nperyear = nint(2.0d0*pi/dtau)

      ! Número de passos da simulação
      nsteps = nint(taumax/dtau)

      ! Inicialização das posições para o método de Verlet
      open(10, file=filename)

      write(10,*) 0.0d0,
     &   rx_i(1), ry_i(1),
     &   rx_ii(1), ry_ii(1),
     &   rx_iii(1), ry_iii(1),
     &   rx_jupiter(1), ry_jupiter(1)

      rx_i(2) = rx_i(1) + vx_i*dtau
      ry_i(2) = ry_i(1) + vy_i*dtau

      rx_ii(2) = rx_ii(1) + vx_ii*dtau
      ry_ii(2) = ry_ii(1) + vy_ii*dtau

      rx_iii(2) = rx_iii(1) + vx_iii*dtau
      ry_iii(2) = ry_iii(1) + vy_iii*dtau

      rx_jupiter(2) = rx_jupiter(1) + vx_jupiter*dtau
      ry_jupiter(2) = ry_jupiter(1) + vy_jupiter*dtau

      do i = 2, nsteps

        ! Atualização das posições do Asteroide I
        rx_i(i+1) = 2.0d0*rx_i(i) - rx_i(i-1)
     &       + aceleracao('x', rx_i(i), ry_i(i),
     &         rx_sol, ry_sol)*dtau**2
     &       + mu_jupiter*aceleracao('x',
     &         rx_i(i), ry_i(i),
     &         rx_jupiter(i), ry_jupiter(i))*dtau**2

        ry_i(i+1) = 2.0d0*ry_i(i) - ry_i(i-1)
     &       + aceleracao('y', rx_i(i), ry_i(i),
     &         rx_sol, ry_sol)*dtau**2
     &       + mu_jupiter*aceleracao('y',
     &         rx_i(i), ry_i(i),
     &         rx_jupiter(i), ry_jupiter(i))*dtau**2

        ! Atualização das posições do Asteroide II
        rx_ii(i+1) = 2.0d0*rx_ii(i) - rx_ii(i-1)
     &       + aceleracao('x', rx_ii(i), ry_ii(i),
     &         rx_sol, ry_sol)*dtau**2
     &       + mu_jupiter*aceleracao('x',
     &         rx_ii(i), ry_ii(i),
     &         rx_jupiter(i), ry_jupiter(i))*dtau**2

        ry_ii(i+1) = 2.0d0*ry_ii(i) - ry_ii(i-1)
     &       + aceleracao('y', rx_ii(i), ry_ii(i),
     &         rx_sol, ry_sol)*dtau**2
     &       + mu_jupiter*aceleracao('y',
     &         rx_ii(i), ry_ii(i),
     &         rx_jupiter(i), ry_jupiter(i))*dtau**2

        ! Atualização das posições do Asteroide III
        rx_iii(i+1) = 2.0d0*rx_iii(i) - rx_iii(i-1)
     &       + aceleracao('x', rx_iii(i), ry_iii(i),
     &         rx_sol, ry_sol)*dtau**2
     &       + mu_jupiter*aceleracao('x',
     &         rx_iii(i), ry_iii(i),
     &         rx_jupiter(i), ry_jupiter(i))*dtau**2

        ry_iii(i+1) = 2.0d0*ry_iii(i) - ry_iii(i-1)
     &       + aceleracao('y', rx_iii(i), ry_iii(i),
     &         rx_sol, ry_sol)*dtau**2
     &       + mu_jupiter*aceleracao('y',
     &         rx_iii(i), ry_iii(i),
     &         rx_jupiter(i), ry_jupiter(i))*dtau**2

        ! Atualização das posições de Júpiter
        ! Júpiter sofre apenas a atração gravitacional do Sol
        rx_jupiter(i+1) = 2.0d0*rx_jupiter(i)
     &       - rx_jupiter(i-1)
     &       + aceleracao('x', rx_jupiter(i), ry_jupiter(i),
     &         rx_sol, ry_sol)*dtau**2

        ry_jupiter(i+1) = 2.0d0*ry_jupiter(i)
     &       - ry_jupiter(i-1)
     &       + aceleracao('y', rx_jupiter(i), ry_jupiter(i),
     &         rx_sol, ry_sol)*dtau**2

        ! Escreve no arquivo de saída as posições dos corpos
        tau = (i-1)*dtau
        write(10,*) tau,
     &       rx_i(i), ry_i(i),
     &       rx_ii(i), ry_ii(i),
     &       rx_iii(i), ry_iii(i),
     &       rx_jupiter(i), ry_jupiter(i)

      end do

      close(10)
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