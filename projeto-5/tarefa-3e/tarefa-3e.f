
      program tarefa3e
      ! O problema de varios corpos: sistema solar com metodo de Verlet.
      ! Retorna rho(tau) dos oito planetas.
      ! Utiliza-se a 3a Lei de Kepler GM_sol = 4pi^2 UA^3 ano^(-2).
      ! Grandezas adimensionais rho = r/UA, tau = 2pi*t/ano,
      ! v = v/(2pi*UA/ano).
      ! Toma-se o Sol como origem estatica.

      implicit none

      character(30) filename
      real*8 taumax, dtau

      real*8 mu_mercurio, mu_venus, mu_terra, mu_marte
      real*8 mu_jupiter, mu_saturno, mu_urano, mu_netuno

      real*8 rx_mercurio(200000), ry_mercurio(200000)
      real*8 vx_mercurio, vy_mercurio

      real*8 rx_venus(200000), ry_venus(200000)
      real*8 vx_venus, vy_venus

      real*8 rx_terra(200000), ry_terra(200000)
      real*8 vx_terra, vy_terra

      real*8 rx_marte(200000), ry_marte(200000)
      real*8 vx_marte, vy_marte

      real*8 rx_jupiter(200000), ry_jupiter(200000)
      real*8 vx_jupiter, vy_jupiter

      real*8 rx_saturno(200000), ry_saturno(200000)
      real*8 vx_saturno, vy_saturno

      real*8 rx_urano(200000), ry_urano(200000)
      real*8 vx_urano, vy_urano

      real*8 rx_netuno(200000), ry_netuno(200000)
      real*8 vx_netuno, vy_netuno

      real*8 pi
      parameter (pi = 3.14159265358979323d0)

      ! Massas adimensionais dos planetas: M_planeta/M_sol
      mu_mercurio = 1.6601d-7
      mu_venus = 2.4478d-6
      mu_terra = 3.0035d-6
      mu_marte = 3.2272d-7
      mu_jupiter = 9.5479d-4
      mu_saturno = 2.8589d-4
      mu_urano = 4.3662d-5
      mu_netuno = 5.1514d-5

      ! Condicoes iniciais adimensionais
      ! Todos os planetas com orbitas inicialmente circulares
      ! e alinhados sobre o eixo x.

      ! Mercurio
      rx_mercurio(1) = 0.3871d0
      ry_mercurio(1) = 0.0d0
      vx_mercurio = 0.0d0
      vy_mercurio = 1.0d0/sqrt(rx_mercurio(1))

      ! Venus
      rx_venus(1) = 0.7233d0
      ry_venus(1) = 0.0d0
      vx_venus = 0.0d0
      vy_venus = 1.0d0/sqrt(rx_venus(1))

      ! Terra
      rx_terra(1) = 1.0000d0
      ry_terra(1) = 0.0d0
      vx_terra = 0.0d0
      vy_terra = 1.0d0/sqrt(rx_terra(1))

      ! Marte
      rx_marte(1) = 1.5237d0
      ry_marte(1) = 0.0d0
      vx_marte = 0.0d0
      vy_marte = 1.0d0/sqrt(rx_marte(1))

      ! Jupiter
      rx_jupiter(1) = 5.2026d0
      ry_jupiter(1) = 0.0d0
      vx_jupiter = 0.0d0
      vy_jupiter = 1.0d0/sqrt(rx_jupiter(1))

      ! Saturno
      rx_saturno(1) = 9.5549d0
      ry_saturno(1) = 0.0d0
      vx_saturno = 0.0d0
      vy_saturno = 1.0d0/sqrt(rx_saturno(1))

      ! Urano
      rx_urano(1) = 19.2184d0
      ry_urano(1) = 0.0d0
      vx_urano = 0.0d0
      vy_urano = 1.0d0/sqrt(rx_urano(1))

      ! Netuno
      rx_netuno(1) = 30.1104d0
      ry_netuno(1) = 0.0d0
      vx_netuno = 0.0d0
      vy_netuno = 1.0d0/sqrt(rx_netuno(1))

      ! Parametros da simulacao
      taumax = 30.0d0*2.0d0*pi
      dtau = 0.001d0

      ! Nome do arquivo de saida
      filename = 'tarefa-3e-saida-1.dat'

      ! Chamada da subrotina de simulacao
      call simulacao_orbita(
     &   filename,
     &   rx_mercurio, ry_mercurio, vx_mercurio, vy_mercurio,
     &   rx_venus, ry_venus, vx_venus, vy_venus,
     &   rx_terra, ry_terra, vx_terra, vy_terra,
     &   rx_marte, ry_marte, vx_marte, vy_marte,
     &   rx_jupiter, ry_jupiter, vx_jupiter, vy_jupiter,
     &   rx_saturno, ry_saturno, vx_saturno, vy_saturno,
     &   rx_urano, ry_urano, vx_urano, vy_urano,
     &   rx_netuno, ry_netuno, vx_netuno, vy_netuno,
     &   mu_mercurio, mu_venus, mu_terra, mu_marte,
     &   mu_jupiter, mu_saturno, mu_urano, mu_netuno,
     &   dtau, taumax)

      end program


      subroutine simulacao_orbita(
     &   filename,
     &   rx_mercurio, ry_mercurio, vx_mercurio, vy_mercurio,
     &   rx_venus, ry_venus, vx_venus, vy_venus,
     &   rx_terra, ry_terra, vx_terra, vy_terra,
     &   rx_marte, ry_marte, vx_marte, vy_marte,
     &   rx_jupiter, ry_jupiter, vx_jupiter, vy_jupiter,
     &   rx_saturno, ry_saturno, vx_saturno, vy_saturno,
     &   rx_urano, ry_urano, vx_urano, vy_urano,
     &   rx_netuno, ry_netuno, vx_netuno, vy_netuno,
     &   mu_mercurio, mu_venus, mu_terra, mu_marte,
     &   mu_jupiter, mu_saturno, mu_urano, mu_netuno,
     &   dtau, taumax)

      ! Simulacao via metodo de Verlet
      ! Cada planeta sofre a atracao gravitacional do Sol
      ! e dos outros sete planetas.

      implicit none
      character(30) filename

      real*8 rx_sol, ry_sol
      parameter (rx_sol = 0.0d0, ry_sol = 0.0d0)

      real*8 rx_mercurio(200000), ry_mercurio(200000)
      real*8 vx_mercurio, vy_mercurio

      real*8 rx_venus(200000), ry_venus(200000)
      real*8 vx_venus, vy_venus

      real*8 rx_terra(200000), ry_terra(200000)
      real*8 vx_terra, vy_terra

      real*8 rx_marte(200000), ry_marte(200000)
      real*8 vx_marte, vy_marte

      real*8 rx_jupiter(200000), ry_jupiter(200000)
      real*8 vx_jupiter, vy_jupiter

      real*8 rx_saturno(200000), ry_saturno(200000)
      real*8 vx_saturno, vy_saturno

      real*8 rx_urano(200000), ry_urano(200000)
      real*8 vx_urano, vy_urano

      real*8 rx_netuno(200000), ry_netuno(200000)
      real*8 vx_netuno, vy_netuno

      real*8 mu_mercurio, mu_venus, mu_terra, mu_marte
      real*8 mu_jupiter, mu_saturno, mu_urano, mu_netuno

      real*8 tau, dtau, taumax
      real*8 aceleracao

      integer*4 i, nsteps

      ! Numero de passos da simulacao
      nsteps = nint(taumax/dtau)

      ! Inicializacao das posicoes para o metodo de Verlet
      open(10, file=filename)

      write(10,*) 0.0d0,
     &   rx_mercurio(1), ry_mercurio(1),
     &   rx_venus(1), ry_venus(1),
     &   rx_terra(1), ry_terra(1),
     &   rx_marte(1), ry_marte(1),
     &   rx_jupiter(1), ry_jupiter(1),
     &   rx_saturno(1), ry_saturno(1),
     &   rx_urano(1), ry_urano(1),
     &   rx_netuno(1), ry_netuno(1)

      rx_mercurio(2) = rx_mercurio(1) + vx_mercurio*dtau
      ry_mercurio(2) = ry_mercurio(1) + vy_mercurio*dtau

      rx_venus(2) = rx_venus(1) + vx_venus*dtau
      ry_venus(2) = ry_venus(1) + vy_venus*dtau

      rx_terra(2) = rx_terra(1) + vx_terra*dtau
      ry_terra(2) = ry_terra(1) + vy_terra*dtau

      rx_marte(2) = rx_marte(1) + vx_marte*dtau
      ry_marte(2) = ry_marte(1) + vy_marte*dtau

      rx_jupiter(2) = rx_jupiter(1) + vx_jupiter*dtau
      ry_jupiter(2) = ry_jupiter(1) + vy_jupiter*dtau

      rx_saturno(2) = rx_saturno(1) + vx_saturno*dtau
      ry_saturno(2) = ry_saturno(1) + vy_saturno*dtau

      rx_urano(2) = rx_urano(1) + vx_urano*dtau
      ry_urano(2) = ry_urano(1) + vy_urano*dtau

      rx_netuno(2) = rx_netuno(1) + vx_netuno*dtau
      ry_netuno(2) = ry_netuno(1) + vy_netuno*dtau

      do i = 2, nsteps

        ! Atualizacao das posicoes de Mercurio
        rx_mercurio(i+1) = 2.0d0*rx_mercurio(i)
     &       - rx_mercurio(i-1)
     &       + aceleracao('x', rx_mercurio(i),
     &         ry_mercurio(i), rx_sol, ry_sol)*dtau**2
     &       + mu_venus*aceleracao('x',
     &         rx_mercurio(i), ry_mercurio(i),
     &         rx_venus(i), ry_venus(i))*dtau**2
     &       + mu_terra*aceleracao('x',
     &         rx_mercurio(i), ry_mercurio(i),
     &         rx_terra(i), ry_terra(i))*dtau**2
     &       + mu_marte*aceleracao('x',
     &         rx_mercurio(i), ry_mercurio(i),
     &         rx_marte(i), ry_marte(i))*dtau**2
     &       + mu_jupiter*aceleracao('x',
     &         rx_mercurio(i), ry_mercurio(i),
     &         rx_jupiter(i), ry_jupiter(i))*dtau**2
     &       + mu_saturno*aceleracao('x',
     &         rx_mercurio(i), ry_mercurio(i),
     &         rx_saturno(i), ry_saturno(i))*dtau**2
     &       + mu_urano*aceleracao('x',
     &         rx_mercurio(i), ry_mercurio(i),
     &         rx_urano(i), ry_urano(i))*dtau**2
     &       + mu_netuno*aceleracao('x',
     &         rx_mercurio(i), ry_mercurio(i),
     &         rx_netuno(i), ry_netuno(i))*dtau**2

        ry_mercurio(i+1) = 2.0d0*ry_mercurio(i)
     &       - ry_mercurio(i-1)
     &       + aceleracao('y', rx_mercurio(i),
     &         ry_mercurio(i), rx_sol, ry_sol)*dtau**2
     &       + mu_venus*aceleracao('y',
     &         rx_mercurio(i), ry_mercurio(i),
     &         rx_venus(i), ry_venus(i))*dtau**2
     &       + mu_terra*aceleracao('y',
     &         rx_mercurio(i), ry_mercurio(i),
     &         rx_terra(i), ry_terra(i))*dtau**2
     &       + mu_marte*aceleracao('y',
     &         rx_mercurio(i), ry_mercurio(i),
     &         rx_marte(i), ry_marte(i))*dtau**2
     &       + mu_jupiter*aceleracao('y',
     &         rx_mercurio(i), ry_mercurio(i),
     &         rx_jupiter(i), ry_jupiter(i))*dtau**2
     &       + mu_saturno*aceleracao('y',
     &         rx_mercurio(i), ry_mercurio(i),
     &         rx_saturno(i), ry_saturno(i))*dtau**2
     &       + mu_urano*aceleracao('y',
     &         rx_mercurio(i), ry_mercurio(i),
     &         rx_urano(i), ry_urano(i))*dtau**2
     &       + mu_netuno*aceleracao('y',
     &         rx_mercurio(i), ry_mercurio(i),
     &         rx_netuno(i), ry_netuno(i))*dtau**2

        ! Atualizacao das posicoes de Venus
        rx_venus(i+1) = 2.0d0*rx_venus(i)
     &       - rx_venus(i-1)
     &       + aceleracao('x', rx_venus(i),
     &         ry_venus(i), rx_sol, ry_sol)*dtau**2
     &       + mu_mercurio*aceleracao('x',
     &         rx_venus(i), ry_venus(i),
     &         rx_mercurio(i), ry_mercurio(i))*dtau**2
     &       + mu_terra*aceleracao('x',
     &         rx_venus(i), ry_venus(i),
     &         rx_terra(i), ry_terra(i))*dtau**2
     &       + mu_marte*aceleracao('x',
     &         rx_venus(i), ry_venus(i),
     &         rx_marte(i), ry_marte(i))*dtau**2
     &       + mu_jupiter*aceleracao('x',
     &         rx_venus(i), ry_venus(i),
     &         rx_jupiter(i), ry_jupiter(i))*dtau**2
     &       + mu_saturno*aceleracao('x',
     &         rx_venus(i), ry_venus(i),
     &         rx_saturno(i), ry_saturno(i))*dtau**2
     &       + mu_urano*aceleracao('x',
     &         rx_venus(i), ry_venus(i),
     &         rx_urano(i), ry_urano(i))*dtau**2
     &       + mu_netuno*aceleracao('x',
     &         rx_venus(i), ry_venus(i),
     &         rx_netuno(i), ry_netuno(i))*dtau**2

        ry_venus(i+1) = 2.0d0*ry_venus(i)
     &       - ry_venus(i-1)
     &       + aceleracao('y', rx_venus(i),
     &         ry_venus(i), rx_sol, ry_sol)*dtau**2
     &       + mu_mercurio*aceleracao('y',
     &         rx_venus(i), ry_venus(i),
     &         rx_mercurio(i), ry_mercurio(i))*dtau**2
     &       + mu_terra*aceleracao('y',
     &         rx_venus(i), ry_venus(i),
     &         rx_terra(i), ry_terra(i))*dtau**2
     &       + mu_marte*aceleracao('y',
     &         rx_venus(i), ry_venus(i),
     &         rx_marte(i), ry_marte(i))*dtau**2
     &       + mu_jupiter*aceleracao('y',
     &         rx_venus(i), ry_venus(i),
     &         rx_jupiter(i), ry_jupiter(i))*dtau**2
     &       + mu_saturno*aceleracao('y',
     &         rx_venus(i), ry_venus(i),
     &         rx_saturno(i), ry_saturno(i))*dtau**2
     &       + mu_urano*aceleracao('y',
     &         rx_venus(i), ry_venus(i),
     &         rx_urano(i), ry_urano(i))*dtau**2
     &       + mu_netuno*aceleracao('y',
     &         rx_venus(i), ry_venus(i),
     &         rx_netuno(i), ry_netuno(i))*dtau**2

        ! Atualizacao das posicoes da Terra
        rx_terra(i+1) = 2.0d0*rx_terra(i)
     &       - rx_terra(i-1)
     &       + aceleracao('x', rx_terra(i),
     &         ry_terra(i), rx_sol, ry_sol)*dtau**2
     &       + mu_mercurio*aceleracao('x',
     &         rx_terra(i), ry_terra(i),
     &         rx_mercurio(i), ry_mercurio(i))*dtau**2
     &       + mu_venus*aceleracao('x',
     &         rx_terra(i), ry_terra(i),
     &         rx_venus(i), ry_venus(i))*dtau**2
     &       + mu_marte*aceleracao('x',
     &         rx_terra(i), ry_terra(i),
     &         rx_marte(i), ry_marte(i))*dtau**2
     &       + mu_jupiter*aceleracao('x',
     &         rx_terra(i), ry_terra(i),
     &         rx_jupiter(i), ry_jupiter(i))*dtau**2
     &       + mu_saturno*aceleracao('x',
     &         rx_terra(i), ry_terra(i),
     &         rx_saturno(i), ry_saturno(i))*dtau**2
     &       + mu_urano*aceleracao('x',
     &         rx_terra(i), ry_terra(i),
     &         rx_urano(i), ry_urano(i))*dtau**2
     &       + mu_netuno*aceleracao('x',
     &         rx_terra(i), ry_terra(i),
     &         rx_netuno(i), ry_netuno(i))*dtau**2

        ry_terra(i+1) = 2.0d0*ry_terra(i)
     &       - ry_terra(i-1)
     &       + aceleracao('y', rx_terra(i),
     &         ry_terra(i), rx_sol, ry_sol)*dtau**2
     &       + mu_mercurio*aceleracao('y',
     &         rx_terra(i), ry_terra(i),
     &         rx_mercurio(i), ry_mercurio(i))*dtau**2
     &       + mu_venus*aceleracao('y',
     &         rx_terra(i), ry_terra(i),
     &         rx_venus(i), ry_venus(i))*dtau**2
     &       + mu_marte*aceleracao('y',
     &         rx_terra(i), ry_terra(i),
     &         rx_marte(i), ry_marte(i))*dtau**2
     &       + mu_jupiter*aceleracao('y',
     &         rx_terra(i), ry_terra(i),
     &         rx_jupiter(i), ry_jupiter(i))*dtau**2
     &       + mu_saturno*aceleracao('y',
     &         rx_terra(i), ry_terra(i),
     &         rx_saturno(i), ry_saturno(i))*dtau**2
     &       + mu_urano*aceleracao('y',
     &         rx_terra(i), ry_terra(i),
     &         rx_urano(i), ry_urano(i))*dtau**2
     &       + mu_netuno*aceleracao('y',
     &         rx_terra(i), ry_terra(i),
     &         rx_netuno(i), ry_netuno(i))*dtau**2

        ! Atualizacao das posicoes de Marte
        rx_marte(i+1) = 2.0d0*rx_marte(i)
     &       - rx_marte(i-1)
     &       + aceleracao('x', rx_marte(i),
     &         ry_marte(i), rx_sol, ry_sol)*dtau**2
     &       + mu_mercurio*aceleracao('x',
     &         rx_marte(i), ry_marte(i),
     &         rx_mercurio(i), ry_mercurio(i))*dtau**2
     &       + mu_venus*aceleracao('x',
     &         rx_marte(i), ry_marte(i),
     &         rx_venus(i), ry_venus(i))*dtau**2
     &       + mu_terra*aceleracao('x',
     &         rx_marte(i), ry_marte(i),
     &         rx_terra(i), ry_terra(i))*dtau**2
     &       + mu_jupiter*aceleracao('x',
     &         rx_marte(i), ry_marte(i),
     &         rx_jupiter(i), ry_jupiter(i))*dtau**2
     &       + mu_saturno*aceleracao('x',
     &         rx_marte(i), ry_marte(i),
     &         rx_saturno(i), ry_saturno(i))*dtau**2
     &       + mu_urano*aceleracao('x',
     &         rx_marte(i), ry_marte(i),
     &         rx_urano(i), ry_urano(i))*dtau**2
     &       + mu_netuno*aceleracao('x',
     &         rx_marte(i), ry_marte(i),
     &         rx_netuno(i), ry_netuno(i))*dtau**2

        ry_marte(i+1) = 2.0d0*ry_marte(i)
     &       - ry_marte(i-1)
     &       + aceleracao('y', rx_marte(i),
     &         ry_marte(i), rx_sol, ry_sol)*dtau**2
     &       + mu_mercurio*aceleracao('y',
     &         rx_marte(i), ry_marte(i),
     &         rx_mercurio(i), ry_mercurio(i))*dtau**2
     &       + mu_venus*aceleracao('y',
     &         rx_marte(i), ry_marte(i),
     &         rx_venus(i), ry_venus(i))*dtau**2
     &       + mu_terra*aceleracao('y',
     &         rx_marte(i), ry_marte(i),
     &         rx_terra(i), ry_terra(i))*dtau**2
     &       + mu_jupiter*aceleracao('y',
     &         rx_marte(i), ry_marte(i),
     &         rx_jupiter(i), ry_jupiter(i))*dtau**2
     &       + mu_saturno*aceleracao('y',
     &         rx_marte(i), ry_marte(i),
     &         rx_saturno(i), ry_saturno(i))*dtau**2
     &       + mu_urano*aceleracao('y',
     &         rx_marte(i), ry_marte(i),
     &         rx_urano(i), ry_urano(i))*dtau**2
     &       + mu_netuno*aceleracao('y',
     &         rx_marte(i), ry_marte(i),
     &         rx_netuno(i), ry_netuno(i))*dtau**2

        ! Atualizacao das posicoes de Jupiter
        rx_jupiter(i+1) = 2.0d0*rx_jupiter(i)
     &       - rx_jupiter(i-1)
     &       + aceleracao('x', rx_jupiter(i),
     &         ry_jupiter(i), rx_sol, ry_sol)*dtau**2
     &       + mu_mercurio*aceleracao('x',
     &         rx_jupiter(i), ry_jupiter(i),
     &         rx_mercurio(i), ry_mercurio(i))*dtau**2
     &       + mu_venus*aceleracao('x',
     &         rx_jupiter(i), ry_jupiter(i),
     &         rx_venus(i), ry_venus(i))*dtau**2
     &       + mu_terra*aceleracao('x',
     &         rx_jupiter(i), ry_jupiter(i),
     &         rx_terra(i), ry_terra(i))*dtau**2
     &       + mu_marte*aceleracao('x',
     &         rx_jupiter(i), ry_jupiter(i),
     &         rx_marte(i), ry_marte(i))*dtau**2
     &       + mu_saturno*aceleracao('x',
     &         rx_jupiter(i), ry_jupiter(i),
     &         rx_saturno(i), ry_saturno(i))*dtau**2
     &       + mu_urano*aceleracao('x',
     &         rx_jupiter(i), ry_jupiter(i),
     &         rx_urano(i), ry_urano(i))*dtau**2
     &       + mu_netuno*aceleracao('x',
     &         rx_jupiter(i), ry_jupiter(i),
     &         rx_netuno(i), ry_netuno(i))*dtau**2

        ry_jupiter(i+1) = 2.0d0*ry_jupiter(i)
     &       - ry_jupiter(i-1)
     &       + aceleracao('y', rx_jupiter(i),
     &         ry_jupiter(i), rx_sol, ry_sol)*dtau**2
     &       + mu_mercurio*aceleracao('y',
     &         rx_jupiter(i), ry_jupiter(i),
     &         rx_mercurio(i), ry_mercurio(i))*dtau**2
     &       + mu_venus*aceleracao('y',
     &         rx_jupiter(i), ry_jupiter(i),
     &         rx_venus(i), ry_venus(i))*dtau**2
     &       + mu_terra*aceleracao('y',
     &         rx_jupiter(i), ry_jupiter(i),
     &         rx_terra(i), ry_terra(i))*dtau**2
     &       + mu_marte*aceleracao('y',
     &         rx_jupiter(i), ry_jupiter(i),
     &         rx_marte(i), ry_marte(i))*dtau**2
     &       + mu_saturno*aceleracao('y',
     &         rx_jupiter(i), ry_jupiter(i),
     &         rx_saturno(i), ry_saturno(i))*dtau**2
     &       + mu_urano*aceleracao('y',
     &         rx_jupiter(i), ry_jupiter(i),
     &         rx_urano(i), ry_urano(i))*dtau**2
     &       + mu_netuno*aceleracao('y',
     &         rx_jupiter(i), ry_jupiter(i),
     &         rx_netuno(i), ry_netuno(i))*dtau**2

        ! Atualizacao das posicoes de Saturno
        rx_saturno(i+1) = 2.0d0*rx_saturno(i)
     &       - rx_saturno(i-1)
     &       + aceleracao('x', rx_saturno(i),
     &         ry_saturno(i), rx_sol, ry_sol)*dtau**2
     &       + mu_mercurio*aceleracao('x',
     &         rx_saturno(i), ry_saturno(i),
     &         rx_mercurio(i), ry_mercurio(i))*dtau**2
     &       + mu_venus*aceleracao('x',
     &         rx_saturno(i), ry_saturno(i),
     &         rx_venus(i), ry_venus(i))*dtau**2
     &       + mu_terra*aceleracao('x',
     &         rx_saturno(i), ry_saturno(i),
     &         rx_terra(i), ry_terra(i))*dtau**2
     &       + mu_marte*aceleracao('x',
     &         rx_saturno(i), ry_saturno(i),
     &         rx_marte(i), ry_marte(i))*dtau**2
     &       + mu_jupiter*aceleracao('x',
     &         rx_saturno(i), ry_saturno(i),
     &         rx_jupiter(i), ry_jupiter(i))*dtau**2
     &       + mu_urano*aceleracao('x',
     &         rx_saturno(i), ry_saturno(i),
     &         rx_urano(i), ry_urano(i))*dtau**2
     &       + mu_netuno*aceleracao('x',
     &         rx_saturno(i), ry_saturno(i),
     &         rx_netuno(i), ry_netuno(i))*dtau**2

        ry_saturno(i+1) = 2.0d0*ry_saturno(i)
     &       - ry_saturno(i-1)
     &       + aceleracao('y', rx_saturno(i),
     &         ry_saturno(i), rx_sol, ry_sol)*dtau**2
     &       + mu_mercurio*aceleracao('y',
     &         rx_saturno(i), ry_saturno(i),
     &         rx_mercurio(i), ry_mercurio(i))*dtau**2
     &       + mu_venus*aceleracao('y',
     &         rx_saturno(i), ry_saturno(i),
     &         rx_venus(i), ry_venus(i))*dtau**2
     &       + mu_terra*aceleracao('y',
     &         rx_saturno(i), ry_saturno(i),
     &         rx_terra(i), ry_terra(i))*dtau**2
     &       + mu_marte*aceleracao('y',
     &         rx_saturno(i), ry_saturno(i),
     &         rx_marte(i), ry_marte(i))*dtau**2
     &       + mu_jupiter*aceleracao('y',
     &         rx_saturno(i), ry_saturno(i),
     &         rx_jupiter(i), ry_jupiter(i))*dtau**2
     &       + mu_urano*aceleracao('y',
     &         rx_saturno(i), ry_saturno(i),
     &         rx_urano(i), ry_urano(i))*dtau**2
     &       + mu_netuno*aceleracao('y',
     &         rx_saturno(i), ry_saturno(i),
     &         rx_netuno(i), ry_netuno(i))*dtau**2

        ! Atualizacao das posicoes de Urano
        rx_urano(i+1) = 2.0d0*rx_urano(i)
     &       - rx_urano(i-1)
     &       + aceleracao('x', rx_urano(i),
     &         ry_urano(i), rx_sol, ry_sol)*dtau**2
     &       + mu_mercurio*aceleracao('x',
     &         rx_urano(i), ry_urano(i),
     &         rx_mercurio(i), ry_mercurio(i))*dtau**2
     &       + mu_venus*aceleracao('x',
     &         rx_urano(i), ry_urano(i),
     &         rx_venus(i), ry_venus(i))*dtau**2
     &       + mu_terra*aceleracao('x',
     &         rx_urano(i), ry_urano(i),
     &         rx_terra(i), ry_terra(i))*dtau**2
     &       + mu_marte*aceleracao('x',
     &         rx_urano(i), ry_urano(i),
     &         rx_marte(i), ry_marte(i))*dtau**2
     &       + mu_jupiter*aceleracao('x',
     &         rx_urano(i), ry_urano(i),
     &         rx_jupiter(i), ry_jupiter(i))*dtau**2
     &       + mu_saturno*aceleracao('x',
     &         rx_urano(i), ry_urano(i),
     &         rx_saturno(i), ry_saturno(i))*dtau**2
     &       + mu_netuno*aceleracao('x',
     &         rx_urano(i), ry_urano(i),
     &         rx_netuno(i), ry_netuno(i))*dtau**2

        ry_urano(i+1) = 2.0d0*ry_urano(i)
     &       - ry_urano(i-1)
     &       + aceleracao('y', rx_urano(i),
     &         ry_urano(i), rx_sol, ry_sol)*dtau**2
     &       + mu_mercurio*aceleracao('y',
     &         rx_urano(i), ry_urano(i),
     &         rx_mercurio(i), ry_mercurio(i))*dtau**2
     &       + mu_venus*aceleracao('y',
     &         rx_urano(i), ry_urano(i),
     &         rx_venus(i), ry_venus(i))*dtau**2
     &       + mu_terra*aceleracao('y',
     &         rx_urano(i), ry_urano(i),
     &         rx_terra(i), ry_terra(i))*dtau**2
     &       + mu_marte*aceleracao('y',
     &         rx_urano(i), ry_urano(i),
     &         rx_marte(i), ry_marte(i))*dtau**2
     &       + mu_jupiter*aceleracao('y',
     &         rx_urano(i), ry_urano(i),
     &         rx_jupiter(i), ry_jupiter(i))*dtau**2
     &       + mu_saturno*aceleracao('y',
     &         rx_urano(i), ry_urano(i),
     &         rx_saturno(i), ry_saturno(i))*dtau**2
     &       + mu_netuno*aceleracao('y',
     &         rx_urano(i), ry_urano(i),
     &         rx_netuno(i), ry_netuno(i))*dtau**2

        ! Atualizacao das posicoes de Netuno
        rx_netuno(i+1) = 2.0d0*rx_netuno(i)
     &       - rx_netuno(i-1)
     &       + aceleracao('x', rx_netuno(i),
     &         ry_netuno(i), rx_sol, ry_sol)*dtau**2
     &       + mu_mercurio*aceleracao('x',
     &         rx_netuno(i), ry_netuno(i),
     &         rx_mercurio(i), ry_mercurio(i))*dtau**2
     &       + mu_venus*aceleracao('x',
     &         rx_netuno(i), ry_netuno(i),
     &         rx_venus(i), ry_venus(i))*dtau**2
     &       + mu_terra*aceleracao('x',
     &         rx_netuno(i), ry_netuno(i),
     &         rx_terra(i), ry_terra(i))*dtau**2
     &       + mu_marte*aceleracao('x',
     &         rx_netuno(i), ry_netuno(i),
     &         rx_marte(i), ry_marte(i))*dtau**2
     &       + mu_jupiter*aceleracao('x',
     &         rx_netuno(i), ry_netuno(i),
     &         rx_jupiter(i), ry_jupiter(i))*dtau**2
     &       + mu_saturno*aceleracao('x',
     &         rx_netuno(i), ry_netuno(i),
     &         rx_saturno(i), ry_saturno(i))*dtau**2
     &       + mu_urano*aceleracao('x',
     &         rx_netuno(i), ry_netuno(i),
     &         rx_urano(i), ry_urano(i))*dtau**2

        ry_netuno(i+1) = 2.0d0*ry_netuno(i)
     &       - ry_netuno(i-1)
     &       + aceleracao('y', rx_netuno(i),
     &         ry_netuno(i), rx_sol, ry_sol)*dtau**2
     &       + mu_mercurio*aceleracao('y',
     &         rx_netuno(i), ry_netuno(i),
     &         rx_mercurio(i), ry_mercurio(i))*dtau**2
     &       + mu_venus*aceleracao('y',
     &         rx_netuno(i), ry_netuno(i),
     &         rx_venus(i), ry_venus(i))*dtau**2
     &       + mu_terra*aceleracao('y',
     &         rx_netuno(i), ry_netuno(i),
     &         rx_terra(i), ry_terra(i))*dtau**2
     &       + mu_marte*aceleracao('y',
     &         rx_netuno(i), ry_netuno(i),
     &         rx_marte(i), ry_marte(i))*dtau**2
     &       + mu_jupiter*aceleracao('y',
     &         rx_netuno(i), ry_netuno(i),
     &         rx_jupiter(i), ry_jupiter(i))*dtau**2
     &       + mu_saturno*aceleracao('y',
     &         rx_netuno(i), ry_netuno(i),
     &         rx_saturno(i), ry_saturno(i))*dtau**2
     &       + mu_urano*aceleracao('y',
     &         rx_netuno(i), ry_netuno(i),
     &         rx_urano(i), ry_urano(i))*dtau**2

        ! Escreve no arquivo de saida as posicoes dos planetas
        tau = (i-1)*dtau
        write(10,*) tau,
     &       rx_mercurio(i), ry_mercurio(i),
     &       rx_venus(i), ry_venus(i),
     &       rx_terra(i), ry_terra(i),
     &       rx_marte(i), ry_marte(i),
     &       rx_jupiter(i), ry_jupiter(i),
     &       rx_saturno(i), ry_saturno(i),
     &       rx_urano(i), ry_urano(i),
     &       rx_netuno(i), ry_netuno(i)

      end do

      close(10)
      end subroutine


      function aceleracao(
     &   componente, rx_corpo_1, ry_corpo_1,
     &   rx_corpo_2, ry_corpo_2)
      ! Calcula a aceleracao de um corpo devido a gravidade de outro
      ! componente: 'x' para x, 'y' para y
      implicit none
      character*1 componente
      real*8 rx_corpo_1, ry_corpo_1, rx_corpo_2, ry_corpo_2
      real*8 aceleracao, r

      r = sqrt((rx_corpo_2-rx_corpo_1)**2
     &       + (ry_corpo_2-ry_corpo_1)**2)

      if (componente .eq. 'x') then
        aceleracao = -(rx_corpo_1-rx_corpo_2)/r**3
      else if (componente .eq. 'y') then
        aceleracao = -(ry_corpo_1-ry_corpo_2)/r**3
      end if

      return
      end function
