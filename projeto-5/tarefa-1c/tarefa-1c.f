      program tarefa1c
      ! Órbitas circulares: escolhendo dtau para que a órbita seja estável nos métodos de Euler-Cromer e Verlet
      ! e verificando a 3a Lei de Kepler.
      ! Utiliza-se a 3a Lei de Kepler GM⊙ = 4pi^2 UA^3 ano^(−2).
      ! Grandezas adimensionais rho = r/UA, tau = 2pi*t/ano, v = v/(2pi*UA/ano).
      ! Toma-se o Sol como origem estática e a Terra como corpo em órbita.

      implicit none

      integer*4 i

      real*8 a, e, periodo, taumax
      real*8 dtau
      real*8 dtau_ec, dtau_v
      real*8 amin, amax, da
      integer*4 num_a

      real*8 get_delta_euler_cromer, get_delta_verlet
      real*8 get_dtau_max_euler_cromer, get_dtau_max_verlet

      real*8 pi
      parameter (pi = 3.14159265358979323d0)

      ! Condicoes iniciais adimensionais
      e = 0.0d0 ! excentricidade da órbita elíptica

      ! Parametros da variacao de a
      num_a = 100 ! numero de valores de a
      amin = 0.1d0 ! a mínimo
      amax = 2.0d0 ! a máximo
      da = (amax-amin)/dble(num_a-1) ! Incremento de a

      ! Arquivos de saida
      open(10, file='tarefa-1c-saida-1.dat')
      open(20, file='tarefa-1c-saida-2.dat')
      open(30, file='tarefa-1c-saida-3.dat')
      open(40, file='tarefa-1c-saida-4.dat')

      ! ! delta em função de dtau ! !
      a = 1.0d0 ! semieixo maior da órbita elíptica
      periodo = 2.0d0*pi*a**(3.0d0/2.0d0)
      taumax = 10.0d0*periodo
      do i = 1, 10

        dtau = 10.0d0**(-dble(i))

        ! Escrita dos resultados
        write(10,*) dtau, get_delta_euler_cromer(
     &     taumax, dtau, a, e)
        write(20,*) dtau, get_delta_verlet(
     &     taumax, dtau, a, e)

      end do

      close(10)
      close(20)

      ! ! dtau_max em função de a ! !
      do i = 1, num_a

        a = amin + dble(i-1)*da

        ! Parâmetros da simulação
        periodo = 2.0d0*pi*a**(3.0d0/2.0d0)
        taumax = 10.0d0*periodo

        ! Metodo de Euler-Cromer
        dtau_ec = get_dtau_max_euler_cromer(
     &       taumax, a, e)

        ! Metodo de Verlet
        dtau_v = get_dtau_max_verlet(
     &       taumax, a, e)

        ! Escrita dos resultados
        write(30,*) a, dtau_ec
        write(40,*) a, dtau_v

      end do

      close(30)
      close(40)

      end program 

      function get_delta_euler_cromer(taumax, dtau, a, e)
      ! Simulação de órbita gravitacional via método de Euler-Cromer
      ! Retorna delta = (rho_max / rho_min) - 1, para verificar a estabilidade da órbita.

        implicit none
        integer*4 i, nsteps
        real*8 taumax, dtau, a, e
        real*8 x, y, vx, vy, r, rmin, rmax
        real*8 get_delta_euler_cromer

        ! Reinicializacao das condicoes iniciais
        x = a
        y = 0.0d0
        vx = -e/sqrt(a)
        vy = sqrt((1.0d0-e**2)/a)

        nsteps = nint(taumax/dtau)

        ! Inicializacao dos extremos
        r = sqrt(x**2 + y**2)
        rmin = r
        rmax = r

        ! Integracao por Euler-Cromer
        do i = 1, nsteps

            r = sqrt(x**2 + y**2)

            ! Atualizacao das velocidades
            vx = vx - (x/r**3)*dtau
            vy = vy - (y/r**3)*dtau

            ! Atualizacao das posicoes
            x = x + vx*dtau
            y = y + vy*dtau

            ! Atualizacao dos extremos do raio
            r = sqrt(x**2 + y**2)

            rmin = min(rmin, r)
            rmax = max(rmax, r)

        end do

        get_delta_euler_cromer = (rmax/rmin) - 1.0d0

      end function


      function get_delta_verlet(taumax, dtau, a, e)
      ! Simulação de órbita gravitacional via método de Verlet
      ! Retorna delta = (rho_max / rho_min) - 1, para verificar a estabilidade da órbita.

        implicit none
        integer*4 i, nsteps
        real*8 taumax, dtau, a, e
        real*8 xprev, yprev, xcurr, ycurr
        real*8 xnext, ynext, vx0, vy0
        real*8 r, rmin, rmax
        real*8 get_delta_verlet

        ! Condicoes iniciais
        xprev = a
        yprev = 0.0d0
        vx0 = -e/sqrt(a)
        vy0 = sqrt((1.0d0-e**2)/a)

        nsteps = nint(taumax/dtau)

        ! Inicializacao dos extremos
        r = sqrt(xprev**2 + yprev**2)
        rmin = r
        rmax = r

        ! Segunda posicao calculada pelo metodo de Euler
        xcurr = xprev + vx0*dtau
        ycurr = yprev + vy0*dtau

        r = sqrt(xcurr**2 + ycurr**2)
        rmin = min(rmin, r)
        rmax = max(rmax, r)

        ! Integracao por Verlet
        do i = 2, nsteps

            r = sqrt(xcurr**2 + ycurr**2)

            ! Atualizacao das posicoes
            xnext = 2.0d0*xcurr - xprev - (xcurr/r**3)*dtau**2

            ynext = 2.0d0*ycurr - yprev - (ycurr/r**3)*dtau**2

            ! Atualizacao dos extremos do raio
            r = sqrt(xnext**2 + ynext**2)

            rmin = min(rmin, r)
            rmax = max(rmax, r)

            ! Avanco dos passos temporais
            xprev = xcurr
            yprev = ycurr

            xcurr = xnext
            ycurr = ynext

        end do

        get_delta_verlet = (rmax/rmin) - 1.0d0

      end function


      function get_dtau_max_euler_cromer(
     &     taumax, a, e)

      implicit none

      real*8 taumax, a, e
      real*8 dtau, delta
      real*8 dtau_inf, dtau_sup
      real*8 get_delta_euler_cromer
      real*8 get_dtau_max_euler_cromer

      integer i

      get_dtau_max_euler_cromer = 0.0d0

      ! Intervalo inicial
      dtau_sup = 1.0d-1

      ! Testa o maior passo inicial
      delta = get_delta_euler_cromer(
     &       taumax, dtau_sup, a, e)

      if (delta .lt. 1.0d-3) then
        get_dtau_max_euler_cromer = dtau_sup
        return
      end if

      ! Busca progressiva do intervalo
      do i = 1, 20

        dtau_inf = dtau_sup/2.0d0

        if (dtau_inf .lt. 1.0d-6) then
          dtau_inf = 1.0d-6
        end if

        delta = get_delta_euler_cromer(
     &         taumax, dtau_inf, a, e)

        if (delta .lt. 1.0d-3) exit

        dtau_sup = dtau_inf

        if (dtau_sup .le. 1.0d-6) return

      end do

      ! Refinamento por busca binaria
      do i = 1, 20

        dtau = (dtau_inf + dtau_sup)/2.0d0

        delta = get_delta_euler_cromer(
     &         taumax, dtau, a, e)

        if (delta .lt. 1.0d-3) then
          dtau_inf = dtau
        else
          dtau_sup = dtau
        end if

      end do

      get_dtau_max_euler_cromer = dtau_inf

      return
      end function


      function get_dtau_max_verlet(
     &     taumax, a, e)

      implicit none

      real*8 taumax, a, e
      real*8 dtau, delta
      real*8 dtau_inf, dtau_sup
      real*8 get_delta_verlet
      real*8 get_dtau_max_verlet

      integer i

      get_dtau_max_verlet = 0.0d0

      ! Intervalo inicial
      dtau_sup = 1.0d-1

      ! Testa o maior passo inicial
      delta = get_delta_verlet(
     &       taumax, dtau_sup, a, e)

      if (delta .lt. 1.0d-3) then
        get_dtau_max_verlet = dtau_sup
        return
      end if

      ! Busca progressiva do intervalo
      do i = 1, 20

        dtau_inf = dtau_sup/2.0d0

        if (dtau_inf .lt. 1.0d-6) then
          dtau_inf = 1.0d-6
        end if

        delta = get_delta_verlet(
     &         taumax, dtau_inf, a, e)

        if (delta .lt. 1.0d-3) exit

        dtau_sup = dtau_inf

        if (dtau_sup .le. 1.0d-6) return

      end do

      ! Refinamento por busca binaria
      do i = 1, 20

        dtau = (dtau_inf + dtau_sup)/2.0d0

        delta = get_delta_verlet(
     &         taumax, dtau, a, e)

        if (delta .lt. 1.0d-3) then
          dtau_inf = dtau
        else
          dtau_sup = dtau
        end if

      end do

      get_dtau_max_verlet = dtau_inf

      return
      end function