      program tarefa2b
      ! Gera um arquivo de saida contendo, em cada linha,
      ! o instante de tempo t, os valores de N(t) das dez
      ! primeiras simulacoes, o valor medio N_medio(t) obtido
      ! das k simulacoes e a barra de erro correspondente.
      ! Formato: t, N1, N2, ..., N10, N_medio, erro

      implicit none

      character*50 arquivo_saida

      integer*4 i, j
      integer*4 k
      integer*4 num_dts
      integer*4 Nt(10**3)
      integer*4 N10(10,10**3)
      integer*4 soma_N(10**3)

      integer*4 N0
      real*8 soma_N2(10**3)
      real*8 N_medio
      real*8 N_quadratico_medio
      real*8 desvio_padrao
      real*8 erro
      real*8 tau, tmax, dt, t

      integer*4 iseed
      parameter(iseed = 42)

      call srand(iseed)

      arquivo_saida = 'tarefa-2b-saida-1.dat'
      open(unit=10, file=arquivo_saida)

      N0 = (10d0)**3
      k = (10d0)**3

      tau = 10d0
      tmax = 30d0
      dt = 0.1d0

      num_dts = int(tmax / dt)

      do j = 1, num_dts + 1
            soma_N(j) = 0
            soma_N2(j) = 0d0
      end do

      ! Preenche as 10 primeiras colunas com as 10 primeiras simulacoes
      do i = 1, k

            Nt(1) = N0

            call simulacao_decaimento(Nt, tmax, dt, tau)

            do j = 1, num_dts + 1

                  soma_N(j) = soma_N(j) + Nt(j)

                  soma_N2(j) = soma_N2(j) +
     &                  dble(Nt(j))**2

                  if (i .le. 10) then
                        N10(i,j) = Nt(j)
                  endif

            end do

      end do

      ! Preenche as ultimas colunas com N_medio e o erro
      do j = 1, num_dts + 1

            t = (j - 1) * dt

            N_medio = dble(soma_N(j)) / dble(k)

            N_quadratico_medio = soma_N2(j) / dble(k)

            desvio_padrao = dsqrt(N_quadratico_medio -
     &            N_medio**2)

            erro = desvio_padrao / dsqrt(dble(k))

            write(10,*) t,
     &            (N10(i,j), i = 1,10),
     &            N_medio, erro

      end do

      close(10)

      end

      function get_decaimentos(N_atual, tau, dt)
      ! Sorteia decaimentos radioativos de nucleos a partir
      ! da probabilidade de decaimento dp = (1/tau) dt.
      ! Entradas:
      ! N_atual - numero de nucleos
      ! tau     - vida media
      ! dt      - intervalo de tempo
      ! Saida:
      ! get_decaimentos - numero de decaimentos no intervalo dt

      integer*4 i, N_atual
      integer*4 get_decaimentos
      real*8 tau, dt, dp, num_aleatorio

      dp = dt / tau
      get_decaimentos = 0

      do i = 1, N_atual
            num_aleatorio = rand()
            if (num_aleatorio .lt. dp) then
                  get_decaimentos = get_decaimentos + 1
            endif
      end do

      return
      end

      subroutine simulacao_decaimento(Nt, tmax, dt, tau)
      ! Simula o decaimento radioativo de N0 nucleos ao longo
      ! do intervalo de tempo [0, tmax], dividindo-o em
      ! intervalos de duracao dt. O numero de decaimentos e
      ! dado pela funcao get_decaimentos.
      ! Entradas:
      ! Nt   - vetor que armazena o numero de nucleos em cada
      !        instante de tempo. O primeiro elemento Nt(1)
      !        corresponde ao instante t = 0.
      ! tmax - tempo maximo da simulacao.
      ! dt   - intervalo de tempo.
      ! tau  - vida media.
      ! Saida:
      ! Nt   - vetor contendo o numero de nucleos restantes em
      !        cada instante de tempo da simulacao. O elemento
      !        Nt(indice_tempo + 1) e obtido a partir de
      !        Nt(indice_tempo) apos os decaimentos ocorridos
      !        durante o intervalo dt.

      integer*4 indice_tempo, num_dts
      real*8 tau, tmax, dt
      integer*4 N_atual
      integer*4 Nt(10**3)
      integer*4 decaimentos, get_decaimentos

      num_dts = int(tmax / dt)

      ! Loop para cada intervalo dt no intervalo [0, tmax]
      do indice_tempo = 1, num_dts
            N_atual = Nt(indice_tempo)
            decaimentos = get_decaimentos(N_atual, tau, dt)
            Nt(indice_tempo + 1) =
     &            N_atual - decaimentos
      end do

      end