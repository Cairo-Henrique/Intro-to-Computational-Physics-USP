      program tarefa2a

      implicit none

      character(50) arquivo_saida

      integer*4 i, j
      integer*4 k
      integer*4 num_dts
      integer*4 Nt(10**3)

      integer*4 N0
      real*8 tau, tmax, dt

      integer*4 iseed
      parameter(iseed = 42)
      call srand(iseed)

      arquivo_saida = 'tarefa-2a-saida-1.dat'
      open(unit=10, file=arquivo_saida)

      ! Define os parâmetros das simulações
      N0 = (10d0)**3
      k = (10d0)**3
      tau = 1d0
      tmax = 10d0 ! tmax >> tau
      dt = 0.01d0 ! dt << tmax

      ! Calcula o número de intervalos de tempo da simulação
      num_dts = int(tmax / dt)

      ! Realiza k simulações independentes do decaimento radioativo
      do i = 1, k
            Nt(1) = N0
            call simulacao_decaimento(Nt, tmax, dt, tau)
            write(10, *) (Nt(j), j = 1, num_dts + 1) ! Linhas i são simulações, colunas j são instantes de tempo

      end do

      close(10)
    
      end

      function get_decaimentos(N_atual, tau, dt)
      ! Sorteia decaimentos radioativos de núcleos a partir
      ! da probabilidade de decaimento dp = (1/tau) dt.
      ! Entradas:
      ! N_atual - número de núcleos
      ! tau     - vida média
      ! dt      - intervalo de tempo
      ! Saída:
      ! get_decaimentos - número de decaimentos no intervalo dt

      integer*4 i, N_atual
      integer*4 get_decaimentos
      real*8 tau, dt, dp, num_aleatorio

      dp = dt / tau
      get_decaimentos = 0

      ! Para cada núcleo intacto, sorteia se ele vai decair
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
      ! intervalos de duracao dt. O numero de decaimentos é dado pela funcao get_decaimentos.
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
      !        Nt(indice_tempo + 1) é obtido a partir de
      !        Nt(indice_tempo) após os decaimentos ocorridos
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
            Nt(indice_tempo + 1) = N_atual - decaimentos ! Subtrai os núcleos que decairam no intervalo
      end do

      end