      program Tarefa5a
      ! Lê do arquivo de entrada tarefa-5-entrada.in as permutações de N
      ! inteiros (1, 2, ..., N) e as correspondentes paridades (-1 ou +1)
      ! Com isso, produz as permutações de N + 1 números com a devida paridade.
      ! O resultado é salvo num arquivo.

      character*30 nome_arquivo_entrada, nome_arquivo_saida

      nome_arquivo_entrada = 'tarefa-5a-entrada-1.in'
      nome_arquivo_saida = 'tarefa-5a-saida-1.dat'

      call permutacoes_n1(nome_arquivo_entrada, nome_arquivo_saida)

      stop
      end

      function cardinalidade_numeros(linha)
      ! Conta quantos numeros existem em uma linha
      ! Input: linha - cadeia de caracteres contendo os numeros
      ! Output: cardinalidade_numeros - quantidade de numeros

      integer*4 cardinalidade_numeros
      integer*4 k, dentro_numero
      character*200 linha

      cardinalidade_numeros = 0
      dentro_numero = 0

      do k=1,200

          if (linha(k:k) .ne. ' ') then

              if (dentro_numero .eq. 0) then
                  cardinalidade_numeros = cardinalidade_numeros + 1
                  dentro_numero = 1
              endif

          else

              dentro_numero = 0

          endif

      end do

      return
      end

      subroutine calcula_permutacao_n1(N, permutacao, paridade,
     &                                 permutacao_n1, paridade_n1, i)
      ! Calcula a permutação de N + 1 números e a paridade correspondente
      ! Inputs: N - número de elementos na permutação original
      !         permutacao - permutação original de N elementos
      !         paridade - paridade da permutação original
      !         i - posição onde N + 1 será inserido
      ! Outputs: permutacao_n1 - permutação de N + 1 elementos
      !          paridade_n1 - paridade da permutação de N + 1 elementos

      integer*4 N, permutacao(100), paridade
      integer*4 permutacao_n1(100), paridade_n1
      integer*4 i, j

      ! Copia os elementos antes da posição i
      do j=1, i-1
          permutacao_n1(j) = permutacao(j)
      end do

      ! Insere o número N + 1 na posição i
      permutacao_n1(i) = N + 1

      ! Copia os elementos depois da posição i
      do j=i, N
          permutacao_n1(j+1) = permutacao(j)
      end do

      ! Calcula a paridade da nova permutação
      paridade_n1 = paridade * (-1)**(N - i + 1)

      return
      end

      subroutine permutacoes_n1(nome_arquivo_entrada,
     &                        nome_arquivo_saida)
      ! Le um arquivo contendo permutacoes de N elementos e suas
      ! respectivas paridades e produz, para cada permutacao,
      ! as N + 1 permutacoes obtidas pela insercao de N + 1.
      !
      ! Input:
      !     nome_arquivo_entrada - nome do arquivo de entrada
      !     nome_arquivo_saida   - nome do arquivo de saida
      !
      ! Output:
      !     arquivo de saída com nome nome_arquivo_saida
      integer*4 N, i, j, k, iostat
      integer*4 cardinalidade_numeros
      integer*4 permutacao(100), paridade
      integer*4 permutacao_n1(100), paridade_n1
      character*30 nome_arquivo_entrada, nome_arquivo_saida
      character*200 linha

      open(10, file=nome_arquivo_entrada, status='old')
      open(20, file=nome_arquivo_saida, status='unknown')

      ! Le a primeira linha como uma cadeia de caracteres
      read(10,'(A)',iostat=iostat) linha

      if (iostat .ne. 0) stop

      ! Como o ultimo numero da linha e a paridade, subtraimos 1
      N = cardinalidade_numeros(linha) - 1

      print *, 'N = ', N

      ! Volta para o inicio do arquivo
      rewind(10)

      do i=1,100

          read(10,*,iostat=iostat)
     &        (permutacao(j),j=1,N), paridade

          if (iostat .ne. 0) exit

          ! Calcula as N + 1 permutacoes de N + 1 numeros
          ! e suas respectivas paridades
          do k=1,N+1

              call calcula_permutacao_n1(N, permutacao, paridade,
     &                                   permutacao_n1, paridade_n1, k)

              print *, (permutacao_n1(j),j=1,N+1), paridade_n1

              write(20,*) (permutacao_n1(j),j=1,N+1),
     &                    paridade_n1

          end do

      end do

      close(10)
      close(20)

      return
      end