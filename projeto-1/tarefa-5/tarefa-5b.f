      program Tarefa5b
      ! Le uma matriz real N x N de um arquivo de entrada.
      ! Utiliza as permutacoes geradas pelo programa anterior.
      ! Para cada permutacao, calcula o produto dos elementos
      ! correspondentes da matriz.
      ! Multiplica o produto pela paridade da permutacao.
      ! Soma os termos para calcular o determinante da matriz.
      ! Ao final, escreve o determinante no arquivo de saida.

      character*30 nome_arquivo_entrada
      real*8 det, determinante

      nome_arquivo_entrada = 'tarefa-5b-entrada-1.in'

      det = determinante(nome_arquivo_entrada)

      print *, 'O determinante eh', det

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

      !print *, 'N = ', N

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

              !print *, (permutacao_n1(j),j=1,N+1), paridade_n1

              write(20,*) (permutacao_n1(j),j=1,N+1),
     &                    paridade_n1

          end do

      end do

      close(10)
      close(20)

      return
      end

      function determinante(nome_arquivo_entrada)
      ! Calcula o determinante de uma matriz real N x N lida de um
      ! arquivo de entrada utilizando as permutacoes geradas pelo
      ! programa anterior.
      !
      ! Input:
      !     nome_arquivo_entrada - nome do arquivo contendo a matriz
      !
      ! Output:
      !     determinante - determinante da matriz
      !
      ! Primeiro, abre o arquivo contendo a matriz e le sua primeira
      ! linha como uma cadeia de caracteres para determinar N.
      ! A funcao cardinalidade_numeros conta quantos elementos existem
      ! nessa linha, determinando assim a dimensao da matriz.
      !
      ! Em seguida, volta ao inicio do arquivo e le os N x N elementos
      ! da matriz, armazenando-os no vetor bidimensional matriz.
      !
      ! Para gerar as permutacoes necessarias ao calculo do
      ! determinante, cria inicialmente um arquivo contendo a unica
      ! permutacao de 1 elemento e sua paridade:
      !
      !     1  1
      !
      ! A subroutine permutacoes_n1 e entao utilizada sucessivamente.
      ! A cada chamada, ela recebe todas as permutacoes de N elementos
      ! presentes em um arquivo e gera todas as permutacoes de N + 1
      ! elementos em outro arquivo.
      !
      ! Os dois arquivos temporarios sao alternados entre entrada e
      ! saida a cada etapa. Dessa forma, nao e necessario criar um
      ! arquivo temporario diferente para cada valor de N.
      !
      ! Quando sao obtidas todas as permutacoes de N elementos, elas
      ! sao lidas do arquivo de saida. Para cada permutacao, calcula-se
      ! o produto dos elementos da matriz selecionados pela permutacao:
      !
      !     A(1,p(1)) * A(2,p(2)) * ... * A(N,p(N))
      !
      ! Esse produto e multiplicado pela paridade da permutacao. A
      ! soma de todos esses termos fornece o determinante pela
      ! formula de Leibniz:
      !
      !     det(A) = soma_p [paridade(p) * produto_i A(i,p(i))]

      real*8 determinante
      real*8 matriz(100,100), produto
      save matriz ! Resolve problema de limite de memória
      integer*4 N, i, j, k, iostat
      integer*4 paridade
      integer*4 permutacao(100)
      integer*4 cardinalidade_numeros
      character*30 nome_arquivo_entrada
      character*30 nome_arquivo_permutacoes
      character*30 nome_arquivo_entrada_temp
      character*30 nome_arquivo_saida_temp
      character*200 linha

      nome_arquivo_permutacoes = 'tarefa-5b-saida-1.dat'
      nome_arquivo_entrada_temp = 'tarefa-5b-entrada-2.in'
      nome_arquivo_saida_temp = 'tarefa-5b-saida-2.dat'

      open(10,file=nome_arquivo_entrada,status='old')

      ! Le a primeira linha para determinar N
      read(10,'(A)',iostat=iostat) linha

      if (iostat .ne. 0) stop

      N = cardinalidade_numeros(linha)

      ! Volta para o inicio do arquivo
      rewind(10)

      ! Le a matriz
      do i=1,N

          read(10,*,iostat=iostat) (matriz(i,j),j=1,N)

          if (iostat .ne. 0) stop

      end do

      close(10)

      ! Cria a permutacao inicial de 1 elemento
      open(30,file=nome_arquivo_entrada_temp,status='unknown')

      write(30,*) 1, 1

      close(30)

      ! Gera sucessivamente todas as permutacoes ate chegar em N elementos
      do k=1,N-1

          ! A ultima geracao deve ser salva no arquivo final
          if (k .eq. N-1) then

              call permutacoes_n1(nome_arquivo_entrada_temp,
     &                            nome_arquivo_permutacoes)

          else

              call permutacoes_n1(nome_arquivo_entrada_temp,
     &                            nome_arquivo_saida_temp)

              ! Alterna os arquivos temporarios
              if (nome_arquivo_entrada_temp .eq.
     &            'tarefa-5b-entrada-2.in') then

                  nome_arquivo_entrada_temp =
     &                'tarefa-5b-saida-2.dat'
                  nome_arquivo_saida_temp =
     &                'tarefa-5b-entrada-2.in'

              else

                  nome_arquivo_entrada_temp =
     &                'tarefa-5b-entrada-2.in'
                  nome_arquivo_saida_temp =
     &                'tarefa-5b-saida-2.dat'

              endif

          endif

      end do

      ! Para N = 1, a permutacao inicial ja e a permutacao final
      if (N .eq. 1) then

          open(30,file=nome_arquivo_permutacoes,status='unknown')

          write(30,*) 1, 1

          close(30)

      endif

      ! Abre o arquivo contendo todas as permutacoes de N elementos
      open(20,file=nome_arquivo_permutacoes,status='old')

      determinante = 0.0d0

      ! Le cada permutacao e sua paridade
      do i=1,10000

          read(20,*,iostat=iostat)
     &        (permutacao(j),j=1,N), paridade

          if (iostat .ne. 0) exit

          ! Calcula o produto correspondente a permutacao
          produto = 1.0d0

          do j=1,N
              produto = produto *
     &                  matriz(j,permutacao(j))
          end do

          ! Soma o produto com o sinal da paridade
          determinante = determinante +
     &                   dble(paridade)*produto

      end do

      close(20)

      return
      end