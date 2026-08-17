      program Tarefa5c
      ! Le uma matriz real A de ordem N e um vetor real y de ordem N
      ! de um arquivo de entrada.
      ! Utiliza o determinante calculado pelo programa anterior para
      ! resolver o sistema linear A x = y pela regra de Cramer.
      ! Ao final, imprime as solucoes dos tres sistemas.

      character*30 nome_arquivo_entrada
      real*8 x(100)
      integer*4 i, j

      do j=4,6

          if (j .eq. 4) then
              nome_arquivo_entrada = 'tarefa-5c-entrada-4.in'
          else if (j .eq. 5) then
              nome_arquivo_entrada = 'tarefa-5c-entrada-5.in'
          else
              nome_arquivo_entrada = 'tarefa-5c-entrada-6.in'
          endif

          call solucao(nome_arquivo_entrada,x)

          print *, ' '
          print *, 'Solucao para N = ', j

          ! Imprime as componentes do vetor solucao
          do i=1,j
              print *, 'x(',i,') = ', x(i)
          end do

      end do

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
                  cardinalidade_numeros =
     &                cardinalidade_numeros + 1
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
      ! Calcula a permutacao de N + 1 numeros e a paridade correspondente
      ! Inputs: N - numero de elementos na permutacao original
      !          permutacao - permutacao original de N elementos
      !          paridade - paridade da permutacao original
      !          i - posicao onde N + 1 sera inserido
      ! Outputs: permutacao_n1 - permutacao de N + 1 elementos
      !           paridade_n1 - paridade de N + 1 elementos

      integer*4 N, permutacao(100), paridade
      integer*4 permutacao_n1(100), paridade_n1
      integer*4 i, j

      ! Copia os elementos antes da posicao i
      do j=1,i-1
          permutacao_n1(j) = permutacao(j)
      end do

      ! Insere o numero N + 1 na posicao i
      permutacao_n1(i) = N + 1

      ! Copia os elementos depois da posicao i
      do j=i,N
          permutacao_n1(j+1) = permutacao(j)
      end do

      ! Calcula a paridade da nova permutacao
      paridade_n1 = paridade * (-1)**(N-i+1)

      return
      end


      subroutine permutacoes_n1(nome_arquivo_entrada,
     &                           nome_arquivo_saida)
      ! Le um arquivo contendo permutacoes de N elementos e suas
      ! respectivas paridades e produz, para cada permutacao,
      ! as N + 1 permutacoes obtidas pela insercao de N + 1.
      !
      ! Input:
      !     nome_arquivo_entrada - nome do arquivo de entrada
      !     nome_arquivo_saida - nome do arquivo de saida
      !
      ! Output:
      !     arquivo de saida com nome nome_arquivo_saida

      integer*4 N, i, j, k, iostat
      integer*4 cardinalidade_numeros
      integer*4 permutacao(100), paridade
      integer*4 permutacao_n1(100), paridade_n1
      character*30 nome_arquivo_entrada, nome_arquivo_saida
      character*200 linha

      open(10,file=nome_arquivo_entrada,status='old')
      open(20,file=nome_arquivo_saida,status='unknown')

      ! Le a primeira linha como uma cadeia de caracteres
      read(10,'(A)',iostat=iostat) linha

      if (iostat .ne. 0) stop

      ! Como o ultimo numero da linha e a paridade, subtraimos 1
      N = cardinalidade_numeros(linha)-1

      rewind(10)

      do i=1,10000

          read(10,*,iostat=iostat)
     &        (permutacao(j),j=1,N),paridade

          if (iostat .ne. 0) exit

          ! Calcula as N + 1 permutacoes
          do k=1,N+1

              call calcula_permutacao_n1(N,permutacao,paridade,
     &             permutacao_n1,paridade_n1,k)

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

      real*8 determinante
      real*8 matriz(100,100), produto
      save matriz

      integer*4 N, i, j, k, iostat
      integer*4 paridade
      integer*4 permutacao(100)
      integer*4 cardinalidade_numeros

      character*30 nome_arquivo_entrada
      character*30 nome_arquivo_permutacoes
      character*30 nome_arquivo_entrada_temp
      character*30 nome_arquivo_saida_temp
      character*200 linha

      nome_arquivo_permutacoes = 'tarefa-5c-saida-1.dat'
      nome_arquivo_entrada_temp = 'tarefa-5c-entrada-2.in'
      nome_arquivo_saida_temp = 'tarefa-5c-saida-2.dat'

      open(10,file=nome_arquivo_entrada,status='old')

      ! Le a primeira linha para determinar N
      read(10,'(A)',iostat=iostat) linha

      if (iostat .ne. 0) stop

      N = cardinalidade_numeros(linha)

      rewind(10)

      ! Le a matriz
      do i=1,N

          read(10,*,iostat=iostat) (matriz(i,j),j=1,N)

          if (iostat .ne. 0) stop

      end do

      close(10)

      ! Cria a permutacao inicial de 1 elemento
      open(30,file=nome_arquivo_entrada_temp,status='unknown')

      write(30,*) 1,1

      close(30)

      ! Gera sucessivamente todas as permutacoes ate chegar em N
      do k=1,N-1

          if (k .eq. N-1) then

              call permutacoes_n1(nome_arquivo_entrada_temp,
     &                            nome_arquivo_permutacoes)

          else

              call permutacoes_n1(nome_arquivo_entrada_temp,
     &                            nome_arquivo_saida_temp)

              if (nome_arquivo_entrada_temp .eq.
     &            'tarefa-5c-entrada-2.in') then

                  nome_arquivo_entrada_temp =
     &                'tarefa-5c-saida-2.dat'

                  nome_arquivo_saida_temp =
     &                'tarefa-5c-entrada-2.in'

              else

                  nome_arquivo_entrada_temp =
     &                'tarefa-5c-entrada-2.in'

                  nome_arquivo_saida_temp =
     &                'tarefa-5c-saida-2.dat'

              endif

          endif

      end do

      ! Para N = 1
      if (N .eq. 1) then

          open(30,file=nome_arquivo_permutacoes,status='unknown')

          write(30,*) 1,1

          close(30)

      endif

      open(20,file=nome_arquivo_permutacoes,status='old')

      determinante = 0.0d0

      do i=1,10000

          read(20,*,iostat=iostat)
     &        (permutacao(j),j=1,N),paridade

          if (iostat .ne. 0) exit

          produto = 1.0d0

          do j=1,N

              produto = produto *
     &                  matriz(j,permutacao(j))

          end do

          determinante = determinante +
     &                   dble(paridade)*produto

      end do

      close(20)

      return
      end


      subroutine solucao(nome_arquivo_entrada,x)
      ! Resolve o sistema linear A x = y pela regra de Cramer.
      !
      ! Input:
      !     nome_arquivo_entrada - arquivo contendo A e y
      !
      ! Output:
      !     x - vetor solucao do sistema linear
      !
      ! O arquivo deve conter N linhas com os elementos da matriz A
      ! e uma ultima linha contendo os N elementos do vetor y.
      !
      ! Para cada componente x(i), e criada uma matriz A_i,
      ! substituindo a coluna i de A pelo vetor y.
      ! A componente x(i) e calculada por:
      !
      !     x(i) = det(A_i) / det(A)

      real*8 x(100)
      real*8 matriz(100,100), matriz_aux(100,100)
      save matriz
      save matriz_aux
      real*8 y(100)
      real*8 det_a, det_ai, determinante
      integer*4 N, i, j, k, iostat
      integer*4 cardinalidade_numeros
      character*30 nome_arquivo_entrada
      character*30 nome_arquivo_temp
      character*200 linha

      nome_arquivo_temp = 'tarefa-5c-entrada-3.in'

      open(10,file=nome_arquivo_entrada,status='old')

      ! Le a primeira linha para determinar N
      read(10,'(A)',iostat=iostat) linha

      if (iostat .ne. 0) stop

      N = cardinalidade_numeros(linha)

      rewind(10)

      ! Le a matriz A
      do i=1,N

          read(10,*,iostat=iostat) (matriz(i,j),j=1,N)

          if (iostat .ne. 0) stop

      end do

      ! Le o vetor y
      read(10,*,iostat=iostat) (y(i),i=1,N)

      if (iostat .ne. 0) stop

      close(10)

      ! Calcula o determinante da matriz original
      det_a = determinante(nome_arquivo_entrada)

      if (det_a .eq. 0.0d0) stop

      ! Calcula cada componente da solucao
      do j=1,N

          ! Copia a matriz original para a matriz auxiliar
          do i=1,N
              do k=1,N
                  matriz_aux(i,k) = matriz(i,k)
              end do
          end do

          ! Substitui a coluna j pelo vetor y
          do i=1,N
              matriz_aux(i,j) = y(i)
          end do

          ! Escreve a matriz A_j no arquivo temporario
          open(30,file=nome_arquivo_temp,status='unknown')

          do i=1,N
              write(30,*) (matriz_aux(i,k),k=1,N)
          end do

          close(30)

          ! Calcula o determinante de A_j
          det_ai = determinante(nome_arquivo_temp)

          ! Regra de Cramer
          x(j) = det_ai/det_a

      end do

      return
      end