      program Tarefa3
      ! Lê os N números reais (REAL*8) do arquivo tarefa-3-entrada-1.in.
      ! Descobre e imprime o valor de N.
      ! Depois, lê o valor de M ≤ N e ordena apenas os M primeiros menores números desse arquivo. 
      ! O resultado é salvo em um arquivo de saída juntamente com o número M.
      ! Usaremos as funcionalidades “iostat” e “rewind”.

      character*50 filename
      integer*4 N, M, i, j, iostat
      real*8 num, val_min

      integer*4 NMAX
      parameter (NMAX = 1000000)
      real*8 numeros(NMAX), numeros_ordenados(NMAX) ! vetores para armazenar os números do arquivo

      filename = 'tarefa-3-entrada-1.in'

      ! unit 10 serve para abrir o arquivo de entrada
      ! file=filename indica o nome do arquivo a ser aberto
      ! status='old' indica que o arquivo já existe
      ! iostat é usado para verificar se o arquivo foi aberto corretamente. iostat=0 indica sucesso, iostat/=0 indica falha
      open(unit=10, file=filename, status='old',
     &iostat=iostat)
    
      ! N é o número de linhas do arquivo
      N = 0
      do 10
        read(10, *, iostat=iostat) num ! * significa que o formato de leitura será automático
        if (iostat .NE. 0) goto 20
        numeros(N+1) = num
        N = N + 1

10    continue

20    continue

      ! volta o ponteiro do arquivo para o início
      rewind(10)

      ! Imprime o valor de N
      print *, 'Numero de linhas do arquivo: N =', N

      ! Solicita ao usuário o valor de M, que deve ser menor ou igual a N
      print *, 'Digite o valor de M (M <= N):'
      read *, M

      do 30 i = 1, M
          numeros_ordenados(i) = val_min(numeros, N)

          do 40 j = 1, N
              if (numeros(j) .EQ. numeros_ordenados(i)) then
                  numeros(j) = 1.0d+300
                  goto 50
              end if
40        continue

50        continue
30    continue

      ! Cria o arquivo de saída
      open(unit=20, file='tarefa-3-saida-1.dat', status='unknown', 
     &iostat=iostat)

      ! Escreve o valor de M no arquivo de saída
      write(20, *) M

      ! Imprime e salva os M menores números em ordem crescente
      print *, 'Os ', M, ' menores numeros em ordem crescente sao:'
      do 60 i = 1, M
        write(20, *) numeros_ordenados(i)
        print *, numeros_ordenados(i)
60    continue
      
      stop
      end

      function val_min(numeros, N)
      ! Retorna o menor valor do vetor numeros de tamanho N
      ! Inputs: numeros - vetor de números reais, N - tamanho do vetor
      ! Output: val_min - menor valor do vetor numeros
      integer*4 N
      real*8 numeros(N), val_min
      integer*4 i
      val_min = numeros(1)
      do 70 i = 2, N
          if (numeros(i) .LT. val_min) then
              val_min = numeros(i)
          end if
70    continue
      end