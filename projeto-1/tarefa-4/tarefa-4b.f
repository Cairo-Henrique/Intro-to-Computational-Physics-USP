      program Tarefa4b
      ! Calcula o cosseno de x (real*8) usando a série de Taylor e compara com a função nativa do Fortran
      ! Usaremos a precisão epsilon para determinar quando parar de adicionar termos na série de Taylor.
      ! O programa testa valores de epsilon cada vez menores até que 
      ! a diferença absoluta entre o valor calculado pela série de Taylor e o valor calculado pela função nativa do Fortran seja menor que 0.0.

      real*8 x, epsilon, taylor_cos, taylor_cos_x, fortran_cos, abs_diff

      print *, 'Digite o valor de x (em radianos):'
      read *, x

      ! Calcula o valor do cosseno usando a função nativa do Fortran
      fortran_cos = cos(x)

      ! Inicializa a precisão epsilon
      epsilon = 1.0e-4

      ! Inicializa a diferença absoluta
      abs_diff = 1.0

      ! Testa valores de epsilon cada vez menores até que a diferença absoluta seja menor que 0.0
10    continue
      epsilon = epsilon / 10.0
      taylor_cos_x = taylor_cos(x, epsilon)
      abs_diff = abs(taylor_cos_x - fortran_cos)
      print *, 'Epsilon:', epsilon
      print *, taylor_cos_x, fortran_cos, abs_diff
      if (abs_diff .gt. 0.0) goto 10

      stop
      end

      function taylor_cos(x, epsilon)
      ! Calcula o cosseno de x usando a série de Taylor
      ! Inputs: x - valor em radianos, epsilon - precisão desejada
      ! Output: taylor_cos - valor aproximado do cosseno de x
      real*8 taylor_cos, x, epsilon, fatorial
      integer*4 n
      real*8 term
      term = 1.0
      taylor_cos = 1.0
      n = 0
20    continue
      n = n + 1
      term = (-1)**n * x**(2*n) / (fatorial(2*n))
      taylor_cos = taylor_cos + term
      if (ABS(term) .GT. epsilon) goto 20 ! continua adicionando termos até que o termo seja menor que epsilon
      return
      end

      function fatorial(n)
      ! Calcula o fatorial de n
      ! Input: n - número inteiro não-negativo
      ! Output: fatorial - valor do fatorial de n
      real*8 fatorial
      integer*4 n, i
      fatorial = 1
      do 30 i = 2, n
            fatorial = fatorial * i
30    continue
      return
      end