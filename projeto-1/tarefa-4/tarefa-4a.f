      program Tarefa4a
      ! Calcula o cosseno de x (real*4) usando a série de Taylor e compara com a função nativa do Fortran
      ! Usaremos a precisão epsilon para determinar quando parar de adicionar termos na série de Taylor.

      real*4 x, epsilon, taylor_cos, taylor_cos_x, fortran_cos, abs_diff
      parameter (epsilon = 1.0e-5)

      print *, 'Digite o valor de x (em radianos):'
      read *, x

      ! Calcula o valor do cosseno usando a série de Taylor
      taylor_cos_x = taylor_cos(x, epsilon)

      ! Calcula o valor do cosseno usando a função nativa do Fortran
      fortran_cos = cos(x)

      ! Calcula a diferença absoluta
      abs_diff = abs(taylor_cos_x - fortran_cos)

      print *, taylor_cos_x, fortran_cos, abs_diff

      stop
      end

      function taylor_cos(x, epsilon)
      ! Calcula o cosseno de x usando a série de Taylor
      ! Inputs: x - valor em radianos, epsilon - precisão desejada
      ! Output: taylor_cos - valor aproximado do cosseno de x
      real*4 taylor_cos, x, epsilon, fatorial
      integer*4 n
      real*4 term
      term = 1.0
      taylor_cos = 1.0
      n = 0
10    continue
      n = n + 1
      term = (-1)**n * x**(2*n) / (fatorial(2*n))
      taylor_cos = taylor_cos + term
      if (ABS(term) .GT. epsilon) goto 10 ! continua adicionando termos até que o termo seja menor que epsilon
      return
      end

      function fatorial(n)
      ! Calcula o fatorial de n
      ! Input: n - número inteiro não-negativo
      ! Output: fatorial - valor do fatorial de n
      real*4 fatorial
      integer*4 n, i
      fatorial = 1
      do 20 i = 2, n
            fatorial = fatorial * i
20    continue
      return
      end