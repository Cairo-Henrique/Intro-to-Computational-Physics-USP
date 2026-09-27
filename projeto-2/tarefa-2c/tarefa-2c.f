      program tarefa2c

      character(50) arquivo_saida

      real*8 lambda
      real*8 gerar_x, x

      integer*4 num_dados

      integer*4 iseed
      parameter(iseed = 42)

      call srand(iseed)

      arquivo_saida = 'tarefa-2c-saida-1.dat'
      open(unit=10, file=arquivo_saida)

      lambda = 0.1d0
      num_dados = 10**4

      do i = 1, num_dados
        x = gerar_x(lambda)
        write(10, *) x
      end do

      close(10)

      end

      function gerar_x(lambda)
      ! Gera um número aleatório seguindo uma distribuição exponencial de parâmetro lambda.
      ! Entrada: lambda, parâmetro positivo da distribuição exponencial.
      ! Saída: gerar_x, valor aleatório gerado segundo p(x) = lambda*exp(-lambda*x).
      real*8 lambda
      real*8 gerar_x
      r = rand()
      gerar_x = - (1d0/lambda) * log(1d0-r)
      return
      end