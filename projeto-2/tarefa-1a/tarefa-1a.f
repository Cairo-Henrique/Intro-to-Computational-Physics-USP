      program Tarefa1a

      character(50) arquivo_media, arquivo_desvio_padrao
      
      integer*4 k, m, N
      parameter (k = 100)
      
      real*8 media, media_sequencia
      real*8 desvio_padrao, desvio_padrao_sequencia
      real*8 sequencia(3**9)
      
      integer*4 iseed
      parameter (iseed = 42)
      call srand(iseed)

      arquivo_media = 'tarefa-1a-saida-1.dat'
      arquivo_desvio_padrao = 'tarefa-1a-saida-2.dat'

      open(unit=10, file=arquivo_media)
      open(unit=20, file=arquivo_desvio_padrao)

      ! Para cada N = 3^m, gera k sequências e coloca os dados nos arquivos de saída
      do m = 1, 9
            N = 3 ** m
            do i = 1, k
                  call gerar_sequencia(sequencia, N)
                  media_sequencia = media(sequencia, N)
                  desvio_padrao_sequencia = desvio_padrao(sequencia, N)
                  print *, media_sequencia
                  print *, desvio_padrao_sequencia
                  write(10, *) N, media_sequencia
                  write(20, *) N, desvio_padrao_sequencia
            end do
      end do

      close(10)
      close(20)
      
      end

      subroutine gerar_sequencia(sequencia, tamanho)
      ! Gera uma sequencia de numeros aleatorios uniformemente
      ! Entrada: tamanho - numero de elementos da sequencia.
      ! Saida: sequencia - vetor contendo os numeros aleatorios.
      
      integer*4 tamanho
      real*8 sequencia(3**9)

      do i = 1, tamanho
            sequencia(i) = rand()
      end do

      end

      function media(sequencia, tamanho)
      ! Calcula a media aritmetica dos elementos de uma sequencia.
      ! Entrada: tamanho - numero de elementos da sequencia.
      ! Saida: media - media aritmetica dos elementos da sequencia.
      
      integer*4 tamanho
      real*8 soma, media
      real*8 sequencia(3**9)

      soma = 0
      do i = 1, tamanho
            soma = soma + sequencia(i)
      end do

      media = soma / tamanho

      return
      end

      function desvio_padrao(sequencia, tamanho)
      ! Calcula o desvio padrao dos elementos de uma sequencia
      ! utilizando a fórmula sigma = <x^2> - <x>^2.
      ! Entrada: tamanho - numero de elementos da sequencia.
      ! Saida: desvio_padrao - desvio padrao da sequencia.
      
      integer*4 tamanho
      real*8 media, desvio_padrao
      real*8 media_sequencia, media_quadratica_sequencia
      real*8 sequencia(3**9), sequencia_quadrada(3**9)
      save sequencia_quadrada

      do i = 1, tamanho
            sequencia_quadrada(i) = sequencia(i)**2
      end do

      media_sequencia = media(sequencia, tamanho)

      media_quadratica_sequencia = media(sequencia_quadrada, tamanho)

      desvio_padrao = 
     &sqrt(media_quadratica_sequencia - media_sequencia**2)

      return
      end