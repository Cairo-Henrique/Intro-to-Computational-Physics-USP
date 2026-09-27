      program tarefa3b
      
      integer*4 k, k_total, t, k_min
      integer*4 precisao_ok

      real*8 delta
      real*8 x, y, r2

      real*8 soma_r2(200)
      real*8 soma_r4(200)

      real*8 media_r2
      real*8 media_r4

      real*8 desvio_r2

      real*8 erro_r2

      real*8 epsilon
      parameter(epsilon = (10d0)**(-1))
      
      ! Inicializa os acumuladores
      do t = 1, 200
        soma_r2(t) = 0d0
        soma_r4(t) = 0d0
      end do

      k_total = 10**7
      k_min = 0

      ! Gera k caminhos
      do k = 1, k_total

        x = 0d0
        y = 0d0

        ! Cada caminho com 200 passos
        do t = 1, 200

          x = x + delta() ! coordenada horizontal
          y = y + delta() ! coordenada vertical
          r2 = x**2 + y**2 ! distância quadrática

          soma_r2(t) = soma_r2(t) + r2
          soma_r4(t) = soma_r4(t) + r2**2 ! para cálculo do desvio padrão

        end do

        ! A cada caminho gerado, verifica a precisao para todos os tempos
        if (k .ge. 2) then

          precisao_ok = 1

          do t = 1, 200

            media_r2 = soma_r2(t)/dble(k)
            media_r4 = soma_r4(t)/dble(k)

            ! Desvio padrao da média de r^2
            desvio_r2 = dsqrt(media_r4 - media_r2**2)

            ! Erro da média de r^2
            erro_r2 = desvio_r2/dsqrt(dble(k))

            ! Se qualquer tempo nao atingir a precisao,
            ! este k ainda nao e suficiente

            if (erro_r2 .gt. epsilon) then
              precisao_ok = 0
            end if

          end do

          ! Se todos os tempos atingiram a precisao,
          ! encontramos o k necessario
          if (precisao_ok .eq. 1) then
            k_min = k
            goto 100
          end if

        end if

      end do

100   continue

      print *, 'k necessario = ', k_min
      print *, 'precisao = ', epsilon

      ! Abrir arquivos
      open(unit=10, file='tarefa-3b-saida-1.dat')

      ! Médias de r^2
      do t = 1, 200
        write(10, *) t, soma_r2(t)/dble(k_min)
      end do

      close(10)

      end

      function delta()
      ! Gera um numero aleatorio uniformemente distribuido entre -1 e 1.
      ! Entrada: Nenhuma.
      ! Saida: delta - numero aleatorio gerado no intervalo.

      real*8 delta
      
      delta = 2d0 * rand() - 1d0 ! bijeção linear entre [0,1) e [-1,1]
      
      return
      end