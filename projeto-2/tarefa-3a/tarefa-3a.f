      program tarefa3a
      
      integer*4 k, k_total, t, k_min
      integer*4 precisao_ok

      real*8 delta
      real*8 x

      real*8 soma_x(200)
      real*8 soma_x2(200)
      real*8 soma_x4(200)

      real*8 pos50(10**6)
      real*8 pos100(10**6)
      real*8 pos200(10**6)

      real*8 media_x
      real*8 media_x2
      real*8 media_x4

      real*8 desvio_x
      real*8 desvio_x2

      real*8 erro_x
      real*8 erro_x2

      real*8 epsilon
      parameter(epsilon = (10d0)**(-1))
      
      ! Inicializa os acumuladores
      do t = 1, 200
        soma_x(t) = 0d0
        soma_x2(t) = 0d0
        soma_x4(t) = 0d0
      end do

      k_total = 10**6
      k_min = 0

      ! Gera k caminhos
      do k = 1, k_total

        x = 0d0

        ! Cada caminho com 200 passos
        do t = 1, 200

          x = x + delta()

          soma_x(t) = soma_x(t) + x
          soma_x2(t) = soma_x2(t) + x**2
          soma_x4(t) = soma_x4(t) + x**4

          ! Distribuicoes P(x) para t = 50, 100, 200
          if (t .eq. 50) then
            pos50(k) = x
          else if (t .eq. 100) then
            pos100(k) = x
          else if (t .eq. 200) then
            pos200(k) = x
          end if

        end do

        ! A cada caminho gerado, verifica a precisao para todos os tempos
        if (k .ge. 2) then

          precisao_ok = 1

          do t = 1, 200

            media_x = soma_x(t)/dble(k)
            media_x2 = soma_x2(t)/dble(k)
            media_x4 = soma_x4(t)/dble(k)

            ! Desvio padrao de x
            desvio_x = dsqrt(media_x2 - media_x**2)

            ! Desvio padrao de x^2
            desvio_x2 = dsqrt(media_x4 - media_x2**2)

            ! Erro das medias
            erro_x = (desvio_x/dsqrt(dble(k)))
            erro_x2 = (desvio_x2/dsqrt(dble(k)))

            ! Se qualquer tempo nao atingir a precisao,
            ! este k ainda nao e suficiente
            if (erro_x .gt. epsilon) then
              precisao_ok = 0
            end if

            if (erro_x2 .gt. epsilon) then
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
      open(unit=10, file='tarefa-3a-saida-1.dat')
      open(unit=20, file='tarefa-3a-saida-2.dat')
      open(unit=30, file='tarefa-3a-saida-3.dat')

      ! Histogramas
      do k = 1, k_min
        write(10, *) 50, pos50(k)
      end do

      do k = 1, k_min
        write(10, *) 100, pos100(k)
      end do

      do k = 1, k_min
        write(10, *) 200, pos200(k)
      end do

      ! Médias e médias quadráticas
      do t = 1, 200
        write(20, *) t, soma_x(t)/dble(k_min)
        write(30, *) t, soma_x2(t)/dble(k_min)
      end do

      close(10)
      close(20)
      close(30)

      end

      function delta()
      ! Gera um numero aleatorio uniformemente distribuido entre -1 e 1.
      ! Entrada: Nenhuma.
      ! Saida: delta - numero aleatorio gerado no intervalo.

      real*8 delta
      
      delta = 2d0 * rand() - 1d0 ! bijeção linear entre [0,1) e [-1,1]
      
      return
      end