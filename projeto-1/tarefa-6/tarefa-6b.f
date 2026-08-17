      program Tarefa6b
      ! Usando o raio R=0.9, 1.0, 1.1, calcula os volumes das d-esferas nas dimensões d = 0, 1, 2, . . . , 25. 
      ! Os resultados devem estar em um arquivo de saída.

      real*8 Ra, Rb, Rc, volume, volume_d_esfera, pi
      integer*4 d, d_max
      parameter (pi = 3.14159265358979323)
      parameter (d_max = 25)

      Ra = 0.9
      Rb = 1.0
      Rc = 1.1

      open(10, file='tarefa-6b-saida-1.dat', status='unknown')
      print *, 'R =', Ra

      do 10 d = 0, d_max
        volume = volume_d_esfera(Ra, d)
        write(10, *) d, volume
10    continue

      open(20, file='tarefa-6b-saida-2.dat', status='unknown')
      print *, 'R =', Rb

      do 20 d = 0, d_max
        volume = volume_d_esfera(Rb, d)
        write(20, *) d, volume
20    continue

      open(30, file='tarefa-6b-saida-3.dat', status='unknown')
      print *, 'R =', Rc

      do 30 d = 0, d_max
        volume = volume_d_esfera(Rc, d)
        write(30, *) d, volume
30    continue
      
      stop
      end

      function fatorial(n)
      ! Calcula o fatorial de n
      ! Input: n - número inteiro não-negativo
      ! Output: fatorial - valor do fatorial de n
      real*8 fatorial
      integer*4 i, n
      fatorial = 1
      do 40 i = 2, n
            fatorial = fatorial * i
40    continue
      return
      end

      function gama(x)
      ! Calcula a função gama de x para x = 1 + d/2 com d natural
      ! Input: x - valor real
      ! Output: gama - valor da função gama de x
      real*8 pi, gama, fatorial, x
      integer*4 i, n
      parameter (pi = 3.14159265358979323)

      if (x .eq. int(x)) then
          ! Se x é inteiro, gama(x) = fatorial(x-1)
          n = int(x)-1
          gama = fatorial(n)
      else
          ! Se x não é inteiro, usa o fato que gama(1/2) = sqrt(pi)
          gama = pi**(0.5)
          ! Usa gama(x) = (x-1)(x-2)... gama(1/2)
          n = int(x - 0.5)
          if (n .ne. 0) then
            do 50 i = 1, n
                gama = gama * (x-i)
50          continue
          end if
      end if
      end

      function volume_d_esfera(R, d)
      ! Calcula o volume da d-esfera de raio R e dimensão d
      ! Inputs: R - raio da esfera, d - dimensão da esfera
      ! Output: volume - volume da d-esfera
      real*8 volume_d_esfera, R, pi, gama, d_real
      integer*4 d
      parameter (pi = 3.14159265358979323)
      
      d_real = dble(d)
      volume_d_esfera = (R ** d) * (pi**(d/2.0)) / gama(1 + d_real/2.0)
      end