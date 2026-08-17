      program Tarefa1;
      ! Recebe r1, r2 e calcula a area e volume do torus

      real * 8 r1, r2, pi, area, volume
      
      parameter (pi = 3.14159265358979323)
      
      print *, 'Digite o raio interno'
      read *, r1
      print *, 'Digite o raio externo'
      read *, r2

      area = 4 * pi**2 * r1 * r2
      volume = 2 * pi**2 * r2 * r1**2

      print *, 'Area do torus: ', area
      print *, 'Volume do torus: ', volume

      stop
      end