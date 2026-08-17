      program Tarefa2
      ! Recebe 3 vetores e 
      ! calcula area lateral e o volume do prisma de base triangular
      ! formado por v1, v2 e v1-v2 e as arestas laterais sao paralelas a v3
      
      real * 8 x1, y1, z1, x2, y2, z2, x3, y3, z3
      real * 8 area_lateral, volume
      real * 8 prod_misto, modulo_prod_vetorial
      real * 8 prod_misto_v1_v2_v3

      print *, 'Digite o vetor v1 (x1, y1, z1)'
        read *, x1, y1, z1
      print *, 'Digite o vetor v2 (x2, y2, z2)'
        read *, x2, y2, z2
      print *, 'Digite o vetor v3 (x3, y3, z3)'
        read *, x3, y3, z3

      prod_misto_v1_v2_v3 = 
     &prod_misto(x1, y1, z1, x2, y2, z2, x3, y3, z3)

      ! Interrompe se v1 e v2 forem colineares
      if (modulo_prod_vetorial(x1, y1, z1, x2, y2, z2) .EQ. 0.0) then
        print *, 'Os vetores v1 e v2 sao colineares.'
        stop
      end if
      
      ! Interrompe se os vetores forem coplanares
      if (prod_misto_v1_v2_v3 .EQ. 0.0) then
        print *, 'Os vetores v1, v2 e v3 sao coplanares.'
        stop
      end if

      ! A area lateral do prisma eh a soma das areas dos 3 paralelogramos formados por v1, v2 e v1-v2 com v3
      area_lateral = 
     &modulo_prod_vetorial(x1, y1, z1, x3, y3, z3) +
     &modulo_prod_vetorial(x2, y2, z2, x3, y3, z3) +
     &modulo_prod_vetorial(x1-x2, y1-y2, z1-z2, x3, y3, z3)

      ! O volume do prisma eh o modulo do produto misto de v1, v2 e v3 (paralelepipedo) dividido por 2
      volume = abs(prod_misto_v1_v2_v3) / 2

      print *, 'Area lateral do prisma: ', area_lateral
      print *, 'Volume do prisma: ', volume
        
      stop
      end

      function prod_escalar(x1, y1, z1, x2, y2, z2)
      ! Retorna produto escalar de dois vetores 3D
      ! Inputs: (x1, y1, z1) e (x2, y2, z2) - coordenadas dos vetores
      ! Output: dot - valor do produto escalar
      real * 8 prod_escalar
      real * 8 x1, y1, z1, x2, y2, z2
      prod_escalar = x1*x2 + y1*y2 + z1*z2
      return
      end

      function prod_misto(x1, y1, z1, x2, y2, z2, x3, y3, z3)
        ! Retorna produto misto de tres vetores 3D
        ! Inputs: (x1, y1, z1), (x2, y2, z2) e (x3, y3, z3) - coordenadas dos vetores
        ! Output: prod_misto - valor do produto misto

        real * 8 prod_misto, prod_escalar
        real * 8 x1, y1, z1, x2, y2, z2, x3, y3, z3
        real * 8 cx, cy, cz

        call prod_vetorial(x1, y1, z1, x2, y2, z2, cx, cy, cz)
        prod_misto = prod_escalar(cx, cy, cz, x3, y3, z3)

        return
      end

      function modulo_prod_vetorial(x1, y1, z1, x2, y2, z2)
        ! Retorna modulo do produto vetorial de dois vetores 3D
        ! Inputs: (x1, y1, z1) e (x2, y2, z2) - coordenadas dos vetores
        ! Output: modulo_prod_vetorial - valor do modulo do produto vetorial

        real * 8 modulo_prod_vetorial
        real * 8 x1, y1, z1, x2, y2, z2
        real * 8 cx, cy, cz

        call prod_vetorial(x1, y1, z1, x2, y2, z2, cx, cy, cz)
        modulo_prod_vetorial = sqrt(cx**2 + cy**2 + cz**2)

        return
      end

      subroutine prod_vetorial(x1, y1, z1, x2, y2, z2, cx, cy, cz)
      ! Retorna produto vetorial de dois vetores 3D
      ! Inputs: (x1, y1, z1) e (x2, y2, z2) - coordenadas dos vetores
      ! Output: (cx, cy, cz) - coordenadas do vetor resultante

        real * 8 x1, y1, z1, x2, y2, z2
        real * 8 cx, cy, cz

        cx = y1*z2 - z1*y2
        cy = z1*x2 - x1*z2
        cz = x1*y2 - y1*x2

        return
      end