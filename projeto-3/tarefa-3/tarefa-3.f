      program tarefa3
      ! Raizes de funções

      implicit none

      real*8 a, b, valor_exato_1, valor_exato_2, valor_exato_3
      real*8 espacamento, x_menos_original, x_mais_original
      real*8 tolerancia

      parameter(tolerancia = (10d0)**(-6))

      ! Intervalo de procura de raizes da função
      a = -10d0
      b = 10d0

      ! Valor exato da primeira raiz da função no intervalo [a,b]
      valor_exato_1 = 7d0/3d0

      ! Valor exato da segunda raiz da função no intervalo [a,b]
      valor_exato_2 = 23d0/3d0
      
      ! Valor exato da terceira raiz da função no intervalo [a,b]
      valor_exato_3 = 28d0/3d0

      ! espaçamento entre os pontos de procura de raizes
      espacamento = 0.1d0

      ! ! PRIMEIRA RAÍZ ! !
      ! Busca direta do intervalo onde está a raiz
      call busca_direta(x_menos_original, x_mais_original, 
     & a, b, espacamento)
      print *, 'Raiz no intervalo: ', 
     & x_menos_original, ' a ', x_mais_original

      ! Roda os três metodos
      call rodar_metodos(x_menos_original, x_mais_original, tolerancia)

      ! printa os valores exatos na linha final
      print *, 'Valor exato da primeira raiz:', valor_exato_1

      ! ! SEGUNDA RAÍZ ! !
      call busca_direta(x_menos_original, x_mais_original, 
     & valor_exato_1+0.1d0, b, espacamento)
      print *, 'Raiz no intervalo: ', 
     & x_menos_original, ' a ', x_mais_original

      call rodar_metodos(x_menos_original, x_mais_original, tolerancia)

      print *, 'Valor exato da segunda raiz:', valor_exato_2

      ! ! TERCEIRA RAÍZ ! !
      call busca_direta(x_menos_original, x_mais_original, 
     & valor_exato_2+0.1d0, b, espacamento)
      print *, 'Raiz no intervalo: ', 
     & x_menos_original, ' a ', x_mais_original

      call rodar_metodos(x_menos_original, x_mais_original, tolerancia)

      print *, 'Valor exato da terceira raiz:', valor_exato_3

      end program

      ! FUNÇÕES

      function f(x)
      real*8 x, f
      f =  27*x**3 - 522*x**2 + 3003*x - 4508
      return
      end function

      function D_f(x)
      ! Derivada analítica da função f(x)
      real*8 x, D_f
      D_f = 81*x**2 - 1044*x + 3003
      return
      end function

      ! SUBROTINAS

      subroutine busca_direta(x_menos, x_mais, a, b, espacamento)
      ! Busca diretamente um intervalo [x_menos, x_mais] que contenha
      ! uma raiz de f(x), percorrendo o intervalo [a,b] com passos
      ! de tamanho espacamento. A busca termina quando encontra dois
      ! pontos consecutivos nos quais f(x) possui sinais opostos 
      ! (Teorema de Bolzano) ou quando o limite superior b e atingido.
      !
      ! Entrada:
      !   a           - limite inferior do intervalo de busca
      !   b           - limite superior do intervalo de busca
      !   espacamento - tamanho do passo utilizado na busca
      !
      ! Saida:
      !   x_menos     - extremo inferior do intervalo que contem a raiz
      !   x_mais      - extremo superior do intervalo que contem a raiz
      real*8 x_menos, x_mais, a, b, espacamento
      real*8 f, f_menos, f_mais
      x_menos = a
      do while (x_menos .LT. b)
        x_mais = x_menos + espacamento
        f_mais = f(x_mais)
        f_menos = f(x_menos)
        if (f_mais * f_menos .LE. 0d0) then
          return
        end if
        x_menos = x_mais
      end do
      return
      end subroutine

      subroutine rodar_metodos(
     & x_menos_original, x_mais_original, tolerancia)
      ! Executa os tres metodos numericos utilizados para encontrar
      ! as raizes da funcao: bissecao, Newton-Raphson e secante.
      ! Cada metodo e inicializado com os mesmos valores originais
      ! do intervalo e com a mesma tolerancia.
      !
      ! Entrada:
      !   x_menos_original - extremo inferior do intervalo inicial
      !   x_mais_original  - extremo superior do intervalo inicial
      !   tolerancia       - criterio de parada dos metodos
      !
      ! Saida:
      !   Os resultados de cada metodo sao apresentados na tela.

      real*8 x_menos_original, x_mais_original, tolerancia
      real*8 x_menos, x_mais

      print *, 'Metodo da Bissecao'
      x_menos = x_menos_original
      x_mais = x_mais_original
      call bissecao(x_menos, x_mais, tolerancia)

      print *, 'Metodo de Newton-Raphson'
      x_menos = x_menos_original
      call newtonraphson(x_menos, tolerancia)

      print *, 'Metodo da Secante'
      x_menos = x_menos_original
      x_mais = x_mais_original
      call secante(x_menos, x_mais, tolerancia)

      return
      end subroutine

      subroutine bissecao(x_menos, x_mais, tolerancia)
      ! Calcula uma raiz de f(x) pelo metodo da bissecao.
      ! O intervalo e sucessivamente dividido ao meio, mantendo-se
      ! sempre o subintervalo no qual a funcao muda de sinal. O
      ! processo termina quando o tamanho do intervalo e menor que
      ! a tolerancia especificada.
      !
      ! Entrada:
      !   x_menos  - extremo inferior do intervalo inicial
      !   x_mais   - extremo superior do intervalo inicial
      !   tolerancia - tolerancia utilizada como criterio de parada
      !
      ! Saida:
      !   A cada iteracao sao apresentados o numero da iteracao e
      !   os extremos atuais do intervalo. Ao final, x_menos e
      !   x_mais delimitam um intervalo contendo a raiz.
      real*8 x_menos, x_mais, tolerancia
      real*8 x_meio, f_menos, f_mais, f_meio
      real*8 f
      integer*4 i
      f_menos = f(x_menos)
      f_mais = f(x_mais)
      i = 0
      do while (abs(x_mais - x_menos) .GE. tolerancia)
        print *, i, x_menos, x_mais
        x_meio = (x_menos + x_mais) / 2d0
        f_meio = f(x_meio)
        if (f_meio * f_menos .LT. 0d0) then
          x_mais = x_meio
          f_mais = f_meio
        else
          x_menos = x_meio
          f_menos = f_meio
        end if
        i = i + 1
      end do
      print *, i, x_menos, x_mais
      return
      end subroutine

      subroutine newtonraphson(x, tolerancia)
      ! Calcula uma raiz de f(x) pelo metodo de Newton-Raphson.
      ! A cada iteracao, utiliza o valor da funcao e de sua derivada
      ! no ponto atual para obter uma nova aproximacao da raiz.
      ! O processo termina quando a diferenca entre duas aproximacoes
      ! consecutivas e menor que a tolerancia especificada.
      !
      ! Entrada:
      !   x         - aproximacao inicial da raiz
      !   tolerancia - tolerancia utilizada como criterio de parada
      !
      ! Saida:
      !   x         - aproximacao da raiz obtida pelo metodo
      real*8 x, tolerancia, x_anterior, f_x, D_f_x
      real*8 f, D_f
      integer*4 i
      i = 0
      x_anterior = 10**9
      do while (abs(x - x_anterior) .GE. tolerancia)
        print *, i, x
        f_x = f(x)
        D_f_x = D_f(x)
        x_anterior = x
        x = x - (f_x / D_f_x)
        i = i + 1
      end do
      print *, i, x
      return
      end subroutine

      subroutine secante(x_anterior, x, tolerancia)
      ! Calcula uma raiz de f(x) pelo metodo da secante.
      ! A cada iteracao, utiliza duas aproximacoes consecutivas para
      ! construir uma reta secante a funcao e obter uma nova
      ! aproximacao da raiz. O processo termina quando a diferenca
      ! entre duas aproximacoes consecutivas e menor que a tolerancia.
      !
      ! Entrada:
      !   x_anterior - primeira aproximacao inicial da raiz
      !   x          - segunda aproximacao inicial da raiz
      !   tolerancia - tolerancia utilizada como criterio de parada
      !
      ! Saida:
      !   x           - aproximacao da raiz obtida pelo metodo
      real*8 x_anterior, x, tolerancia
      real*8 f_x, f_x_anterior
      real*8 x_novo
      real*8 f
      integer*4 i
      i = 0
      do while (abs(x - x_anterior) .GE. tolerancia)
        print *, i, x_anterior, x
        f_x = f(x)
        f_x_anterior = f(x_anterior)
        x_novo = x - f_x * (x - x_anterior) / (f_x - f_x_anterior)
        x_anterior = x
        x = x_novo
        i = i + 1
      end do
      print *, i, x_anterior, x
      return
      end