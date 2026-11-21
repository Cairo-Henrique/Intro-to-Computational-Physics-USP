      program tarefa5c
      ! Pêndulo amortecido e forçado
      ! As grandezas físicas são adimensionalizadas
      ! Utiliza o método de Euler-Cromer
      ! Retorna as seções de Poincaré no espaço de fase
      ! com base nos dados do program tarefa5a

      implicit none

      integer*4 i, iostat
      real*8 tau, theta, omega, E
      real*8 nu, T_ext
      real*8 proximo_periodo, tau_transiente
      real*8 pi
      character(len=25) filename_entrada, filename_saida

      parameter(pi = 3.14159265358979323d0) ! valor de pi

      nu = (2d0/3d0) ! Frequência angular de excitação adimensionalizada
      T_ext = 2.0d0 * pi / nu ! Período do termo forçante
      tau_transiente = 50.0d0 ! Tempo de transiente adimensionalizado

      ! Executa simulação para diferentes valores de alpha
      do i = 1, 6
          ! Abre o arquivo de entrada gerado pelo programa tarefa5a
          write(filename_entrada,'(A,I1,A)') 
     &     'tarefa-5a-saida-', i, '.dat'
          open(unit=10, file=filename_entrada, iostat=iostat)
          ! Abre o arquivo de saída
          write(filename_saida,'(A,I1,A)')
     &     'tarefa-5c-saida-', i, '.dat'
          open(unit=20, file=filename_saida)

          proximo_periodo = 0.0d0

          do while (iostat .eq. 0)
              ! Lê os valores de theta, omega e epsilon do arquivo de entrada
              read(10,*,iostat=iostat) tau, theta, omega, E
              if (iostat .ne. 0) exit
              if (tau .ge. tau_transiente .and.
     &        tau .ge. proximo_periodo) then
                  ! Escreve os valores de theta e omega no arquivo de saída
                  write(20,*) tau, theta, omega
                  proximo_periodo = proximo_periodo + T_ext
              end if
          end do
          
          close(10)
          close(20)
      end do

      end program