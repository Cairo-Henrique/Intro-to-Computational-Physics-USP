      program tarefa5b
      ! Pêndulo amortecido e forçado
      ! As grandezas físicas são adimensionalizadas
      ! Utiliza o método de Euler-Cromer
      ! Retorna omega(theta) com base nos dados do program tarefa5a

      implicit none

      real*8 tau, theta, omega, E
      integer*4 i, iostat
      character(len=25) filename_entrada, filename_saida

      ! Executa simulação para diferentes valores de alpha
      do i = 1, 6
          ! Abre o arquivo de entrada gerado pelo programa tarefa5a
          write(filename_entrada,'(A,I1,A)') 
     &     'tarefa-5a-saida-', i, '.dat'
          open(unit=10, file=filename_entrada, iostat=iostat)
          ! Abre o arquivo de saída
          write(filename_saida,'(A,I1,A)') 
     &     'tarefa-5b-saida-', i, '.dat'
          open(unit=20, file=filename_saida)

          do while (iostat .eq. 0)
              ! Lê os valores de theta, omega e epsilon do arquivo de entrada
              read(10,*,iostat=iostat) tau, theta, omega, E
              if (iostat .ne. 0) exit
              ! Escreve os valores de theta e omega no arquivo de saída
              write(20,*) theta, omega
          end do

          close(10)
          close(20)
      end do

      end program