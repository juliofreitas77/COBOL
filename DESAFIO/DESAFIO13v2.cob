      ******************************************************************
      * Author: JULIO CESAR S FREITAS
      * Date: 19/11/2019
      * Purpose: Elaborar um programa para ler o arquivo
      * CalendarioTrabalho.txt, e listar:
      * a) os dias sem intervalo para almoço;
      * b) a quantidade de dias com intervalo para almoço menor que 1
      *     (uma) hora;
      * c) quantos finais de semana (sábado e/ou domingo) trabalhados.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DESAFIO13.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT  ARQUIVO-CALENDAR
               ASSIGN TO 'C:\cobol\COBOL\DESAFIO\Calendario.txt'
               ORGANISATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.
       DATA DIVISION.
       FILE SECTION.
       FD ARQUIVO-CALENDAR.
           01 REG-ENTRADA.
               05 REGISTRO-STRING      PIC X(33).

       WORKING-STORAGE SECTION.
       01 FIM-ARQUIVO PIC X.
       01 WS-FILE-STATUS PIC 9(2).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

       1000-INICIAR.
           OPEN INPUT ARQUIVO-CALENDAR.
           MOVE "N" TO FIM-ARQUIVO.

           IF WS-FILE-STATUS EQUAL 00
               PERFORM 2100-LER-REGISTRO
               PERFORM 2200-EXIBIR-REGISTRO
               UNTIL FIM-ARQUIVO EQUAL "S"
           ELSE
               DISPLAY "ARQUIVO SEM REGISTROS"
               PERFORM 3000-FINALIZAR
           END-IF.


       2000-PRINCIPAL.
           PERFORM 1000-INICIAR
           .

       2100-LER-REGISTRO SECTION.
           READ ARQUIVO-CALENDAR RECORD AT END
           MOVE "S" TO FIM-ARQUIVO
           .

       2200-EXIBIR-REGISTRO.
           DISPLAY REGISTRO-STRING
      *     PERFORM 2200-EXIBIR-REGISTRO
           .


       3000-FINALIZAR.
                CLOSE ARQUIVO-CALENDAR.
           STOP RUN.

       END PROGRAM DESAFIO13.
