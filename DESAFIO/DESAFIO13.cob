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
               ORGANISATION IS SEQUENTIAL .
       DATA DIVISION.
       FILE SECTION.
       FD ARQUIVO-CALENDAR.

           01 REG-ENTRADA.
               05 WS-DATA.
                   10 DIA      PIC 9(2).
                   10 FILLER   PIC X.
                   10 MES      PIC 9(2).
                   10 FILLER   PIC X.
                   10 ANO      PIC 9(2).
                   10 FILLER   PIC X.
               05 WS-HORA-ENTRADA1.
                   10 E1-HORA01    PIC 9.
                   10 FILLER       PIC X.
                   10 E1-MINUTO01  PIC 9(2).
                   10 FILLER       PIC X.
                05 WS-HORA-SAIDA1.
                   10 S1-HORA01    PIC 9(1).
                   10 FILLER       PIC X(1).
                   10 S1-MINUTO01  PIC 9(2).
                   10 FILLER       PIC X(1).
                05 WS-HORA-ENTRADA2.
                   10 E2-HORA02    PIC 9(1).
                   10 FILLER       PIC X(1).
                   10 E2-MINUTO02  PIC 9(2).
                   10 FILLER       PIC X(1).
                05 WS-HORA-SAIDA02.
                   10 S2-HORA02    PIC 9(1).
                   10 FILLER       PIC X(1).
                   10 S2-MINUTO02  PIC 9(2).

       WORKING-STORAGE SECTION.
       01 FIM-ARQUIVO PIC X.
       01 WS-FILE-STATUS PIC 9(2).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.

       1000-INICIAR.
           OPEN INPUT ARQUIVO-CALENDAR.
           MOVE "N" TO FIM-ARQUIVO.
      *     PERFORM 2100-LER-REGISTRO.
      *****VALIDA A EXISTENCIA DE REGISROS NO ARQUIVO
           IF WS-FILE-STATUS EQUAL 00
               PERFORM 2100-LER-REGISTRO
               PERFORM 2200-EXIBIR-REGISTRO
               UNTIL FIM-ARQUIVO EQUAL "S"
           ELSE
               DISPLAY "ARQUIVO SEM REGISTROS."
               PERFORM 3000-FINALIZAR
           END-IF.

           PERFORM 3000-FINALIZAR.

       2000-PRINCIPAL.
           PERFORM 1000-INICIAR
           .

       2100-LER-REGISTRO.
           READ ARQUIVO-CALENDAR RECORD AT END
           MOVE "S" TO FIM-ARQUIVO
           .

       2200-EXIBIR-REGISTRO.

           DISPLAY WS-DATA WS-HORA-ENTRADA1 WS-HORA-SAIDA1
                   WS-HORA-ENTRADA2 WS-HORA-SAIDA02.

           PERFORM 2100-LER-REGISTRO
           .


       3000-FINALIZAR.
                CLOSE ARQUIVO-CALENDAR.
           STOP RUN.

       END PROGRAM DESAFIO13.
