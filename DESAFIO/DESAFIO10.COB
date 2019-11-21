      ******************************************************************
      * Author: JULIO CESAR S
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DESAFIO10.
       ENVIRONMENT DIVISION.
           CONFIGURATION SECTION.
           SPECIAL-NAMES.
               DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WCA-VARIAVEIS.
           05 ENTRADA1.
               10 H-ENTRADA1 PIC 9(02).
               10 FILLER     PIC X(1)  VALUE SPACE.
               10 M-ENTRADA1 PIC 9(02) VALUES ZEROS.
           05 S1.
               10 H-SAIDA1 PIC 9(02) VALUES ZEROS.
               10 FILLER     PIC X(1)  VALUE SPACE.
               10 M-SAIDA1 PIC 9(02) VALUES ZEROS.
           05 E2.
               10 H-ENTRADA2 PIC 9(02) VALUES ZEROS.
               10 FILLER     PIC X(1)  VALUE SPACE.
               10 M-ENTRADA2 PIC 9(02) VALUE ZEROS.
           05 S2.
               10 H-SAIDA2 PIC 9(02) VALUES ZEROS.
               10 FILLER     PIC X(1)  VALUE SPACE.
               10 M-SAIDA2 PIC 9(02) VALUES ZEROS.

       01 HORAS-TRABALHADAS PIC 9(2)V99.
       01 HORAS-ALMOCO PIC 9(2)V99.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 1000-INICIAR
           PERFORM 2000-PROCESSAR
           PERFORM 3000-FINALIZAR

           STOP RUN
           .
       1000-INICIAR.
           PERFORM 2100-VAL-ENT01
           PERFORM 2200-VAL-SAIDA1
           PERFORM 2300-VAL-ENT02
           PERFORM 2400-VAL-SAIDA

           .

       2000-PROCESSAR.
           COMPUTE HORAS-ALMOCO =
           (60*H-ENTRADA2+M-ENTRADA2 - 60*H-SAIDA1 - M-SAIDA1)/60
           COMPUTE HORAS-TRABALHADAS =
           (60*H-SAIDA2 + M-SAIDA2 - 60*H-ENTRADA1 - M-ENTRADA1)/60
           SUBTRACT HORAS-ALMOCO FROM HORAS-TRABALHADAS
           .

       3000-FINALIZAR.

           DISPLAY "-total de horas trabalhadas no dia: "
                       HORAS-TRABALHADAS
           DISPLAY "-total de horas de intervalo para almoço: "
                       HORAS-ALMOCO
           .
            STOP RUN.

       2100-VAL-ENT01.
           DISPLAY "ENTRADA MANHA?"
           ACCEPT ENTRADA1

           IF H-ENTRADA1 GREATER THAN 24 OR
              M-ENTRADA1 GREATER THAN 60
               DISPLAY "DIGITE UMA HORA VALIDA PARA "
               PERFORM 2100-VAL-ENT01
           ELSE
               CONTINUE
           END-IF.

       2200-VAL-SAIDA1.
           DISPLAY "SAIDA MANHA?"
           ACCEPT S1
           IF H-SAIDA1 GREATER THAN 24 OR
              M-SAIDA1 GREATER THAN 60
               DISPLAY "DIGITE UMA HORA VALIDA PARA PRIMEIRA SAIDA!"
               PERFORM 2200-VAL-SAIDA1
           ELSE
               CONTINUE
           END-IF.


       2300-VAL-ENT02.
           DISPLAY "ENTRADA TARDE? "
           ACCEPT E2
           IF H-ENTRADA2 GREATER THAN 24 OR
              M-ENTRADA2 GREATER THAN 60
               DISPLAY "DIGITE UMA HORA VALIDA PARA SEGUNDA ENTRADA!"
               PERFORM 2300-VAL-ENT02
           ELSE
               CONTINUE
           END-IF.

       2400-VAL-SAIDA.
           DISPLAY "FIM DE EXPEDIENTE? "
           ACCEPT S2
            IF H-SAIDA2 GREATER THAN 24 OR
              M-SAIDA2 GREATER THAN 60
               DISPLAY "DIGITE UMA HORA VALIDA PARA FIM DE EXPEDIENTE!"
               PERFORM 2400-VAL-SAIDA
           ELSE
               CONTINUE
           END-IF.

       END PROGRAM DESAFIO10.
