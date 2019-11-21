      ******************************************************************
      * Author:
      * Date:
      * Purpose:  Elabore um algoritmo que imprima todos os números
      * primos existentes entre Número Inicial (nroIni) e Número Final
      *(nroFim). nroIni e nroFim são números naturais fornecidos
      * pelo usuário.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       1   WS-NROINI        PIC 9(2) VALUES ZEROS.
       1   WS-NROFIM        PIC 9(3) VALUES ZEROS.
       1   WS-COUNT         PIC 9(2) VALUES 1.
       1   WS-COUNT2        PIC 9(2) VALUES 1.
       1   WS-AUX           PIC 9(2) VALUES 1.
       1   WS-AUX2           PIC 9(2) VALUES 1.
       1   WS-MOD          PIC 9(2) VALUES 1.
       1   WS-RESULT       PIC 9(2) VALUES 1.
       1   WS-COUNT-PRIMOS  PIC 9(2) VALUES 1.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            DISPLAY "DIGITE NUMERO INICIAL: "
            ACCEPT WS-NROINI

            DISPLAY "DIGITE O NUMERO FINAL: "
            ACCEPT WS-NROFIM

           PERFORM UNTIL WS-NROINI EQUAL WS-NROFIM + 1
               SET WS-COUNT2 TO 2
               PERFORM UNTIL WS-AUX2 = WS-AUX + 1
                   DIVIDE WS-AUX2 BY WS-COUNT2 GIVING WS-RESULT
                      REMAINDER WS-MOD
               IF WS-MOD EQUALS ZERO OR WS-AUX2 EQUAL 2
                   DISPLAY WS-AUX2 ' ' WITH NO ADVANCING
                   ADD 1 TO WS-COUNT-PRIMOS
               END-IF

               ADD 1 TO WS-AUX2
               ADD 1 TO WS-COUNT2
           END-PERFORM

           ADD 1 TO WS-AUX
               END-PERFORM
           .

           DISPLAY "QUANTIDADE DE NUMEROS PRIMOS: " WS-COUNT-PRIMOS
           .
       END PROGRAM YOUR-PROGRAM-NAME.
