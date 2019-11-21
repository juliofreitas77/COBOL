      ******************************************************************
      * Author: Julio Cesar
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DESAFIO11.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WCA-VARIAVEIS.
           05 DATA-V.
               10 ANO PIC S9(04).
               10 FILLER PIC X(1) VALUE "-".
               10 MES PIC S9(02).
               10 FILLER PIC X(1) VALUE "-".
               10 DIA PIC 9(02).
           05 VALIDADOR PIC X(08).
           05 BISSEXTO PIC X(1) VALUE "A".
           05 AUX1     PIC 9(05) VALUES ZEROS.
           05 AUX2     PIC 9(05) VALUES ZEROS.
           05 AUX3     PIC 9(05) VALUES ZEROS.
           05 AUX4     PIC 9(05) VALUES ZEROS.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 1000-INICIO
           PERFORM 2000-FIM
           STOP RUN
           .
       1000-INICIO.

           DISPLAY "QUAL A DATA?"
           ACCEPT DATA-V

           MOVE "VALIDA" TO VALIDADOR

           DIVIDE ANO BY 4 GIVING AUX1 REMAINDER AUX2
           DIVIDE ANO BY 100 GIVING AUX3 REMAINDER AUX4

           IF AUX2 EQUAL ZEROS AND AUX4 NOT EQUALS ZEROS
               MOVE "S" TO BISSEXTO
           ELSE
               MOVE "N" TO BISSEXTO
           END-IF.

           IF DIA GREATER THAN 31
            THEN MOVE "INVALIDA" TO VALIDADOR
           END-IF.

           IF MES EQUAL 02 AND BISSEXTO EQUAL "S"
               AND DIA GREATER THAN 29
             THEN MOVE "INVALIDA" TO VALIDADOR
           END-IF.

           IF MES EQUAL 02 AND BISSEXTO EQUAL "N"
               AND DIA GREATER THAN 28
             THEN MOVE "INVALIDA" TO VALIDADOR
           END-IF.

           IF (MES EQUAL 04 OR MES EQUAL 06 OR MES EQUAL 09
               OR MES EQUAL 11) AND DIA GREATER THAN 30
             THEN MOVE "INVALIDA" TO VALIDADOR
           END-IF.

           IF MES LESS THAN 1 OR GREATER THAN 12
               THEN MOVE "INVALIDA" TO VALIDADOR
           END-IF

           IF ANO LESS THAN 1601
               THEN MOVE "INVALIDA" TO VALIDADOR
           END-IF
           .
       2000-FIM.
           DISPLAY "A DATA DE ENTRADA FOI: " DATA-V "."
           DISPLAY "ESSA E UMA DATA: " VALIDADOR "."
           .
            STOP RUN.
       END PROGRAM DESAFIO11.
