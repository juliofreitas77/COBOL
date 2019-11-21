      ******************************************************************
      * Author: Julio Cesar
      * Date: 14/11/2019
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DESAFIO09.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WCA-VARIAVEIS.
           05 WS-QTD-GRAOS         PIC 9(20) VALUES ZEROS.
           05 WS-AUX1              PIC 9(02) VALUES ZEROS.
           05 QTD-GRAOS-TONE    PIC 9(13)v99 VALUES ZEROS.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 1000-INICIO
           PERFORM 2000-FIM
           STOP RUN
           .
       1000-INICIO.
           SET WS-AUX1 TO 0
           SET WS-QTD-GRAOS TO 0
           PERFORM UNTIL WS-AUX1 EQUAL 63
               DISPLAY WS-AUX1 " " WS-QTD-GRAOS
               COMPUTE WS-QTD-GRAOS = WS-QTD-GRAOS + 2**WS-AUX1
               ADD 1 TO WS-AUX1
           END-PERFORM
           COMPUTE QTD-GRAOS-TONE = (42*(WS-QTD-GRAOS/1000))/1000000
           .
       2000-FIM.

           DISPLAY "A RAINHA DEVERA ENTREGAR " WS-QTD-GRAOS
           " GRAOS, O QUE "  WITH NO ADVANCING
           DISPLAY "CORRESPONDE A " QTD-GRAOS-TONE " TONELADAS DE TRIGO"
           .
            STOP RUN.
       END PROGRAM DESAFIO09.
