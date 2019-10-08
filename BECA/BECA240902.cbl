      ******************************************************************
      * Author: JULIO CESAR
      * Date: 24/09/2019
      * Purpose: PROGRAMA TESTE DE PROGRAMAS
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BECA.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS-DATA PIC X(8).
       01 WS-DATA-DISPLAY.
           05 WS-DIA PIC X(2).
           05 FILLER PIC X VALUE '/'.
           05 WS-MES PIC X(2).
           05 FILLER PIC X VALUE '/'.
           05 WS-ANO PIC X(4).
       01 WS-ALFA PIC X VALUE 'A'.
       01 WS-GRUPO.
           05 WS-NUMERICA  PIC 9 VALUE 1.
           05 FILLER       PIC X VALUE '-'.
           05 WS-ALFA2     PIC X VALUE 'B'.
       01 WS-BOOLEANA PIC X VALUE 'S'.
           88 WS-SIM        VALUE 'S'.
           88 WS-NAO        VALUE 'N'.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           ACCEPT WS-DATA FROM DATE YYYYMMDD.

      *     MOVE WS-DATA TO WS-DATA-DISPLAY.
           MOVE WS-DATA(1:4) TO WS-ANO
           MOVE WS-DATA(5:2)TO WS-MES
           MOVE WS-DATA(7:2) TO WS-DIA
           IF WS-BOOLEANA EQUAL 'S'
               DISPLAY WS-DATA-DISPLAY
           ELSE
               DISPLAY WS-DATA
           END-IF.

            STOP RUN.
       END PROGRAM BECA.
