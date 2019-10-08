      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS-VALOR1 PIC 9(10).
       01 WS-VALOR2 PIC 9(10).
       01 WS-VALOR3 PIC S9(10) VALUE ZEROS.
       01 WS-MASC   PIC +ZZZ.ZZZ.ZZ9.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            MOVE 1 TO WS-VALOR1.
            MOVE 2 TO WS-VALOR2.

            ADD WS-VALOR1 TO WS-VALOR2 GIVING WS-VALOR3.

            MOVE WS-VALOR3 TO WS-MASC.

            DISPLAY WS-MASC.

            SUBTRACT 10 FROM WS-VALOR2 GIVING WS-VALOR3.
            MOVE WS-VALOR3 TO WS-MASC.
            DISPLAY WS-MASC.

            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
