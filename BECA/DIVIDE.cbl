<<<<<<< HEAD
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
       01 WS-VALOR3 PIC S9(10)V99 VALUE ZEROS.
       01 WS-VALOR4 PIC 9(10)V99  VALUE ZEROS.
       01 WS-MASC   PIC +ZZZ.ZZZ.ZZ9,99.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            MOVE 10 TO WS-VALOR1.
            MOVE 2  TO WS-VALOR2.

            DIVIDE WS-VALOR1 BY WS-VALOR2 GIVING WS-VALOR3
                                   REMAINDER WS-VALOR4

            MOVE WS-VALOR3 TO WS-MASC.

            DISPLAY WS-MASC.
            DISPLAY WS-VALOR4.



            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
=======
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
       01 WS-VALOR3 PIC S9(10)V99 VALUE ZEROS.
       01 WS-VALOR4 PIC 9(10)V99  VALUE ZEROS.
       01 WS-MASC   PIC +ZZZ.ZZZ.ZZ9,99.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            MOVE 10 TO WS-VALOR1.
            MOVE 2  TO WS-VALOR2.

            DIVIDE WS-VALOR1 BY WS-VALOR2 GIVING WS-VALOR3
                                   REMAINDER WS-VALOR4

            MOVE WS-VALOR3 TO WS-MASC.

            DISPLAY WS-MASC.
            DISPLAY WS-VALOR4.



            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
>>>>>>> fc4eb318f1bf419679fc59f0577be2685a746e7a
