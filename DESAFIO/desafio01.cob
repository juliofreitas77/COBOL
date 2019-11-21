      ******************************************************************
      * Author: Julio Cesar
      * Date: 11/11/2019
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       1   WS-CONSTANTES.
           5 CT-01   PIC 9(01) VALUE 1.
           5 CT-02   PIC 9(10) VALUE 10.

       1   WS-VAR.
           5 WS-INDICE    PIC 9(2).
           5 WS-Y         PIC S9(1).
           5 WS-X         COMP-2.


       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           MOVE CT-01 TO WS-INDICE
           PERFORM CT-02 TIMES
               IF FUNCTION MOD(WS-INDICE, 2)=0
                   MOVE -1 TO WS-Y
               ELSE
                   MOVE 1 TO WS-Y
               END-IF
               COMPUTE WS-X = WS-X +(WS-INDICE /(WS-INDICE **2)* WS-Y)
                    ADD 1 TO WS-INDICE
           END-PERFORM
           .

           DISPLAY 'X = ' WS-X.

            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
