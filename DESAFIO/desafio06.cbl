      *Construa um algoritmo que calcule o valor aproximado de PI
      * utilizando a fórmula (série de Nilakantha):
      * PI = 3 + 4/(2*3*4) - 4/(4*5*6) + 4/(6*7*8) - 4/(8*9*10) +
      *  4/(10*11*12) - 4/(12*13*14)
      * Obs.: solicitar ao usuário o enésimo termo para a quantidade de
      *  denominadores

       IDENTIFICATION DIVISION.
       PROGRAM-ID. DESAFIO08.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       1   WS-H        COMP-2.
       1   WS-X        PIC 9(04) COMP.
       1   WS-RM       COMP-2.
       1   WS-N        PIC 9(9).
       1   WS-F        PIC S9(01).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
       01-PROCESSAR.
           MOVE 1 TO WS-F
           MOVE 2 TO WS-X
           MOVE 3 TO WS-H

           PERFORM VARYING WS-N FROM 1 BY 1
           UNTIL WS-N > 4990
           COMPUTE WS-RM = WS-X * (WS-X + 1) * (WS-X + 2)
           COMPUTE WS-H = WS-H + (4 / WS-RM) * WS-F
           COMPUTE WS-F = WS-F * -1
           ADD 2 TO WS-X
           END-PERFORM

           DISPLAY 'PI = 'WS-H
           COMPUTE WS-H = FUNCTION PI
           DISPLAY 'PI = ' WS-H

            STOP RUN.
       END PROGRAM DESAFIO08.
